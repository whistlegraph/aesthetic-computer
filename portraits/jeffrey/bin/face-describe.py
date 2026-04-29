#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Phase 2: rich scene-graph description for face-matched jeffrey images.

Reads the JSONL produced by face-match.py, filters to is_jeffrey=true (or all
images with a face), and sends each through GPT-4o vision with reference
photos for identity grounding. Output is structured JSON aligned with the
everyday.tina.zone-inspired scene-graph schema from the rev2 feasibility doc:

  subject: description, expression, pose
  environment: location, time_of_day, lighting, background_details
  photography: camera_angle, style, framing
  + domain (categorical theme), n_other_people, is_jeffrey_confirmed,
    tags, caption_hint

This metadata is dual-purpose:
  - immediate: a curated, browsable jeffrey-by-date index with descriptions
  - downstream: training data for jeffrey's scene-graph composer
    (see vault/personal/2026-other/jeffrey-image-model-feasibility.md)

Usage:
  python face-describe.py \\
      --match-jsonl $VAULT/jeffrey-platter/curated/jeffrey-match.jsonl \\
      --output $VAULT/jeffrey-platter/curated/jeffrey-described.jsonl

Args:
  --match-jsonl <path>   input JSONL from face-match.py. Required.
  --output <path>        JSONL output (resumes by skipping already-described). Required.
  --min-similarity <f>   only describe images with at least this face-match score.
                         Default 0.5 (matches face-match's default threshold).
  --include-other-faces  also describe images with non-jeffrey faces (cross-check).
  --limit <int>          process at most N records (smoke testing). Default: all.
  --concurrency <int>    parallel API requests. Default: 4.
  --model <id>           OpenAI model. Default: gpt-4o.
"""

from __future__ import annotations

import argparse
import asyncio
import base64
import json
import os
import sys
from datetime import datetime, timezone
from pathlib import Path

import cv2
from openai import APIError, AsyncOpenAI, RateLimitError

REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
VAULT_ENV = REPO_ROOT / "aesthetic-computer-vault" / ".devcontainer" / "envs" / "devcontainer.env"
SHOOT_DIR = REPO_ROOT / "portraits" / "jeffrey" / "corpus" / "shoot"

# Two refs is enough for identity grounding; more bloats every request.
REFERENCE_PHOTOS = [
    SHOOT_DIR / "jeffery-av--01.jpg",
    SHOOT_DIR / "jeffery-av--07.jpg",
]

SYSTEM_PROMPT = """\
You are tagging a personal photograph of a specific person — referred to as \
"jeffrey" — for a structured scene-graph index. The two reference images at \
the top of the user message are confirmed photos of jeffrey from a recent \
photoshoot. Use them to verify identity. He may look different across years \
(2014–2026): different hair length/color, beard variation, glasses on/off, \
older or younger. Be flexible on age but strict on facial structure.

Return JSON with these fields:

- is_jeffrey_confirmed (boolean): is jeffrey clearly the subject (or one of \
the subjects) of this candidate image? false if you can't tell, or if the \
visible face is not jeffrey.
- confidence (0-1): your confidence in is_jeffrey_confirmed.
- subject (object — fill only if is_jeffrey_confirmed):
  - description: jeffrey's appearance in THIS photo. Hair (length, color, \
state), beard (none / stubble / short / full / specific shape), clothing \
(specific items + colors + condition), accessories (glasses, hats, jewelry). \
1-2 sentences, dense and visual.
  - expression: short phrase, e.g. "soft smile", "open-mouthed laugh", \
"focused, eyes down", "neutral, slight tension at mouth".
  - pose: what jeffrey is physically doing — "leaning into a kitchen counter, \
holding a coffee cup", "seated cross-legged drawing on a tablet", "mid-stride \
on a sidewalk, looking sideways at the camera".
- environment (object):
  - location: specific where possible — "small home studio", "Brooklyn \
sidewalk at night", "art gallery interior with white walls". Avoid generic \
"indoors".
  - time_of_day: "morning" / "afternoon" / "evening" / "night" / "unclear".
  - lighting: short phrase — "warm afternoon window light", "harsh fluorescent \
overhead", "cyan night-club uplighting", "soft ambient lamp".
  - background_details: 1-2 sentences on what's visibly behind/around jeffrey \
— furniture, objects, art on walls, signage, other people in background.
- photography (object):
  - camera_angle: "eye-level frontal", "low-angle from waist", "high-angle \
overhead", "side profile", "over-the-shoulder", etc.
  - style: short phrase — "casual phone selfie", "Gen Z photo dump", "staged \
event portrait", "candid 35mm film", "fluorescent-lit interior snapshot". \
Capture the *aesthetic*.
  - framing: "tight close-up of face", "medium shot waist-up", "wide environmental".
- domain (string): pick one of the following categories that best describes \
what jeffrey is doing or where he is. If none fits well, pick "other".
  Categories: art (drawing, painting, whistlegraph), coding, performance \
(stage, AV show, music), lecture-or-teaching, repair (fixing things), \
cooking-or-eating, solo-portrait, with-fia, social (with friends/group), \
travel-or-outdoor, studio-or-workshop, kidlisp, other.
- n_other_people (integer): count of other clearly-visible people (not \
jeffrey, not background blur).
- tags (array of strings): 3-7 freeform short tags capturing notable specifics \
— e.g. ["coffee", "kitchen", "morning", "casual"] or ["projector", "stage", \
"audience", "blue lighting"].
- caption_hint (string or null): 1 short sentence in the voice of an IG \
caption that would fit this photo. Optional — null if nothing comes to mind.

Output JSON ONLY. If is_jeffrey_confirmed is false, set subject to null and \
keep environment/photography/domain/n_other_people/tags filled in for the \
image as it stands.\
"""

OUTPUT_SCHEMA = {
    "type": "object",
    "additionalProperties": False,
    "properties": {
        "is_jeffrey_confirmed": {"type": "boolean"},
        "confidence": {"type": "number"},
        "subject": {
            "type": ["object", "null"],
            "additionalProperties": False,
            "properties": {
                "description": {"type": "string"},
                "expression": {"type": "string"},
                "pose": {"type": "string"},
            },
            "required": ["description", "expression", "pose"],
        },
        "environment": {
            "type": "object",
            "additionalProperties": False,
            "properties": {
                "location": {"type": "string"},
                "time_of_day": {"type": "string"},
                "lighting": {"type": "string"},
                "background_details": {"type": "string"},
            },
            "required": ["location", "time_of_day", "lighting", "background_details"],
        },
        "photography": {
            "type": "object",
            "additionalProperties": False,
            "properties": {
                "camera_angle": {"type": "string"},
                "style": {"type": "string"},
                "framing": {"type": "string"},
            },
            "required": ["camera_angle", "style", "framing"],
        },
        "domain": {"type": "string"},
        "n_other_people": {"type": "integer"},
        "tags": {"type": "array", "items": {"type": "string"}},
        "caption_hint": {"type": ["string", "null"]},
    },
    "required": [
        "is_jeffrey_confirmed", "confidence", "subject", "environment",
        "photography", "domain", "n_other_people", "tags", "caption_hint",
    ],
}


def load_openai_key() -> str:
    if "OPENAI_API_KEY" in os.environ:
        return os.environ["OPENAI_API_KEY"]
    if VAULT_ENV.exists():
        for line in VAULT_ENV.read_text().splitlines():
            if line.startswith("OPENAI_API_KEY="):
                return line.split("=", 1)[1].strip().strip('"').strip("'")
    sys.exit("OPENAI_API_KEY not set and not in vault devcontainer.env")


def encode_image(path: Path, max_dim: int = 1024) -> str:
    img = cv2.imread(str(path))
    if img is None:
        raise ValueError(f"could not read {path}")
    h, w = img.shape[:2]
    if max(h, w) > max_dim:
        scale = max_dim / max(h, w)
        img = cv2.resize(img, (int(w * scale), int(h * scale)))
    ok, buf = cv2.imencode(".jpg", img, [cv2.IMWRITE_JPEG_QUALITY, 88])
    if not ok:
        raise ValueError(f"jpeg encode failed for {path}")
    return base64.standard_b64encode(buf.tobytes()).decode("ascii")


def build_user_content(image_b64: str, ref_b64s: list[str]) -> list[dict]:
    content = [{"type": "text", "text": "REFERENCE photos of jeffrey (not the candidate):"}]
    for ref_b64 in ref_b64s:
        content.append({
            "type": "image_url",
            "image_url": {"url": f"data:image/jpeg;base64,{ref_b64}", "detail": "low"},
        })
    content.append({"type": "text", "text": "CANDIDATE image — tag this:"})
    content.append({
        "type": "image_url",
        "image_url": {"url": f"data:image/jpeg;base64,{image_b64}", "detail": "high"},
    })
    return content


async def describe_one(
    client: AsyncOpenAI,
    ref_b64s: list[str],
    record: dict,
    model: str,
    sem: asyncio.Semaphore,
) -> dict:
    out: dict = {
        "path": record["path"],
        "rel_path": record["rel_path"],
        "date": record.get("date"),
        "shortcode": record.get("shortcode"),
        "match_similarity": record.get("best_similarity"),
        "match_ref": record.get("best_match_ref"),
        "described_at": datetime.now(timezone.utc).isoformat(),
        "model": model,
        "error": None,
    }
    try:
        image_b64 = encode_image(Path(record["path"]))
    except Exception as e:
        out["error"] = f"encode: {type(e).__name__}: {e}"
        return out

    user_content = build_user_content(image_b64, ref_b64s)

    async with sem:
        for attempt in range(3):
            try:
                response = await client.chat.completions.create(
                    model=model,
                    messages=[
                        {"role": "system", "content": SYSTEM_PROMPT},
                        {"role": "user", "content": user_content},
                    ],
                    response_format={
                        "type": "json_schema",
                        "json_schema": {
                            "name": "scene_graph_tag",
                            "strict": True,
                            "schema": OUTPUT_SCHEMA,
                        },
                    },
                    max_tokens=900,
                )
                parsed = json.loads(response.choices[0].message.content or "{}")
                out.update(parsed)
                out["_usage"] = {
                    "prompt_tokens": response.usage.prompt_tokens,
                    "completion_tokens": response.usage.completion_tokens,
                    "cached_tokens": (
                        getattr(response.usage, "prompt_tokens_details", None)
                        and response.usage.prompt_tokens_details.cached_tokens
                    ) or 0,
                }
                return out
            except RateLimitError:
                await asyncio.sleep(2 ** attempt + 1)
            except APIError as e:
                out["error"] = f"api: {type(e).__name__}: {e}"
                if attempt < 2:
                    await asyncio.sleep(2 ** attempt)
                    continue
                return out
            except Exception as e:
                out["error"] = f"{type(e).__name__}: {e}"
                return out
    return out


async def main_async(args: argparse.Namespace) -> int:
    api_key = load_openai_key()
    client = AsyncOpenAI(api_key=api_key)

    # Load matches
    match_path = Path(args.match_jsonl).expanduser().resolve()
    if not match_path.exists():
        sys.exit(f"match jsonl not found: {match_path}")
    matches: list[dict] = []
    for line in match_path.read_text().splitlines():
        try:
            r = json.loads(line)
        except json.JSONDecodeError:
            continue
        if r.get("error"):
            continue
        sim = r.get("best_similarity", 0)
        if r.get("is_jeffrey") or (args.include_other_faces and r.get("n_faces", 0) > 0):
            if sim >= args.min_similarity:
                matches.append(r)
    matches.sort(key=lambda r: r.get("best_similarity", 0), reverse=True)
    print(f"matched candidates: {len(matches)}", file=sys.stderr)

    # Resume
    out_path = Path(args.output).expanduser().resolve()
    out_path.parent.mkdir(parents=True, exist_ok=True)
    already: set[str] = set()
    if out_path.exists():
        for line in out_path.read_text().splitlines():
            try:
                already.add(json.loads(line)["path"])
            except (json.JSONDecodeError, KeyError):
                pass
    matches = [r for r in matches if r["path"] not in already]
    if args.limit:
        matches = matches[: args.limit]
    print(f"to describe: {len(matches)} ({len(already)} already done)", file=sys.stderr)
    if not matches:
        return 0

    # Load reference photos
    ref_b64s = []
    for ref in REFERENCE_PHOTOS:
        if not ref.exists():
            sys.exit(f"reference photo missing: {ref}")
        ref_b64s.append(encode_image(ref, max_dim=512))
    print(f"loaded {len(ref_b64s)} reference photos", file=sys.stderr)

    sem = asyncio.Semaphore(args.concurrency)
    totals = {"prompt": 0, "completion": 0, "cached": 0, "confirmed": 0, "rejected": 0, "errors": 0}

    with out_path.open("a") as f:
        tasks = [describe_one(client, ref_b64s, r, args.model, sem) for r in matches]
        for i, coro in enumerate(asyncio.as_completed(tasks), 1):
            record = await coro
            usage = record.pop("_usage", None)
            f.write(json.dumps(record) + "\n")
            f.flush()
            if usage:
                totals["prompt"] += usage["prompt_tokens"]
                totals["completion"] += usage["completion_tokens"]
                totals["cached"] += usage["cached_tokens"]
            if record.get("error"):
                totals["errors"] += 1
                tag = "✗"
            elif record.get("is_jeffrey_confirmed"):
                totals["confirmed"] += 1
                tag = "✓"
            else:
                totals["rejected"] += 1
                tag = "·"
            sim = record.get("match_similarity") or 0
            err = f" ERR={record['error'][:60]}" if record.get("error") else ""
            domain = record.get("domain", "—")
            print(
                f"[{i}/{len(matches)}] {tag} sim={sim:.3f} {domain:<22} {record['rel_path'][:35]}{err}",
                file=sys.stderr,
            )

    cost_in = totals["prompt"] * 2.50e-6
    cost_out = totals["completion"] * 10e-6
    cache_savings = totals["cached"] * 1.25e-6  # 50% off cached input tokens
    print(
        f"\ndone. confirmed={totals['confirmed']} rejected={totals['rejected']} errors={totals['errors']}",
        file=sys.stderr,
    )
    print(
        f"tokens: prompt={totals['prompt']} completion={totals['completion']} cached={totals['cached']}",
        file=sys.stderr,
    )
    print(
        f"approx cost: ${cost_in + cost_out:.2f} (${cost_in:.2f} in + ${cost_out:.2f} out, "
        f"~${cache_savings:.2f} saved by cache)",
        file=sys.stderr,
    )
    return 0


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--match-jsonl", required=True)
    p.add_argument("--output", required=True)
    p.add_argument("--min-similarity", type=float, default=0.5)
    p.add_argument("--include-other-faces", action="store_true")
    p.add_argument("--limit", type=int, default=0)
    p.add_argument("--concurrency", type=int, default=4)
    p.add_argument("--model", default="gpt-4o")
    args = p.parse_args()
    return asyncio.run(main_async(args))


if __name__ == "__main__":
    sys.exit(main())
