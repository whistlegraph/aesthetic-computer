#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Walk a directory of images, identify which contain jeffrey, and tag each with
expression / framing / description metadata via Claude Opus 4.7 vision.

Pipeline per image:
  1. opencv Haar cascade — does this image have any face? (~5ms, free)
     - If no face: emit a face=false record and skip the API call.
  2. Claude Opus 4.7 vision — given reference photos of jeffrey (cached) +
     this candidate, return structured JSON: is_jeffrey, confidence, expression,
     framing, description, n_other_people.
  3. Append the record to a JSONL output file.

Reference photos live in the system prompt with cache_control, so subsequent
requests within a 5-minute window pay ~0.1× for the reference tokens. With
a sustained run, cache reads dominate cost.

Usage:
  IG_ARCHIVE=$VAULT/jeffrey-platter/ig-archive/whistlegraph
  python face-tag.py --input "$IG_ARCHIVE/*.jpg" \\
      --output ~/jeffreys-tagged.jsonl

Args:
  --input <glob>       glob of input image paths (jpg/png/webp). Required.
  --output <path>      JSONL output file. Resumes by skipping paths already in
                       this file. Required.
  --limit <int>        process at most N images (smoke testing). Default: all.
  --model <id>         Claude model. Default: claude-opus-4-7. Override with
                       claude-sonnet-4-6 to cut cost ~5×, claude-haiku-4-5 to
                       cut ~20× (lower fidelity).
  --skip-no-face       if set, omit no-face records from output (default: keep).
  --concurrency <int>  parallel API requests. Default: 5. Higher → faster but
                       more rate-limit pressure.

Output JSONL record (one per line):
  {
    "path": "/abs/path/to/image.jpg",
    "rel_path": "image.jpg",
    "has_face": true,
    "is_jeffrey": true,
    "confidence": 0.92,
    "expression": "soft smile",
    "framing": "selfie",
    "description": "jeffrey holding a coffee in a kitchen",
    "n_other_people": 0,
    "model": "claude-opus-4-7",
    "tagged_at": "2026-04-29T...",
    "error": null
  }
"""

from __future__ import annotations

import argparse
import asyncio
import base64
import json
import os
import sys
from datetime import datetime, timezone
from glob import glob
from pathlib import Path

import cv2
from anthropic import APIError, AsyncAnthropic, RateLimitError

REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
SHOOT_DIR = REPO_ROOT / "portraits" / "jeffrey" / "corpus" / "shoot"
VAULT_ENV = REPO_ROOT / "aesthetic-computer-vault" / "lith" / ".env"

# Reference photos — chosen to span lighting/angle without being too many.
# All from the AV shoot, master tier (--01..--10 are highest quality).
# Keeping it tight (3 refs) for cache size. Add more if accuracy is poor.
REFERENCE_PHOTOS = [
    SHOOT_DIR / "jeffery-av--01.jpg",
    SHOOT_DIR / "jeffery-av--04.jpg",
    SHOOT_DIR / "jeffery-av--07.jpg",
]

SYSTEM_INSTRUCTIONS = """\
You are identifying whether a specific person — referred to as "jeffrey" — \
appears in a photograph, and tagging the photo with metadata.

The reference images that follow are confirmed photos of jeffrey at the time \
of the AV photoshoot. Use them to recognize his face structure, hair, beard if \
present, and overall appearance. Account for: aging (photos may be from any \
year between 2014 and 2026), changes in hair length/color, beard variation, \
glasses on/off, lighting, and angle.

For each candidate image you receive, return JSON with these fields:
- is_jeffrey: boolean — is jeffrey visible in this image (any portion of him, \
even partial face, even from behind if recognizable)?
- confidence: number from 0 to 1 — your confidence in is_jeffrey. Use ≥0.8 \
when the face is clear and matches; 0.5-0.8 when partial or angled; <0.5 when \
uncertain. If is_jeffrey is false, this is your confidence he is NOT present.
- expression: short phrase describing facial expression (e.g. "soft smile", \
"laughing", "neutral focus", "serious", "open mouth singing"), or null if no \
face is visible or jeffrey is not present.
- framing: one of "selfie" (jeffrey only, arm-distance), "portrait" \
(staged/composed shot of jeffrey), "candid" (unposed jeffrey), "group" \
(jeffrey with others), "background" (jeffrey not the subject), "no_jeffrey".
- description: 1-2 sentence factual description of what's in the image — \
setting, activity, what's visible. Include jeffrey's role if present.
- n_other_people: integer count of other people visible (excluding jeffrey).

Be honest about uncertainty. False positives (tagging non-jeffrey as jeffrey) \
are worse than false negatives. If the face is too small/blurry/angled to tell, \
set is_jeffrey=false with low confidence and a description noting the ambiguity.\
"""

OUTPUT_SCHEMA = {
    "type": "object",
    "properties": {
        "is_jeffrey": {"type": "boolean"},
        "confidence": {"type": "number"},
        "expression": {"type": ["string", "null"]},
        "framing": {
            "type": "string",
            "enum": ["selfie", "portrait", "candid", "group", "background", "no_jeffrey"],
        },
        "description": {"type": "string"},
        "n_other_people": {"type": "integer"},
    },
    "required": ["is_jeffrey", "confidence", "framing", "description", "n_other_people"],
    "additionalProperties": False,
}


def load_anthropic_key() -> str:
    if "ANTHROPIC_API_KEY" in os.environ:
        return os.environ["ANTHROPIC_API_KEY"]
    if VAULT_ENV.exists():
        for line in VAULT_ENV.read_text().splitlines():
            if line.startswith("ANTHROPIC_API_KEY="):
                return line.split("=", 1)[1].strip().strip('"').strip("'")
    sys.exit("ANTHROPIC_API_KEY not set and not found in vault/lith/.env")


_face_cascade = None


def has_face(image_path: Path) -> bool:
    """Cheap local check: does this image plausibly contain at least one face?"""
    global _face_cascade
    if _face_cascade is None:
        cascade_path = cv2.data.haarcascades + "haarcascade_frontalface_default.xml"
        _face_cascade = cv2.CascadeClassifier(cascade_path)
    img = cv2.imread(str(image_path))
    if img is None:
        return False
    gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    faces = _face_cascade.detectMultiScale(gray, scaleFactor=1.2, minNeighbors=4, minSize=(40, 40))
    return len(faces) > 0


def encode_image(path: Path, max_dim: int = 1024) -> tuple[str, str]:
    """Return (base64_data, media_type) for an image, downsampled to max_dim."""
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
    return base64.standard_b64encode(buf.tobytes()).decode("ascii"), "image/jpeg"


def build_system_blocks(reference_paths: list[Path]) -> list[dict]:
    """System prompt: text instructions + reference jeffrey photos. Cached."""
    blocks: list[dict] = [{"type": "text", "text": SYSTEM_INSTRUCTIONS}]
    for ref in reference_paths:
        if not ref.exists():
            sys.exit(f"reference photo missing: {ref} — run fetch-corpus.mjs --shoot first")
        b64, mt = encode_image(ref, max_dim=768)
        blocks.append({
            "type": "image",
            "source": {"type": "base64", "media_type": mt, "data": b64},
        })
    # cache_control on the LAST block of the stable prefix caches everything before it
    blocks[-1]["cache_control"] = {"type": "ephemeral"}
    return blocks


async def tag_image(
    client: AsyncAnthropic,
    system_blocks: list[dict],
    image_path: Path,
    model: str,
) -> dict:
    """Send one image to Claude vision; return parsed result dict."""
    b64, mt = encode_image(image_path)
    response = await client.messages.create(
        model=model,
        max_tokens=400,
        system=system_blocks,
        messages=[{
            "role": "user",
            "content": [
                {"type": "image", "source": {"type": "base64", "media_type": mt, "data": b64}},
                {"type": "text", "text": "Tag this photo per the schema."},
            ],
        }],
        output_config={"format": {"type": "json_schema", "schema": OUTPUT_SCHEMA}},
    )
    text = next((b.text for b in response.content if b.type == "text"), "")
    parsed = json.loads(text)
    parsed["_usage"] = {
        "input_tokens": response.usage.input_tokens,
        "output_tokens": response.usage.output_tokens,
        "cache_creation_input_tokens": response.usage.cache_creation_input_tokens or 0,
        "cache_read_input_tokens": response.usage.cache_read_input_tokens or 0,
    }
    return parsed


async def process_one(
    client: AsyncAnthropic,
    system_blocks: list[dict],
    image_path: Path,
    model: str,
    sem: asyncio.Semaphore,
) -> dict:
    """Per-image pipeline: face detect → (maybe) Claude → record."""
    record: dict = {
        "path": str(image_path.resolve()),
        "rel_path": image_path.name,
        "tagged_at": datetime.now(timezone.utc).isoformat(),
        "model": model,
        "error": None,
    }
    try:
        record["has_face"] = has_face(image_path)
    except Exception as e:
        record["has_face"] = False
        record["error"] = f"face_detect: {e}"
        return record

    if not record["has_face"]:
        record["is_jeffrey"] = False
        record["confidence"] = 0.0
        record["expression"] = None
        record["framing"] = "no_jeffrey"
        record["description"] = "no face detected"
        record["n_other_people"] = 0
        return record

    async with sem:
        for attempt in range(3):
            try:
                tagged = await tag_image(client, system_blocks, image_path, model)
                record.update(tagged)
                return record
            except RateLimitError:
                await asyncio.sleep(2 ** attempt + 1)
            except APIError as e:
                record["error"] = f"api: {type(e).__name__}: {e}"
                if attempt < 2:
                    await asyncio.sleep(2 ** attempt)
                    continue
                return record
            except Exception as e:
                record["error"] = f"{type(e).__name__}: {e}"
                return record
    return record


async def main_async(args: argparse.Namespace) -> int:
    api_key = load_anthropic_key()
    client = AsyncAnthropic(api_key=api_key)

    paths = sorted(Path(p) for p in glob(args.input))
    if not paths:
        sys.exit(f"no inputs matched: {args.input}")

    out_path = Path(args.output).expanduser().resolve()
    out_path.parent.mkdir(parents=True, exist_ok=True)
    already_tagged: set[str] = set()
    if out_path.exists():
        for line in out_path.read_text().splitlines():
            try:
                already_tagged.add(json.loads(line)["path"])
            except (json.JSONDecodeError, KeyError):
                pass
    paths = [p for p in paths if str(p.resolve()) not in already_tagged]
    if args.limit:
        paths = paths[: args.limit]

    print(
        f"to process: {len(paths)} ({len(already_tagged)} already tagged in output)",
        file=sys.stderr,
    )
    if not paths:
        return 0

    system_blocks = build_system_blocks(REFERENCE_PHOTOS)
    sem = asyncio.Semaphore(args.concurrency)

    totals = {"input": 0, "output": 0, "cache_create": 0, "cache_read": 0, "jeffrey": 0, "no_face": 0}
    with out_path.open("a") as f:
        tasks = [process_one(client, system_blocks, p, args.model, sem) for p in paths]
        for i, coro in enumerate(asyncio.as_completed(tasks), 1):
            record = await coro
            if args.skip_no_face and not record.get("has_face"):
                pass
            else:
                f.write(json.dumps(record) + "\n")
                f.flush()
            usage = record.pop("_usage", None)
            if usage:
                totals["input"] += usage["input_tokens"]
                totals["output"] += usage["output_tokens"]
                totals["cache_create"] += usage["cache_creation_input_tokens"]
                totals["cache_read"] += usage["cache_read_input_tokens"]
            if record.get("is_jeffrey"):
                totals["jeffrey"] += 1
            if not record.get("has_face"):
                totals["no_face"] += 1
            tag = "✓" if record.get("is_jeffrey") else (" " if record.get("has_face") else "·")
            err = f" ERR={record['error']}" if record.get("error") else ""
            conf = record.get("confidence", 0)
            print(
                f"[{i}/{len(paths)}] {tag} {record['rel_path']} c={conf:.2f}{err}",
                file=sys.stderr,
            )

    print(
        f"\ndone. {totals['jeffrey']} jeffrey / {totals['no_face']} no-face / "
        f"{len(paths) - totals['no_face'] - totals['jeffrey']} other-faces",
        file=sys.stderr,
    )
    print(
        f"tokens: input={totals['input']} output={totals['output']} "
        f"cache_create={totals['cache_create']} cache_read={totals['cache_read']}",
        file=sys.stderr,
    )
    return 0


def main() -> int:
    p = argparse.ArgumentParser(description="Face-tag images via Claude vision.")
    p.add_argument("--input", required=True, help="glob of input image paths")
    p.add_argument("--output", required=True, help="JSONL output path")
    p.add_argument("--limit", type=int, default=0)
    p.add_argument("--model", default="claude-opus-4-7")
    p.add_argument("--skip-no-face", action="store_true")
    p.add_argument("--concurrency", type=int, default=5)
    args = p.parse_args()
    return asyncio.run(main_async(args))


if __name__ == "__main__":
    sys.exit(main())
