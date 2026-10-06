#!/usr/bin/env python3
"""Bounded No Paint image-state experiment. Network runs require --live.

python3 run.py prepare
python3 run.py pilot --live
python3 run.py chain --model klein --start base --turns 50 --live
python3 run.py report
"""
import argparse
import base64
import datetime
import hashlib
import html
import importlib.util
import io
import json
from pathlib import Path
import statistics
import time
import urllib.error

import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
spec = importlib.util.spec_from_file_location(
    "provider_image", HERE.parent / "image-model-turntables-2026-09-23/provider_image.py")
provider = importlib.util.module_from_spec(spec)
spec.loader.exec_module(provider)

MODELS = {
    "klein": {"endpoint": "fal-ai/flux-2/klein/4b/edit", "steps": 4},
    "kontext": {"endpoint": "fal-ai/flux-kontext/dev", "steps": 28},
}
MOVE = (
    "Continue this exact image by making exactly one small, deliberate painting move. "
    "Choose the move yourself in response to the existing composition: add one simple mark, "
    "extend one existing shape, or change one small region of color. "
    "Keep the change within a single area occupying at most one tenth of the canvas. "
    "Preserve every other part of the input image, including its colors, texture, softness, "
    "edges, empty areas, and framing. If the image is blank or noise, add just one small mark "
    "and preserve the rest. Return only the complete updated image. "
    "Do not finish the painting, add a scene, enhance detail, sharpen, relight, add lettering, "
    "or change the overall style."
)
SPECIFIED = (
    "Add exactly one small solid magenta circle to this exact image. "
    "Its center must be at 25 percent of the image width from the left and "
    "75 percent of the image height from the top. Its diameter must be 8 percent "
    "of the image width. Use flat color #ff00ff with no outline or shadow. "
    "Preserve all pixels outside this small circle, including all existing colors, "
    "texture, softness, composition, and framing. Return the complete updated image."
)

def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2) + "\n")

def digest(data):
    return hashlib.sha256(data).hexdigest()

def prepare():
    starts = HERE / "starts"
    starts.mkdir(exist_ok=True)
    Image.new("RGB", (256, 256), "white").save(starts / "blank.png")
    rng = np.random.default_rng(20261005)
    noise = rng.integers(0, 256, size=(256, 256), dtype=np.uint8)
    Image.fromarray(noise).convert("RGB").save(starts / "noise.png")
    source = ROOT / "papers/nopaint-3-full-shape/assets/archive-l4f0ipzy.png"
    # The original archive file is a 256px square painting plus a 32px label footer.
    Image.open(source).convert("RGB").crop((0, 0, 256, 256)).save(starts / "base.png")
    write_json(HERE / "plan.json", {
        "canvas": [256, 256], "provider": "fal", "models": MODELS,
        "starts": {"blank": "white RGB", "noise": "grayscale uniform noise, numpy seed 20261005",
                   "base": {"source": str(source.relative_to(ROOT)), "crop": [0, 0, 256, 256],
                            "public_record": "https://nopaint.art/l4f0ipzy"}},
        "prompts": {"autonomous": MOVE, "specified": SPECIFIED},
        "protocol": "Every request receives a complete current PNG; no conversation history. "
                    "Autonomous means the model selects the move under a fixed instruction, not prompt-free. "
                    "Chain mode automatically adopts outputs for a stress test; no public painting is changed.",
        "postprocess": "Convert RGB and resize to canonical 256x256 with LANCZOS only when needed. "
                       "Keep native provider output. Never mask or composite outputs to hide drift.",
    })
    print("Prepared blank, noise, and archived painting at 256x256.")

def metrics(before, after, specified=False):
    a = np.asarray(Image.open(before).convert("RGB"), dtype=np.int16)
    b = np.asarray(Image.open(after).convert("RGB"), dtype=np.int16)
    delta = np.abs(a - b)
    d = delta.max(axis=2)
    result = {"mean_channel_delta_255": float(delta.mean()),
              "changed_fraction_gt_8": float((d > 8).mean()),
              "changed_fraction_gt_24": float((d > 24).mean()),
              "identical_fraction": float((d == 0).mean())}
    if specified:
        y, x = np.ogrid[:256, :256]
        outside = ((x - 64) ** 2 + (y - 192) ** 2) > 16 ** 2
        result["outside_target_changed_fraction_gt_8"] = float((d[outside] > 8).mean())
    return result

def generate(client, key, model, start, before, step, kind="autonomous", inference_size=256):
    label = model if inference_size == 256 else f"{model}-{inference_size}"
    folder = HERE / "runs" / label / start / kind
    folder.mkdir(parents=True, exist_ok=True)
    receipt = folder / f"{step:03}.json"
    canonical = folder / f"{step:03}.png"
    raw = before.read_bytes()
    prompt = SPECIFIED if kind == "specified" else MOVE
    seed = 20261005 + step * 17
    identity = {"model": MODELS[model]["endpoint"], "input_sha256": digest(raw),
                "prompt_sha256": digest(prompt.encode()), "seed": seed,
                "inference_size": inference_size}
    if receipt.exists():
        prior = json.loads(receipt.read_text())
        if prior.get("identity") != identity:
            raise ValueError(f"Existing experiment differs: {receipt}; choose a fresh directory")
        if prior.get("status") == "complete" and canonical.exists():
            return canonical
        raise ValueError(f"Prior incomplete/failed request at {receipt}; inspect before resubmitting")
    sent = raw
    if inference_size != 256:
        buf = io.BytesIO()
        Image.open(io.BytesIO(raw)).resize((inference_size, inference_size), Image.Resampling.NEAREST).save(buf, format="PNG")
        sent = buf.getvalue()
    data_uri = "data:image/png;base64," + base64.b64encode(sent).decode()
    payload = {"prompt": prompt, "seed": seed, "num_inference_steps": MODELS[model]["steps"],
               "num_images": 1, "output_format": "png", "sync_mode": True,
               "enable_safety_checker": True}
    if model == "klein":
        payload.update(image_urls=[data_uri], image_size={"width": inference_size, "height": inference_size})
    else:
        payload.update(image_url=data_uri, resolution_mode="match_input", guidance_scale=2.5,
                       acceleration="none")
    row = {"identity": identity, "provider": "fal", "model": MODELS[model]["endpoint"],
           "start": start, "kind": kind, "step": step,
           "input": str(before.relative_to(HERE)), "sent_size": [inference_size, inference_size],
           "prompt": prompt, "parameters": {k: v for k, v in payload.items() if k not in ("image_url", "image_urls", "prompt")},
           "created_at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
           "status": "submitted", "api_network": {}}
    write_json(receipt, row)
    t = time.perf_counter()
    try:
        response, headers = provider.request(
            client, "https://fal.run/" + MODELS[model]["endpoint"], 180,
            json.dumps(payload).encode(),
            {"Authorization": "Key " + key, "Content-Type": "application/json"}, row["api_network"])
        row["api_seconds"] = time.perf_counter() - t
        result = json.loads(response)
        if result.get("error"):
            raise ValueError("Provider returned an error")
        if any(result.get("has_nsfw_concepts") or []):
            raise ValueError("Provider safety filter flagged output; not treated as a model move")
        item = result["images"][0]
        url = item["url"]
        if url.startswith("data:image/"):
            image_bytes = base64.b64decode(url.split(",", 1)[1], validate=True)
        elif url.startswith("https://"):
            image_bytes, _ = provider.request(client, url, 60)
        else:
            raise ValueError("Unexpected image result URL")
        image = Image.open(io.BytesIO(image_bytes)).convert("RGB")
        native = folder / f"{step:03}-native.png"
        image.save(native)
        row["returned_size"] = list(image.size)
        image.resize((256, 256), Image.Resampling.LANCZOS).save(canonical)
        row.update(status="complete", total_seconds=time.perf_counter() - t,
                   output=str(canonical.relative_to(HERE)), native_output=str(native.relative_to(HERE)),
                   output_sha256=digest(canonical.read_bytes()),
                   provider_timings=result.get("timings"), returned_seed=result.get("seed"),
                   request_id=result.get("request_id") or headers.get("x-fal-request-id"),
                   metrics=metrics(before, canonical, kind == "specified"))
        write_json(receipt, row)
        print(json.dumps({"model": label, "start": start, "kind": kind, "step": step,
                          "seconds": round(row["total_seconds"], 3), "size": row["returned_size"],
                          "changed_pct": round(row["metrics"]["changed_fraction_gt_8"] * 100, 1)}), flush=True)
        return canonical
    except Exception as error:
        row.update(status="failed", failed_after_seconds=time.perf_counter() - t,
                   error=(f"HTTP {error.code}" if isinstance(error, urllib.error.HTTPError) else str(error)[:200]))
        write_json(receipt, row)
        print(json.dumps({"model": label, "start": start, "step": step, "error": row["error"]}), flush=True)
        raise

def report():
    rows = [json.loads(p.read_text()) for p in sorted((HERE / "runs").glob("*/*/*/*.json"))]
    completed = [r for r in rows if r["status"] == "complete"]
    write_json(HERE / "results.json", rows)
    summary = {}
    for model in sorted({r["model"] for r in completed}):
        rr = [r for r in completed if r["model"] == model]
        ss = [r["total_seconds"] for r in rr]
        summary[model] = {"completed": len(rr), "median_seconds": statistics.median(ss),
                          "min_seconds": min(ss), "max_seconds": max(ss),
                          "returned_sizes": sorted({str(r["returned_size"]) for r in rr})}
    write_json(HERE / "summary.json", summary)
    font_path = "/System/Library/Fonts/Supplemental/Arial.ttf"
    font = ImageFont.truetype(font_path, 21)
    starts_panel = Image.new("RGB", (816, 308), "#eee9df")
    starts_draw = ImageDraw.Draw(starts_panel)
    for c, start in enumerate(["blank", "noise", "base"]):
        starts_draw.text((8+c*272, 10), start.capitalize(), fill="#151515", font=font)
        starts_panel.paste(Image.open(HERE / "starts" / f"{start}.png").convert("RGB"), (8+c*272, 44))
    starts_panel.save(HERE / "starts.png")
    panel = Image.new("RGB", (816, 984), "#eee9df")
    draw = ImageDraw.Draw(panel)
    for c, label in enumerate(["Start", "Klein 4B", "Kontext-dev"]):
        draw.text((16+c*272, 12), label, fill="#151515", font=font)
    for r, start in enumerate(["blank", "noise", "base"]):
        y = 56+r*306
        for c, model in enumerate([None, "klein", "kontext"]):
            p = HERE / "starts" / f"{start}.png" if model is None else HERE / "runs" / model / start / "autonomous/001.png"
            if p.exists():
                panel.paste(Image.open(p).convert("RGB"), (8+c*272, y))
            else:
                draw.text((20+c*272, y+110), "Not generated", fill="#595550", font=font)
        draw.text((8, y+263), start.capitalize(), fill="#151515", font=font)
    panel.save(HERE / "pilot.png")
    # The report is an offline visualization; controls never make API calls.
    data = json.dumps(rows).replace("</", "<\\/")
    page = '''<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>No Paint model moves</title>
<style>body{margin:24px;background:#eee9df;color:#171717;font:16px system-ui}h1{font-size:26px}select,button,input{font:inherit}nav{display:flex;gap:12px;flex-wrap:wrap;align-items:center;margin:24px 0}.pair{display:flex;gap:24px;flex-wrap:wrap}figure{margin:0}img{width:min(40vw,512px);min-width:256px;image-rendering:pixelated;background:white}figcaption{margin:8px 0}#status{margin:20px 0}details{max-width:1000px}pre{white-space:pre-wrap}a{color:inherit}</style>
<h1>No Paint model moves</h1><p>Full image in → one proposed move → full image out. Fixed instruction; no conversation history.</p>
<nav><label>Model <select id="model"></select></label><label>Start <select id="start"><option>blank</option><option>noise</option><option>base</option></select></label><label>Test <select id="kind"><option value="autonomous">Choose a move</option><option value="specified">Specified circle</option></select></label><button id="prev">←</button><input aria-label="Move" id="turn" type="range" min="1" max="1" value="1"><button id="next">→</button><span id="count"></span></nav>
<div class="pair"><figure><img id="before" alt="Complete input image"><figcaption>Before</figcaption></figure><figure><img id="after" alt="Complete model output"><figcaption>After</figcaption></figure></div><p id="status"></p><details><summary>Prompt and measurement</summary><pre id="detail"></pre></details><p>Changed pixels exceed 8/255 in at least one RGB channel. This measures change, not artistic quality. Chained outputs were automatically adopted for this experiment.</p>
<script>const rows=DATA;const $=id=>document.getElementById(id);let active=[];
const models=[...new Set(rows.map(r=>r.model))];models.forEach(m=>{let o=document.createElement('option');o.value=m;o.textContent=m;$('model').append(o)});
function choose(){active=rows.filter(r=>r.model===$('model').value&&r.start===$('start').value&&r.kind===$('kind').value).sort((a,b)=>a.step-b.step);$('turn').max=Math.max(1,active.length);$('turn').value=1;show()}
function show(){const r=active[+$('turn').value-1];$('before').src=r?.input||'starts/'+$('start').value+'.png';$('after').style.visibility=r?.status==='complete'?'visible':'hidden';if(!r){$('status').textContent='Not generated.';$('count').textContent='';$('detail').textContent='';return} $('count').textContent='Move '+r.step;$('detail').textContent=JSON.stringify(r,null,2);if(r.status!=='complete'){$('status').textContent='Request failed: '+r.error+'. No model image returned.';return} $('after').src=r.output;$('status').textContent=r.total_seconds.toFixed(2)+' s round trip · '+r.returned_size.join(' × ')+' returned · '+(r.metrics.changed_fraction_gt_8*100).toFixed(1)+'% pixels changed'}
['model','start','kind'].forEach(id=>$(id).onchange=choose);$('turn').oninput=show;$('prev').onclick=()=>{$('turn').value=Math.max(1,+$('turn').value-1);show()};$('next').onclick=()=>{$('turn').value=Math.min(active.length,+$('turn').value+1);show()};choose();</script></html>'''.replace("DATA", data)
    (HERE / "index.html").write_text(page)
    print(json.dumps(summary, indent=2))

def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("command", choices=["prepare", "pilot", "specified", "chain", "report"])
    p.add_argument("--live", action="store_true")
    p.add_argument("--model", choices=list(MODELS))
    p.add_argument("--start", choices=["blank", "noise", "base"], default="base")
    p.add_argument("--turns", type=int, default=5)
    p.add_argument("--size", type=int, choices=[256, 512, 1024], default=256)
    args = p.parse_args()
    if args.command == "prepare": return prepare()
    if args.command == "report": return report()
    if not args.live: raise ValueError("Network generation requires --live")
    if not 1 <= args.turns <= 50: raise ValueError("Use 1–50 turns per explicit run")
    import httpx
    key = provider.credential("FAL_KEY", str(ROOT / "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env"))
    with httpx.Client(http2=True, follow_redirects=False, limits=httpx.Limits(keepalive_expiry=120)) as client:
        for model in ([args.model] if args.model else MODELS):
            starts = ["blank", "noise", "base"] if args.command == "pilot" else [args.start]
            for start in starts:
                before = HERE / "starts" / f"{start}.png"
                count = args.turns if args.command == "chain" else 1
                kind = "specified" if args.command == "specified" else "autonomous"
                for step in range(1, count+1):
                    before = generate(client, key, model, start, before, step, kind, args.size)
    report()

if __name__ == "__main__":
    main()
