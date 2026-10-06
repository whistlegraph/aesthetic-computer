#!/usr/bin/env python3
"""Local, prompt-free SD-Turbo image-state experiment. No inference API calls.

Run with the dependencies pinned in local-requirements.txt, then:
  python local_run.py --download
  python local_run.py pilot
  python local_run.py chain --turns 5
  python local_run.py report
"""
import argparse
from contextlib import nullcontext
import datetime
import gc
import hashlib
import json
import os
from pathlib import Path
import statistics
import time

os.environ.setdefault("HF_HUB_DISABLE_TELEMETRY", "1")
os.environ.setdefault("HF_HUB_DISABLE_IMPLICIT_TOKEN", "1")

import numpy as np
from PIL import Image, ImageDraw, ImageFont
from move_control import MoveCancelled, check_cancel

HERE = Path(__file__).resolve().parent
OUT = HERE / "local"
MODEL = "stabilityai/sd-turbo"
REVISION = "b261bac6fd2cf515557d5d0707481eafa0485ec2"


def write(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, indent=2) + "\n")


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def measures(before, after):
    a = np.asarray(Image.open(before).convert("RGB"), dtype=np.float32)
    b = np.asarray(Image.open(after).convert("RGB"), dtype=np.float32)
    d = np.abs(a-b)
    return {"mean_channel_delta_255": float(d.mean()),
            "changed_fraction_gt_8": float((d.max(axis=2) > 8).mean()),
            "changed_fraction_gt_24": float((d.max(axis=2) > 24).mean())}


def checkpoint(download=False):
    from huggingface_hub import snapshot_download
    return snapshot_download(
        MODEL, revision=REVISION, local_files_only=not download,
        allow_patterns=["model_index.json", "scheduler/*.json", "tokenizer/*",
                        "text_encoder/config.json", "text_encoder/*fp16.safetensors",
                        "unet/config.json", "unet/*fp16.safetensors",
                        "vae/config.json", "vae/*fp16.safetensors"],
        max_workers=2,
    )


def load(run_name):
    import torch
    import diffusers
    from diffusers import StableDiffusionImg2ImgPipeline
    if not torch.backends.mps.is_available():
        raise RuntimeError("This experiment expects the local Apple Metal GPU")
    torch.set_num_threads(2)
    t = time.perf_counter()
    pipe = StableDiffusionImg2ImgPipeline.from_pretrained(
        checkpoint(), torch_dtype=torch.float16, variant="fp16",
        use_safetensors=True, local_files_only=True,
    ).to("mps")
    pipe.set_progress_bar_config(disable=True)
    # SD-Turbo uses no classifier-free guidance. An empty prompt is the fixed
    # condition, not a new verbal edit instruction or a caption of the input.
    with torch.inference_mode():
        embed, _ = pipe.encode_prompt("", "mps", 1, False)
    torch.mps.synchronize()
    # No later call needs text encoding. Retain only the fixed empty embedding.
    pipe.text_encoder = None
    gc.collect()
    torch.mps.empty_cache()
    write(OUT / f"runtime-{run_name}.json", {
        "model": MODEL, "revision": REVISION, "device": "mps", "dtype": "float16",
        "torch": torch.__version__, "diffusers": diffusers.__version__,
        "model_load_and_empty_prompt_seconds": time.perf_counter()-t,
        "prompt": "", "canvas": [256, 256],
        "text_encoder_released_after_embedding": True,
        "postprocess": "None beyond the pipeline's normal RGB output conversion. No masks or blending.",
        "state": "Full 8-bit RGB image every move. No image latents persist between moves.",
        "timing": "Per-move time includes PNG read, VAE encode, denoising, VAE decode, GPU synchronization and PNG save. Excludes model load and cached empty prompt.",
    })
    print("Model loaded on Metal GPU.", flush=True)
    return pipe, embed


def reconstruction_controls(pipe):
    """Isolate deterministic VAE compression from the diffusion transformation."""
    import torch
    for start in ["base", "blank", "noise"]:
        before = HERE / "starts" / f"{start}.png"
        output = OUT / "vae-control" / start / "001.png"
        if output.exists():
            continue
        output.parent.mkdir(parents=True, exist_ok=True)
        torch.mps.synchronize()
        t = time.perf_counter()
        with torch.inference_mode():
            pixels = pipe.image_processor.preprocess(Image.open(before).convert("RGB")).to("mps", torch.float16)
            latent = pipe.vae.encode(pixels).latent_dist.mode()
            decoded = pipe.vae.decode(latent, return_dict=False)[0]
            result = pipe.image_processor.postprocess(decoded, output_type="pil")[0]
        torch.mps.synchronize()
        result.save(output)
        write(output.with_suffix(".json"), {
            "kind": "vae-control", "status": "complete", "cold_first_move": False,
            "input": str(before.relative_to(HERE)), "output": str(output.relative_to(HERE)),
            "model": MODEL, "revision": REVISION, "latent_selection": "distribution mode",
            "total_seconds": time.perf_counter()-t, "metrics": measures(before, output),
        })
        print("Saved VAE-only control: " + start, flush=True)


def move(pipe, embed, before, folder, step, strength, seed, cold=False, observe=None, cancel=None):
    import torch
    folder.mkdir(parents=True, exist_ok=True)
    output = folder / f"{step:03}.png"
    receipt = output.with_suffix(".json")
    identity = {"model": MODEL, "revision": REVISION, "input_sha256": sha(before),
                "strength": strength, "seed": seed, "num_inference_steps": 4,
                "guidance_scale": 0.0, "prompt": "", "size": [256, 256]}
    if receipt.exists():
        prior = json.loads(receipt.read_text())
        if prior.get("identity") != identity:
            raise RuntimeError(f"Existing run has different parameters: {receipt}")
        if prior.get("status") == "complete" and output.exists():
            return output
        raise RuntimeError(f"Inspect prior failed/interrupted run before retry: {receipt}")
    row = {"identity": identity, "status": "running", "cold_first_move": cold,
           "text_encoder_released_after_embedding": pipe.text_encoder is None,
           "created_at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
           "input": str(before.relative_to(HERE)), "output": str(output.relative_to(HERE)),
           "actual_denoise_steps": int(4*strength), "step": step}
    write(receipt, row)
    torch.mps.synchronize()
    t = time.perf_counter()
    try:
        check_cancel(cancel)
        im = Image.open(before).convert("RGB")
        if im.size != (256, 256):
            raise ValueError("Expected full 256x256 input")
        if observe:
            from model_trace import ModelTrace
        with torch.inference_mode(), (ModelTrace(pipe, observe) if observe else nullcontext()) as trace:
            result = pipe(prompt_embeds=embed, image=im, num_inference_steps=4,
                          strength=strength, guidance_scale=0.0,
                          generator=torch.Generator(device="cpu").manual_seed(seed),
                          **({"callback_on_step_end": trace.callback} if trace else {}))
        torch.mps.synchronize()
        check_cancel(cancel)
        result.images[0].save(output)
        if trace:
            trace.sample("Output", "x′ = D(z′)")
        row.update(status="complete", total_seconds=time.perf_counter()-t,
                   observed=bool(observe),
                   returned_size=list(result.images[0].size), output_sha256=sha(output),
                   mps_allocated_bytes=torch.mps.current_allocated_memory(),
                   mps_driver_allocated_bytes=torch.mps.driver_allocated_memory(),
                   metrics=measures(before, output))
        write(receipt, row)
        print(json.dumps({"output": row["output"], "seconds": round(row["total_seconds"], 3),
                          "changed_pct": round(row["metrics"]["changed_fraction_gt_8"]*100, 1)}), flush=True)
        return output
    except MoveCancelled:
        row.update(status="cancelled", seconds=time.perf_counter()-t)
        write(receipt, row)
        raise
    except Exception as error:
        row.update(status="failed", error=str(error)[:500], seconds=time.perf_counter()-t)
        write(receipt, row)
        raise


def report():
    rows = [json.loads(p.read_text()) for p in sorted(OUT.glob("*/*/*.json"))
            if p.name != "session.json"]
    complete = [r for r in rows if r["status"] == "complete" and r.get("kind") != "vae-control"]
    warm = [r["total_seconds"] for r in complete if not r["cold_first_move"]]
    write(OUT / "results.json", rows)
    by_strength = {}
    for strength in [.25, .5, .75]:
        values = [r["total_seconds"] for r in complete
                  if not r["cold_first_move"] and r["identity"]["strength"] == strength]
        if values:
            by_strength[str(strength)] = {"count": len(values), "median_seconds": statistics.median(values),
                                           "min_seconds": min(values), "max_seconds": max(values)}
    write(OUT / "summary.json", {
        "completed": len(complete), "warm_median_seconds": statistics.median(warm) if warm else None,
        "warm_min_seconds": min(warm) if warm else None,
        "warm_max_seconds": max(warm) if warm else None,
        "by_strength": by_strength,
        "note": "Descriptive timings of these specific moves, not a general model benchmark.",
    })
    font = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial.ttf", 18)
    panel = Image.new("RGB", (1080, 938), "#eee9df")
    draw = ImageDraw.Draw(panel)
    for c, label in enumerate(["Input", "Strength 0.25", "Strength 0.50", "Strength 0.75"]):
        draw.text((c*270+7, 8), label, font=font, fill="#111111")
    for r, start in enumerate(["blank", "noise", "base"]):
        y = r*302+40
        paths = [HERE / "starts" / f"{start}.png"] + [OUT / f"strength-{s:.2f}" / start / "001.png" for s in [.25, .5, .75]]
        for c, path in enumerate(paths):
            if path.exists():
                panel.paste(Image.open(path).convert("RGB"), (c*270+7, y))
        draw.text((7, y+264), start, font=font, fill="#111111")
    panel.save(OUT / "comparison.png")
    chain = [HERE / "starts/base.png"] + sorted((OUT / "chain/base").glob("*.png"))
    if len(chain) > 1:
        frames = [Image.open(p).convert("RGB").resize((512,512), Image.Resampling.NEAREST) for p in chain]
        frames[0].save(OUT / "chain.gif", save_all=True, append_images=frames[1:], duration=800, loop=0)
        strip = Image.new("RGB", (270*len(chain), 292), "#eee9df")
        strip_draw = ImageDraw.Draw(strip)
        for i, path in enumerate(chain):
            strip.paste(Image.open(path).convert("RGB"), (i*270+7, 30))
            strip_draw.text((i*270+7, 5), "Input" if i == 0 else f"Move {i}", font=font, fill="#111111")
        strip.save(OUT / "chain.png")
    data = json.dumps(complete).replace("</", "<\\/")
    page = '''<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Local No Paint moves</title>
<style>body{margin:24px;background:#eee9df;color:#171717;font:16px system-ui}h1{font-size:26px}select,button,input{font:inherit}nav{display:flex;gap:16px;align-items:center;flex-wrap:wrap;margin:24px 0}.pair{display:flex;gap:24px;flex-wrap:wrap}figure{margin:0}img{width:min(42vw,512px);min-width:256px;image-rendering:pixelated}figcaption{margin:8px 0}details{margin-top:24px;max-width:1000px}pre{white-space:pre-wrap}a{color:inherit}</style>
<h1>Local No Paint moves</h1>
<nav><label>Sequence <select id="sequence"><option value="single">One move</option><option value="chain">Repeated move</option></select></label><label>Input <select id="start"><option>base</option><option>noise</option><option>blank</option></select></label><label>Strength <select id="strength"><option>0.25</option><option>0.50</option><option>0.75</option></select></label><button id="prev" aria-label="Previous move">←</button><input id="turn" aria-label="Move number" type="range" min="0" max="0" value="0"><button id="next" aria-label="Next move">→</button><span id="count"></span></nav>
<div class="pair"><figure><img id="before" alt="Complete input image"><figcaption>Before</figcaption></figure><figure><img id="after" alt="Complete model output"><figcaption>After</figcaption></figure></div><p id="status"></p>
<p>SD-Turbo · 256 × 256 · empty prompt · local GPU. Each move receives the complete previous image. No masks or blending.</p>
<details><summary>Measurement</summary><pre id="detail"></pre></details><p><a href="comparison.png">All starting states and strengths</a> · <a href="chain.png">Whole sequence</a></p>
<script>const rows=DATA,$=id=>document.getElementById(id);let active=[];
function choose(){const chain=$('sequence').value==='chain';if(chain){$('start').value='base';$('strength').value='0.25'}$('start').disabled=chain;$('strength').disabled=chain;const folder=chain?'chain':'strength-'+$('strength').value;active=rows.filter(r=>r.output.startsWith('local/'+folder+'/'+$('start').value+'/')).sort((a,b)=>a.step-b.step);$('turn').max=Math.max(0,active.length-1);$('turn').value=0;show()}
function show(){const r=active[+$('turn').value];$('before').src='../'+(r?.input||'starts/'+$('start').value+'.png');$('after').style.visibility=r?'visible':'hidden';if(!r){$('status').textContent='Not generated.';$('count').textContent='';$('detail').textContent='';return}$('after').src='../'+r.output;$('count').textContent='Move '+r.step;$('status').textContent=r.total_seconds.toFixed(2)+' s'+(r.cold_first_move?' (first move after load)':'')+' · '+(r.metrics.changed_fraction_gt_8*100).toFixed(1)+'% pixels changed';$('detail').textContent=JSON.stringify(r,null,2)}
['sequence','start','strength'].forEach(id=>$(id).onchange=choose);$('turn').oninput=show;$('prev').onclick=()=>{$('turn').value=Math.max(0,+$('turn').value-1);show()};$('next').onclick=()=>{$('turn').value=Math.min(active.length-1,+$('turn').value+1);show()};choose();</script></html>'''.replace("DATA", data)
    (OUT / "index.html").write_text(page)
    print(json.dumps({"completed": len(complete), "warm_seconds": warm}), flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["pilot", "chain", "report"], nargs="?")
    parser.add_argument("--download", action="store_true")
    parser.add_argument("--turns", type=int, default=5)
    parser.add_argument("--controls", action="store_true")
    args = parser.parse_args()
    if args.download:
        checkpoint(download=True)
        print("Checkpoint cached. Generation can now run offline.", flush=True)
    if args.command is None:
        return
    if args.command == "report":
        return report()
    if not 1 <= args.turns <= 20:
        raise ValueError("Use 1–20 local moves")
    pipe, embed = load(args.command)
    cold = True
    if args.command == "pilot":
        for strength in [.25, .5, .75]:
            for start in ["base", "blank", "noise"]:
                move(pipe, embed, HERE / "starts" / f"{start}.png",
                     OUT / f"strength-{strength:.2f}" / start, 1, strength, 20261005, cold)
                cold = False
    else:
        before = HERE / "starts/base.png"
        for step in range(1, args.turns+1):
            before = move(pipe, embed, before, OUT / "chain/base", step, .25, 20261005, cold)
            cold = False
    if args.controls:
        reconstruction_controls(pipe)
    report()


if __name__ == "__main__":
    main()
