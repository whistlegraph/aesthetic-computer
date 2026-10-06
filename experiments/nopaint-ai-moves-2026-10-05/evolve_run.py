#!/usr/bin/env python3
"""Multi-step local img2img with decoded previews of actual latent states."""
import argparse
import datetime
import gc
import hashlib
from pathlib import Path
import time

import numpy as np
from PIL import Image
from local_run import HERE, OUT, measures, sha, write
from move_control import MoveCancelled, check_cancel

MODEL = "stable-diffusion-v1-5/stable-diffusion-v1-5"
REVISION = "451f4fe16113bff5a5d2269ed5ad43b0592e9a14"
PREVIEW_MODEL = "madebyollin/taesd"
PREVIEW_REVISION = "614f76814bbe30edbe2e627ace1c2234c81a2c0e"
SCHEDULE_STEPS = 32


def checkpoints(download=False):
    from huggingface_hub import snapshot_download
    common = dict(local_files_only=not download, max_workers=2)
    main = snapshot_download(MODEL, revision=REVISION, allow_patterns=[
        "model_index.json", "scheduler/*.json", "tokenizer/*",
        "text_encoder/config.json", "text_encoder/*fp16.safetensors",
        "unet/config.json", "unet/*fp16.safetensors",
        "vae/config.json", "vae/*fp16.safetensors"], **common)
    preview = snapshot_download(PREVIEW_MODEL, revision=PREVIEW_REVISION,
                                allow_patterns=["config.json", "diffusion_pytorch_model.safetensors"], **common)
    return main, preview


def load(run_name):
    import torch
    from diffusers import AutoencoderTiny, EulerDiscreteScheduler, StableDiffusionImg2ImgPipeline
    if not torch.backends.mps.is_available():
        raise RuntimeError("This experiment expects the local Apple Metal GPU")
    torch.set_num_threads(2)
    started = time.perf_counter()
    main, preview = checkpoints()
    pipe = StableDiffusionImg2ImgPipeline.from_pretrained(
        main, torch_dtype=torch.float16, variant="fp16", use_safetensors=True,
        local_files_only=True, safety_checker=None, feature_extractor=None,
        requires_safety_checker=False).to("mps")
    pipe.scheduler = EulerDiscreteScheduler.from_config(pipe.scheduler.config)
    pipe.set_progress_bar_config(disable=True)
    with torch.inference_mode():
        embed, _ = pipe.encode_prompt("", "mps", 1, False)
    pipe.text_encoder = None
    gc.collect()
    torch.mps.empty_cache()
    pipe.preview_decoder = AutoencoderTiny.from_pretrained(
        preview, torch_dtype=torch.float16, use_safetensors=True, local_files_only=True).to("mps")
    pipe.preview_decoder.encoder = None
    torch.mps.synchronize()
    write(OUT / f"runtime-{run_name}.json", {
        "model": MODEL, "revision": REVISION, "preview_model": PREVIEW_MODEL,
        "preview_revision": PREVIEW_REVISION, "device": "mps", "dtype": "float16",
        "scheduler": type(pipe.scheduler).__name__, "schedule_steps": SCHEDULE_STEPS,
        "model_load_seconds": time.perf_counter()-started, "canvas": [256, 256],
        "prompt": "", "guidance_scale": 0, "text_encoder_released_after_embedding": True,
        "state": "Complete RGB input for each move. Real latent updates within each move.",
        "previews": "TAESD approximate decoding of actual post-update latents; original VAE for final image."})
    print("Multi-step model loaded on Metal GPU.", flush=True)
    return pipe, embed


def move(pipe, embed, before, folder, step, strength, seed, cold=False, observe=None, cancel=None, mask=None):
    import torch
    output = folder / f"{step:03}.png"
    receipt = output.with_suffix(".json")
    if output.exists() or receipt.exists():
        raise RuntimeError(f"Refusing to overwrite an existing move: {receipt}")
    folder.mkdir(parents=True, exist_ok=True)
    frame_folder = folder / f"{step:03}-frames"
    frame_folder.mkdir()
    started = time.perf_counter()
    frames, previews_seconds = [], 0.0
    calls = 0
    initial_preview = False
    total_steps = int(SCHEDULE_STEPS*strength)
    row = {"status": "running", "step": step, "cold_first_move": cold,
           "created_at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
           "input": str(before.relative_to(HERE)), "output": str(output.relative_to(HERE)),
           "actual_denoise_steps": total_steps, "observed": bool(observe),
           "identity": {"model": MODEL, "revision": REVISION, "input_sha256": sha(before),
                        "preview_model": PREVIEW_MODEL, "preview_revision": PREVIEW_REVISION,
                        "strength": strength, "seed": seed, "num_inference_steps": SCHEDULE_STEPS,
                        "guidance_scale": 0.0, "prompt": "", "size": [256, 256]}}
    if mask:
        row["identity"]["mask_sha256"] = sha(mask)
        row["inpaint_method"] = "four-channel latent preservation"
    write(receipt, row)

    def preview(latent, index, timestep, stage):
        nonlocal previews_seconds
        check_cancel(cancel)
        t = time.perf_counter()
        # TAESD consumes the diffusion model's scaled latents directly.
        decoded = pipe.preview_decoder.decode(latent, return_dict=False)[0]
        image = pipe.image_processor.postprocess(decoded, output_type="pil")[0]
        path = frame_folder / f"{index:03}.png"
        image.save(path)
        values = latent.detach().cpu().numpy()
        np.savez_compressed(path.with_suffix(".npz"), latent=values)
        frame = {"index": len(frames), "step": index, "steps": total_steps,
                 "stage": stage, "seconds": time.perf_counter()-started,
                 "timestep": float(timestep), "image": str(path.relative_to(HERE)),
                 "latent_sha256": hashlib.sha256(values.tobytes()).hexdigest(),
                 "preview_decoder": PREVIEW_MODEL, "approximate_decode": True}
        frames.append(frame)
        if observe:
            observe(frame)
        check_cancel(cancel)
        previews_seconds += time.perf_counter()-t

    original_add_noise = pipe.scheduler.add_noise

    def add_noise(original_samples, noise, timesteps):
        nonlocal initial_preview
        check_cancel(cancel)
        result = original_add_noise(original_samples, noise, timesteps)
        if observe and not initial_preview:
            initial_preview = True
            preview(result, 0, timesteps[0].item(), "Noisy input")
        return result

    def callback(pipeline, index, timestep, values):
        nonlocal calls
        calls += 1
        check_cancel(cancel)
        if observe:
            preview(values["latents"], index+1, timestep.item(), "Denoise")
        return values

    had_override = "add_noise" in vars(pipe.scheduler)
    original_override = vars(pipe.scheduler).get("add_noise")
    pipe.scheduler.add_noise = add_noise
    try:
        check_cancel(cancel)
        source = Image.open(before).convert("RGB")
        if source.size != (256, 256):
            raise ValueError("Expected full 256x256 input")
        torch.mps.synchronize()
        started = time.perf_counter()
        active = pipe
        options = {}
        if mask:
            from diffusers import StableDiffusionInpaintPipeline
            if not hasattr(pipe, "inpaint"):
                pipe.inpaint = StableDiffusionInpaintPipeline.from_pipe(pipe, torch_dtype=pipe.unet.dtype)
                pipe.inpaint.set_progress_bar_config(disable=True)
            active = pipe.inpaint
            options = {"mask_image": Image.open(mask).convert("L"), "height": 256, "width": 256}
        with torch.inference_mode():
            result = active(prompt_embeds=embed, image=source, num_inference_steps=SCHEDULE_STEPS,
                          strength=strength, guidance_scale=0.0,
                          generator=torch.Generator(device="cpu").manual_seed(seed),
                          callback_on_step_end=callback, **options)
        torch.mps.synchronize()
        check_cancel(cancel)
        result.images[0].save(output)
        if calls != total_steps:
            raise RuntimeError(f"Expected {total_steps} real updates, observed {calls}")
        row.update(status="complete", total_seconds=time.perf_counter()-started,
                   preview_seconds=previews_seconds, actual_denoise_steps=calls,
                   frames=frames, returned_size=list(result.images[0].size),
                   output_sha256=sha(output), metrics=measures(before, output),
                   mps_allocated_bytes=torch.mps.current_allocated_memory(),
                   mps_driver_allocated_bytes=torch.mps.driver_allocated_memory())
        write(receipt, row)
        print({"output": row["output"], "seconds": round(row["total_seconds"], 3),
               "actual_updates": calls, "previews": len(frames)}, flush=True)
        return output
    except MoveCancelled:
        row.update(status="cancelled", actual_denoise_steps=calls, frames=frames,
                   seconds=time.perf_counter()-started)
        write(receipt, row)
        raise
    except Exception as error:
        row.update(status="failed", error=str(error)[:500], seconds=time.perf_counter()-started)
        write(receipt, row)
        raise
    finally:
        if had_override:
            pipe.scheduler.add_noise = original_override
        else:
            del pipe.scheduler.add_noise


def benchmark(pipe=None, embed=None):
    if pipe is None:
        pipe, embed = load("evolution-benchmark")
    folder = OUT / "evolution-benchmark" / datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
    seen = []
    before = HERE / "starts/base.png"
    a = move(pipe, embed, before, folder, 1, .25, 20261005,
             cold=True, observe=lambda frame: seen.append(frame))
    b = move(pipe, embed, before, folder, 2, .25, 20261005)
    assert len(seen) == 9, "Expected noisy input plus eight denoised states"
    assert len({f["latent_sha256"] for f in seen}) == 9, "Latent states must really change"
    assert sha(a) == sha(b), "Previewing changed the model output"
    frames = [Image.open(before).convert("RGB")]+[Image.open(HERE / f["image"]).convert("RGB") for f in seen]+[Image.open(a).convert("RGB")]
    frames[0].save(folder / "trajectory.gif", save_all=True, append_images=frames[1:], duration=400, loop=0)
    strip = Image.new("RGB", (256*len(frames), 256))
    for i, frame in enumerate(frames):
        strip.paste(frame, (i*256, 0))
    strip.save(folder / "trajectory.png")
    write(folder / "verification.json", {"actual_updates": 8, "distinct_latent_states": 9,
                                        "preview_output_matches_unobserved": True})
    print("Verified real evolution and unchanged final output:", folder, flush=True)
    return folder


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--download", action="store_true")
    args = parser.parse_args()
    if args.download:
        print(checkpoints(download=True))
    else:
        benchmark()
