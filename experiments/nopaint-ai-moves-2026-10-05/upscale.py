"""Local, tiled Real-ESRGAN export. The 256px painting is never modified."""
import hashlib
import json
from pathlib import Path
import time
import urllib.request

from PIL import Image

HERE = Path(__file__).resolve().parent
MODEL = "realesr-general-x4v3"
WEIGHTS_URL = f"https://github.com/xinntao/Real-ESRGAN/releases/download/v0.2.5.0/{MODEL}.pth"
WEIGHTS_SHA256 = "8dc7edb9ac80ccdc30c3a5dca6616509367f05fbc184ad95b731f05bece96292"


def weights_path():
    path = HERE / "local/upscale-models" / f"{MODEL}.pth"
    if path.exists() and hashlib.sha256(path.read_bytes()).hexdigest() == WEIGHTS_SHA256:
        return path
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(".download")
    try:
        with urllib.request.urlopen(WEIGHTS_URL, timeout=60) as response:
            data = response.read(10_000_001)
        if len(data) > 10_000_000 or hashlib.sha256(data).hexdigest() != WEIGHTS_SHA256:
            raise ValueError("Upscaler model checksum failed. Try again.")
        temporary.write_bytes(data)
        temporary.replace(path)
    finally:
        temporary.unlink(missing_ok=True)
    return path


def tiled_inference(model, tensor, progress, tile=128, padding=36):
    """34 convolutions need a 34px halo; 36 prevents seams without blending."""
    import torch
    _, _, height, width = tensor.shape
    output = torch.empty((1, 3, height * 4, width * 4), dtype=tensor.dtype, device="cpu")
    total = ((height + tile - 1) // tile) * ((width + tile - 1) // tile)
    completed = 0
    with torch.inference_mode():
        for y in range(0, height, tile):
            for x in range(0, width, tile):
                right, bottom = min(x + tile, width), min(y + tile, height)
                left_pad, top_pad = max(0, x - padding), max(0, y - padding)
                right_pad, bottom_pad = min(width, right + padding), min(height, bottom + padding)
                result = model(tensor[:, :, top_pad:bottom_pad, left_pad:right_pad])
                output[:, :, y*4:bottom*4, x*4:right*4] = result[:, :,
                    (y-top_pad)*4:(bottom-top_pad)*4, (x-left_pad)*4:(right-left_pad)*4].cpu()
                completed += 1
                progress(completed / total)
    return output


def upscale_image(source, output, scale=4, progress=lambda value: None):
    if type(scale) is not int or scale not in (2, 4):
        raise ValueError("Choose 2× or 4× upscale.")
    started = time.monotonic()
    import numpy as np
    import torch
    from vendor.realesrgan.srvgg_arch import SRVGGNetCompact

    with Image.open(source) as image:
        if image.size != (256, 256):
            raise ValueError("Upscale expects the 256×256 painting.")
        pixels = np.asarray(image.convert("RGB"), dtype=np.float32).copy() / 255.0
    progress(0)
    weights = weights_path()
    device = "mps" if torch.backends.mps.is_available() else "cuda" if torch.cuda.is_available() else "cpu"
    model = SRVGGNetCompact(num_conv=32).eval()
    state = torch.load(weights, map_location="cpu", weights_only=True)
    model.load_state_dict(state.get("params_ema", state.get("params", state)))
    model = model.to(device)
    tensor = torch.from_numpy(pixels.transpose(2, 0, 1)).unsqueeze(0).to(device)
    result = tiled_inference(model, tensor, lambda fraction: progress(fraction * .95))
    pixels = result.squeeze(0).clamp(0, 1).permute(1, 2, 0).numpy()
    image = Image.fromarray(np.rint(pixels * 255).astype(np.uint8))
    if scale == 2:
        image = image.resize((512, 512), Image.Resampling.LANCZOS)
    output = Path(output)
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_suffix(".tmp")
    image.save(temporary, format="PNG")
    temporary.replace(output)
    receipt = {"model": MODEL, "weights_sha256": WEIGHTS_SHA256, "device": device,
               "scale": scale, "size": list(image.size), "braincells": 0,
               "source_sha256": hashlib.sha256(Path(source).read_bytes()).hexdigest(),
               "output_sha256": hashlib.sha256(output.read_bytes()).hexdigest(),
               "seconds": round(time.monotonic() - started, 3)}
    output.with_suffix(".json").write_text(json.dumps(receipt, indent=2) + "\n")
    del model, tensor, result
    if device == "mps":
        torch.mps.empty_cache()
    progress(1)
    return receipt
