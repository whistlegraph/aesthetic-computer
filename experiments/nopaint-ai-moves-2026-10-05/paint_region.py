"""A 256px brush mask, native inpainting, and cropped-model fallback."""
import base64
import hashlib
from PIL import Image
from local_run import HERE, sha, write

MASK_BYTES = 256*256//8
NATIVE_MASK_ENGINES = {"evolve", "fleet-evolve"}


def decode_mask(bits):
    if not isinstance(bits, str) or len(bits) > 11000:
        raise ValueError("Invalid brush mask")
    try:
        raw = base64.b64decode(bits, validate=True)
    except ValueError:
        raise ValueError("Invalid brush mask") from None
    if len(raw) != MASK_BYTES:
        raise ValueError("Expected a 256x256 brush mask")
    return Image.frombytes("1", (256, 256), raw).convert("L")


def encode_mask(path):
    return base64.b64encode(Image.open(path).convert("1").tobytes()).decode()


def save_mask(bits, folder):
    if bits is None:
        return None
    image = decode_mask(bits)
    if not image.getbbox():
        return None
    path = folder / (hashlib.sha256(image.tobytes()).hexdigest()+".png")
    path.parent.mkdir(parents=True, exist_ok=True)
    image.save(path)
    return path


class Region:
    def __init__(self, before, mask, engine, folder):
        self.before, self.mask_path, self.folder = before, mask, folder
        self.input = before
        self.native = engine in NATIVE_MASK_ENGINES
        self.box = None
        self.mask = Image.open(mask).convert("L") if mask else None
        self.source = Image.open(before).convert("RGB") if mask else None
        if self.mask and not self.native:
            x0, y0, x1, y1 = self.mask.getbbox()
            side = min(256, max(x1-x0, y1-y0)+16)
            left = max(0, min(256-side, (x0+x1-side)//2))
            top = max(0, min(256-side, (y0+y1-side)//2))
            self.box = (left, top, left+side, top+side)
            self.input = folder / "input.png"
            folder.mkdir(parents=True, exist_ok=True)
            self.source.crop(self.box).resize((256, 256), Image.Resampling.BICUBIC).save(self.input)

    @property
    def kwargs(self):
        return {"mask": self.mask_path} if self.mask_path and self.native else {}

    def composite(self, path, output):
        if self.mask is None:
            return path
        generated = Image.open(path).convert("RGB")
        if self.box:
            full = self.source.copy()
            side = self.box[2]-self.box[0]
            full.paste(generated.resize((side, side), Image.Resampling.LANCZOS), self.box)
            generated = full
        result = Image.composite(generated, self.source, self.mask)
        output.parent.mkdir(parents=True, exist_ok=True)
        result.save(output)
        return output

    def preview(self, frame):
        if self.mask is not None and frame.get("image"):
            path = self.composite(HERE / frame["image"], self.folder / "frames" / f"{frame['index']:03}.png")
            return {**frame, "image": str(path.relative_to(HERE)), "outside_mask_preserved": True}
        return frame

    def finish(self, path):
        if self.mask is None:
            return path
        output = self.composite(path, self.folder / "result.png")
        write(self.folder / "region.json", {
            "method": "native-mask" if self.native else "cropped-buffer",
            "input_sha256": sha(self.before), "mask_sha256": sha(self.mask_path),
            "model_input_sha256": sha(self.input), "model_output_sha256": sha(path),
            "crop": self.box, "output_sha256": sha(output), "outside_mask_preserved": True})
        return output
