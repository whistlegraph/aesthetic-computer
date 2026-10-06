"""Seeded, CPU-only moves on the current RGB painting. No inference or network.

Each yielded image is an actual completed brush, tile, scanline, or filter pass;
there are no timed waits or synthetic tween frames. A fixed-point input (for
example a blank bitmap under a spatial transform) receives a small seed mark.

Uses the installed Pillow and NumPy APIs; no third-party source is vendored.
Pillow: MIT-CMU, https://github.com/python-pillow/Pillow/blob/main/LICENSE
NumPy: BSD-3-Clause, https://numpy.org/doc/stable/license.html
Pillow operations: https://pillow.readthedocs.io/en/stable/reference/index.html
"""
import datetime
import math
from pathlib import Path
import random
import time

import numpy as np
from PIL import Image, ImageDraw, ImageEnhance, ImageFilter, ImageOps, __version__ as pillow_version

from local_run import HERE, measures, sha, write
from move_control import MoveCancelled, check_cancel

REVISION = "1"
STRENGTHS = (.25, .5, .75)
FAMILIES = {
    "classic-pixels": ("slice", "turn", "sort", "mosaic"),
    "classic-color": ("hue", "tint", "contrast", "posterize"),
    "classic-primitives": ("stroke", "shapes", "stipple", "hatch", "diffuse"),
}
FAMILIES["classic"] = tuple(operation for family in FAMILIES.values() for operation in family)
LABELS = {"slice": "Shift strips", "turn": "Turn tiles", "sort": "Sort pixels",
          "mosaic": "Mosaic", "hue": "Rotate hue", "tint": "Tint",
          "contrast": "Contrast", "posterize": "Posterize", "stroke": "Stroke",
          "shapes": "Shapes", "stipple": "Stipple", "hatch": "Hatch", "diffuse": "Diffuse"}


def _box(rng, side):
    x, y = rng.randrange(257-side), rng.randrange(257-side)
    return x, y, x+side, y+side


def _color(image, rng):
    """Start with a canvas color, then perturb it enough to draw on a blank."""
    sample = image.getpixel((rng.randrange(256), rng.randrange(256)))
    return tuple(max(0, min(255, channel + rng.choice((-1, 1))*rng.randint(48, 112)))
                 for channel in sample)


def _slice(image, level, rng):
    pixels = np.array(image)
    vertical = rng.choice((False, True))
    if vertical:
        pixels = pixels.transpose(1, 0, 2).copy()
    for _ in range((3, 5, 8)[level]):
        width = (6, 12, 24)[level]
        start = rng.randrange(257-width)
        offset = rng.choice((-1, 1))*rng.randint(1, (4, 12, 28)[level])
        pixels[start:start+width] = np.roll(pixels[start:start+width], offset, axis=1)
        yield Image.fromarray(pixels.transpose(1, 0, 2) if vertical else pixels).copy()


def _turn(image, level, rng):
    image = image.copy()
    choices = (Image.Transpose.ROTATE_90, Image.Transpose.ROTATE_270,
               Image.Transpose.FLIP_LEFT_RIGHT, Image.Transpose.FLIP_TOP_BOTTOM)
    for _ in range((2, 4, 6)[level]):
        box = _box(rng, (24, 48, 80)[level])
        image.paste(image.crop(box).transpose(rng.choice(choices)), box)
        yield image.copy()


def _sort(image, level, rng):
    pixels = np.array(image)
    vertical = rng.choice((False, True))
    if vertical:
        pixels = pixels.transpose(1, 0, 2).copy()
    x0, y0, x1, y1 = _box(rng, (32, 64, 112)[level])
    descending = rng.choice((False, True))
    for start in range(y0, y1, (8, 16, 28)[level]):
        patch = pixels[start:start+(8, 16, 28)[level], x0:x1]
        luminance = patch.astype(np.uint16).sum(axis=2)
        order = np.argsort(luminance, axis=1, kind="stable")
        if descending:
            order = order[:, ::-1]
        pixels[start:start+patch.shape[0], x0:x1] = np.take_along_axis(patch, order[:, :, None], axis=1)
        yield Image.fromarray(pixels.transpose(1, 0, 2) if vertical else pixels).copy()


def _mosaic(image, level, rng):
    image = image.copy()
    for _ in range((3, 5, 8)[level]):
        side = (24, 40, 64)[level]
        box = _box(rng, side)
        patch = image.crop(box)
        cells = max(2, side // (4, 8, 16)[level])
        patch = patch.resize((cells, cells), Image.Resampling.BOX).resize((side, side), Image.Resampling.NEAREST)
        image.paste(patch, box)
        yield image.copy()


def _hue(image, level, rng):
    shift = rng.choice((-1, 1))*(2, 5, 10)[level]
    for _ in range(4):
        hue, saturation, value = image.convert("HSV").split()
        hue = hue.point([(v+shift) % 256 for v in range(256)])
        image = Image.merge("HSV", (hue, saturation, value)).convert("RGB")
        yield image


def _tint(image, level, rng):
    tint = Image.new("RGB", image.size, _color(image, rng))
    for _ in range(4):
        image = Image.blend(image, tint, (.035, .07, .13)[level])
        yield image


def _contrast(image, level, rng):
    direction = rng.choice((-1, 1))
    factor = 1 + direction*(.025, .065, .13)[level]
    for _ in range(4):
        image = ImageEnhance.Contrast(image).enhance(factor)
        yield image


def _posterize(image, level, rng):
    for bits in range(7, (5, 4, 2)[level], -1):
        image = ImageOps.posterize(image, bits)
        yield image


def _stroke(image, level, rng):
    image = image.copy()
    draw = ImageDraw.Draw(image, "RGBA")
    color = (*_color(image, rng), rng.randint(120, 220))
    x, y = rng.randrange(256), rng.randrange(256)
    angle = rng.random()*math.tau
    for _ in range(8):
        angle += rng.uniform(-.45, .45)
        length = (5, 10, 18)[level]
        nx = max(0, min(255, x+math.cos(angle)*length))
        ny = max(0, min(255, y+math.sin(angle)*length))
        draw.line((x, y, nx, ny), fill=color, width=(2, 5, 10)[level])
        x, y = nx, ny
        yield image.copy()


def _shapes(image, level, rng):
    image = image.copy()
    draw = ImageDraw.Draw(image, "RGBA")
    kind = rng.choice(("ellipse", "rectangle", "triangle"))
    for _ in range((2, 4, 6)[level]):
        color = (*_color(image, rng), rng.randint(100, 200))
        side = rng.randint((12, 24, 40)[level], (24, 48, 80)[level])
        x0, y0, x1, y1 = _box(rng, side)
        if kind == "triangle":
            draw.polygon(((x0, y1-1), ((x0+x1)//2, y0), (x1-1, y1-1)), fill=color)
        else:
            getattr(draw, kind)((x0, y0, x1-1, y1-1), fill=color)
        yield image.copy()


def _stipple(image, level, rng):
    image = image.copy()
    draw = ImageDraw.Draw(image, "RGBA")
    color = (*_color(image, rng), rng.randint(120, 220))
    x0, y0 = rng.randrange(256), rng.randrange(256)
    spread, radius = (10, 22, 40)[level], (1, 2, 3)[level]
    for _ in range(4):
        for _ in range((16, 48, 112)[level]):
            x = max(0, min(255, round(rng.gauss(x0, spread))))
            y = max(0, min(255, round(rng.gauss(y0, spread))))
            draw.ellipse((x-radius, y-radius, x+radius, y+radius), fill=color)
        yield image.copy()


def _hatch(image, level, rng):
    image = image.copy()
    draw = ImageDraw.Draw(image, "RGBA")
    color = (*_color(image, rng), rng.randint(100, 200))
    x0, y0, x1, y1 = _box(rng, (32, 64, 112)[level])
    horizontal = rng.choice((False, True))
    for band in range(4):
        for line in range(2):
            offset = (band*2+line)*(y1-y0)//8
            xy = (x0, y0+offset, x1-1, y0+offset) if horizontal else (x0+offset, y0, x0+offset, y1-1)
            draw.line(xy, fill=color, width=(1, 2, 3)[level])
        yield image.copy()


def _diffuse(image, level, rng):
    image = image.copy()
    box = _box(rng, (48, 88, 144)[level])
    for _ in range(4):
        # Repeated local Gaussian passes are the actual algorithm state.
        patch = image.crop(box).filter(ImageFilter.GaussianBlur((.6, 1, 1.6)[level]))
        image.paste(patch, box)
        yield image.copy()


OPERATIONS = {name: globals()["_"+name] for name in FAMILIES["classic"]}


def _plan(strength, seed, family, operation):
    if isinstance(strength, bool) or strength not in STRENGTHS:
        raise ValueError("Classic strength must be .25 (Small), .5 (Medium), or .75 (Large)")
    if family not in FAMILIES:
        raise ValueError("Unknown classic engine")
    if not isinstance(seed, int) or isinstance(seed, bool) or not 0 <= seed < 2**63:
        raise ValueError("Expected a nonnegative integer seed below 2**63")
    rng = random.Random(seed)
    choice = rng.choice(FAMILIES[family])
    if operation is not None:
        if operation not in FAMILIES[family]:
            raise ValueError("Operation does not belong to this classic engine")
        choice = operation
    return STRENGTHS.index(strength), rng, choice


def states(source, strength, seed, *, family="classic", operation=None, cancel=None):
    """Yield (operation, actual RGB state); useful independently of file receipts."""
    level, rng, choice = _plan(strength, seed, family, operation)
    check_cancel(cancel)
    if source.size != (256, 256):
        raise ValueError("Expected full 256x256 input")
    source = source.convert("RGB")
    original = previous = source.tobytes()
    current = source
    for current in OPERATIONS[choice](source, level, rng):
        check_cancel(cancel)
        pixels = current.tobytes()
        if pixels != previous:
            yield choice, current
            check_cancel(cancel)
        previous = pixels
    if current.tobytes() == original:
        # A spatial permutation cannot evolve a uniform bitmap. Seed a visible,
        # bounded dot instead of returning an identical, apparently broken move.
        check_cancel(cancel)
        current = current.copy()
        draw = ImageDraw.Draw(current)
        side = (6, 10, 16)[level]
        x, y = rng.randrange(257-side), rng.randrange(257-side)
        color = tuple(v+96 if v < 128 else v-96 for v in current.getpixel((x, y)))
        draw.ellipse((x, y, x+side-1, y+side-1), fill=color)
        yield "seed", current
        check_cancel(cancel)


def move(pipe, embed, before, folder, step, strength, seed, cold=False, observe=None,
         cancel=None, *, family="classic"):
    from engines import ENGINES
    _, _, operation = _plan(strength, seed, family, None)
    before, folder = Path(before), Path(folder)
    output = folder / f"{step:03}.png"
    receipt = output.with_suffix(".json")
    temporary = output.with_suffix(".tmp.png")
    if output.exists() or receipt.exists():
        raise RuntimeError(f"Refusing to overwrite an existing move: {receipt}")
    check_cancel(cancel)
    folder.mkdir(parents=True, exist_ok=True)
    started, frames = time.perf_counter(), []
    row = {"status": "running", "step": step, "cold_first_move": cold,
           "created_at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
           "input": str(before.relative_to(HERE)), "output": str(output.relative_to(HERE)),
           "observed": bool(observe), "billing": {"braincells": 0},
           "identity": {"model": ENGINES[family]["model"], "revision": REVISION,
                        "input_sha256": sha(before), "strength": strength, "seed": seed,
                        "family": family, "operation": operation, "size": [256, 256],
                        "pillow": pillow_version, "numpy": np.__version__}}
    write(receipt, row)
    updates = 0
    try:
        with Image.open(before) as loaded:
            source = loaded.convert("RGB")
        for name, result in states(source, strength, seed, family=family, cancel=cancel):
            updates += 1
            if name == "seed":
                row["seed_mark"] = True
            if observe:
                frame_path = folder / f"{step:03}-frames" / f"{updates:03}.png"
                frame_path.parent.mkdir(parents=True, exist_ok=True)
                result.save(frame_path)
                check_cancel(cancel)
                frame = {"index": len(frames), "step": updates,
                         "stage": LABELS.get(name, "Seed mark"), "operation": name,
                         "seconds": time.perf_counter()-started,
                         "image": str(frame_path.relative_to(HERE)), "state_sha256": sha(frame_path),
                         "approximate_decode": False}
                frames.append(frame)
                observe(frame)
                check_cancel(cancel)
        check_cancel(cancel)
        result.save(temporary)
        check_cancel(cancel)
        temporary.replace(output)
        check_cancel(cancel)
        row.update(status="complete", total_seconds=time.perf_counter()-started,
                   actual_updates=updates, frames=frames, returned_size=list(result.size),
                   output_sha256=sha(output), metrics=measures(before, output))
        write(receipt, row)
        return output
    except MoveCancelled:
        output.unlink(missing_ok=True)
        row.update(status="cancelled", seconds=time.perf_counter()-started,
                   actual_updates=updates, frames=frames)
        write(receipt, row)
        raise
    except Exception as error:
        output.unlink(missing_ok=True)
        row.update(status="failed", error=str(error)[:500], seconds=time.perf_counter()-started,
                   actual_updates=updates, frames=frames)
        write(receipt, row)
        raise
    finally:
        temporary.unlink(missing_ok=True)
