#!/usr/bin/env python3
"""
build-ac-platter.py — assemble the real AC screen captures (capture-ac-platter.mjs
output in marketing/captures/platter/) into one labeled contact-sheet "platter".

Real product frames, no generation. Use the platter as a reference board for
gpt-image-2 regens (e.g. the Restless Egg felt video's laptop screens) and as
laptop-screen imagery. Brand styling matches the deck/video (YWFT + Arial,
warm cream, coral accents).

Usage: python3 marketing/bin/build-ac-platter.py
"""

from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

HERE = Path(__file__).parent
SHOTS = HERE.parent / "captures" / "platter"
OUT = HERE.parent / "captures" / "ac-platter.png"

CREAM = (240, 230, 207); INK = (44, 32, 50); DIM = (120, 100, 86)
CORAL = (208, 96, 84); VIOLET = (138, 100, 184); GREEN = (110, 168, 92)
ACCENTS = [CORAL, VIOLET, GREEN]

YWFT_BOLD = str(Path.home() / "Library" / "Fonts" / "ywft-processing-bold.ttf")
ARIAL = "/System/Library/Fonts/Supplemental/Arial.ttf"
ARIAL_BOLD = "/System/Library/Fonts/Supplemental/Arial Bold.ttf"
FALLBACK = "/System/Library/Fonts/SFNS.ttf"

# name → (display label). Order here = order on the platter.
LABELS = [
    ("notepat",      "notepat — the instrument"),
    ("prompt",       "the prompt — aesthetic.computer"),
    ("kidlisp-roz",  "$roz — a KidLisp piece"),
    ("kidlisp-ger",  "$ger — a KidLisp piece"),
    ("laklok",       "laklok"),
]

THUMB_W = 1080
PAD = 80
GAP = 56
LABEL_H = 76


def font(path, size):
    try: return ImageFont.truetype(path, size)
    except: return ImageFont.truetype(FALLBACK, size)


def fit_contain(img, tw, th, bg=(18, 14, 24)):
    canvas = Image.new("RGB", (tw, th), bg)
    sw, sh = img.size
    s = min(tw / sw, th / sh)
    nw, nh = int(sw * s), int(sh * s)
    canvas.paste(img.resize((nw, nh), Image.LANCZOS), ((tw - nw) // 2, (th - nh) // 2))
    return canvas


present = [(n, l) for n, l in LABELS if (SHOTS / f"{n}.png").exists()]
if not present:
    raise SystemExit(f"no captures found in {SHOTS} — run capture-ac-platter.mjs first")

COLS = {1: 1, 2: 2, 3: 3, 4: 2}.get(len(present), 3)  # balanced grid, no empty cells

# thumbnail height from the first shot's aspect (they share a viewport)
first = Image.open(SHOTS / f"{present[0][0]}.png")
aspect = first.size[1] / first.size[0]
THUMB_H = int(THUMB_W * aspect)
cell_h = THUMB_H + LABEL_H

rows = (len(present) + COLS - 1) // COLS
HEAD = 200
W = PAD * 2 + COLS * THUMB_W + (COLS - 1) * GAP
H = HEAD + rows * cell_h + (rows - 1) * GAP + PAD

img = Image.new("RGB", (W, H), CREAM)
d = ImageDraw.Draw(img)

# header
d.text((PAD, 56), "AESTHETIC.COMPUTER", font=font(ARIAL_BOLD, 30), fill=CORAL)
d.text((PAD, 92), "screen platter / real product frames", font=font(YWFT_BOLD, 78), fill=INK)
d.rectangle([PAD, HEAD - 22, PAD + 90, HEAD - 14], fill=CORAL)

for i, (name, label) in enumerate(present):
    r, c = divmod(i, COLS)
    x = PAD + c * (THUMB_W + GAP)
    y = HEAD + r * (cell_h + GAP)
    accent = ACCENTS[i % len(ACCENTS)]
    thumb = fit_contain(Image.open(SHOTS / f"{name}.png").convert("RGB"), THUMB_W, THUMB_H)
    img.paste(thumb, (x, y))
    d.rectangle([x, y, x + THUMB_W - 1, y + THUMB_H - 1], outline=accent, width=4)
    d.ellipse([x, y + THUMB_H + 26, x + 14, y + THUMB_H + 40], fill=accent)
    d.text((x + 32, y + THUMB_H + 18), label, font=font(ARIAL, 36), fill=INK)

d.text((PAD, H - 58), "marketing/captures/platter · regenerate with capture-ac-platter.mjs",
       font=font(ARIAL, 24), fill=DIM)

img.save(OUT)
print(f"✓ {OUT}  ({len(present)} screens, {W}x{H})")
