#!/usr/bin/env python
# glyph-atlas.py — Comic Sans MS Bold, rasterized once, so perf-rays.mjs's raw-pixel renderer can letter the lyric
# like the slab prox-rock / MacNeoPolitan Trio captions (menuband LyricCaption.swift: bubble fill + stroke + hard
# shadow, each glyph jittered on its own). Two 8-bit coverage layers per glyph, printable ASCII, one row:
#   fill  — the letter itself
#   outer — the letter grown by the stroke (perf-rays draws outer in the stroke colour, offset once more for the
#           hard shadow, then fill in the ink on top)
#
#   pop/.venv/bin/python pop/sailor-song/bin/glyph-atlas.py [--px 72] [--stroke 3]
#     → src/glyph-atlas/comic-<px>.json + .fill.raw + .outer.raw
import json, os, sys
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__)); LANE = os.path.dirname(HERE)
arg = lambda k, d: sys.argv[sys.argv.index(f"--{k}") + 1] if f"--{k}" in sys.argv else d
PX, STROKE = int(arg("px", 72)), int(arg("stroke", 3))
FONT = next((f for f in ["/System/Library/Fonts/Supplemental/Comic Sans MS Bold.ttf", "/Library/Fonts/Comic Sans MS Bold.ttf"] if os.path.exists(f)), None)
assert FONT, "Comic Sans MS Bold not found (fc-list | grep -i comic)"
font = ImageFont.truetype(FONT, PX); asc, desc = font.getmetrics()
PAD = STROKE + 2; CH = asc + desc + 2 * PAD                                   # one cell height for every glyph
chars = [chr(c) for c in range(32, 127)]
cells = []; x = 0
for ch in chars:
    l, t, r, b = font.getbbox(ch, stroke_width=STROKE); adv = font.getlength(ch)
    w = max(1, r - l) + 2 * PAD; cells.append((ch, x, w, l - PAD, adv)); x += w
W = x
fill = Image.new("L", (W, CH), 0); outer = Image.new("L", (W, CH), 0)
df, do = ImageDraw.Draw(fill), ImageDraw.Draw(outer)
for ch, cx, w, dx, adv in cells:
    do.text((cx - dx, PAD), ch, font=font, fill=255, stroke_width=STROKE, stroke_fill=255)
    df.text((cx - dx, PAD), ch, font=font, fill=255)
out = os.path.join(LANE, "src/glyph-atlas"); os.makedirs(out, exist_ok=True); stem = os.path.join(out, f"comic-{PX}")
open(stem + ".fill.raw", "wb").write(fill.tobytes()); open(stem + ".outer.raw", "wb").write(outer.tobytes())
json.dump({"px": PX, "stroke": STROKE, "W": W, "H": CH, "pad": PAD, "ascent": asc,
           "glyphs": {ch: {"x": cx, "w": w, "dx": dx, "adv": adv} for ch, cx, w, dx, adv in cells}}, open(stem + ".json", "w"))
print(f"✓ {stem}.json  {len(cells)} glyphs, {W}×{CH}")
