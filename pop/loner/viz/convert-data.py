#!/usr/bin/env python3
# convert-data.py — one-time sidecar baker for scorecast.c (the C port of
# review-score4.py). C should not carry an npz reader or a JSON parser or a
# TrueType rasterizer for a fixed set of sixty words — so everything the
# renderer needs that isn't raw audio gets flattened here, once, into
# pop/loner/viz/wg.bin:
#
#   * wg-retrace.npz  -> ordmap / pcoords / ptimes  (the pen path)
#   * wg-appear.npz   -> ink8, the final ink alpha as uint8 (exactly the
#                        (ink_a*255).astype(uint8) the Python resizes)
#   * wordclock.json  -> flat entries with word/mark as indices
#   * text atlases    -> every string the video ever draws, rasterized by
#                        PIL with the same YWFT ttfs at the same sizes and
#                        anchors as the Python renderer. stb_truetype.h is
#                        nowhere on this machine and can't be fetched, so
#                        instead of teaching C a font stack we hand it the
#                        same pixels PIL would have drawn.
#
# Run once (re-run only if the npz/json/fonts change):
#   python3 pop/loner/viz/convert-data.py
import json, struct, sys
import numpy as np
from PIL import Image, ImageDraw, ImageFont

VIZ = "/Users/jas/aesthetic-computer/pop/loner/viz"
FONT_B = "/Users/jas/aesthetic-computer/slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf"
FONT_R = "/Users/jas/aesthetic-computer/slab/menuband/Sources/MenuBand/Resources/ywft-processing-regular.ttf"

wg = np.load(f"{VIZ}/wg-appear.npz")
appear_mask = wg["final_mask"]
final = wg["final"]
paper = float(wg["paper"])
ink_a = np.clip((paper - final) / (paper - 60.0), 0.0, 1.0)
ink_a[~appear_mask] = 0.0
ink8 = (ink_a * 255).astype(np.uint8)          # same truncation as the Python

r = np.load(f"{VIZ}/wg-retrace.npz")
ORD = r["ordmap"].astype(np.int32)             # (698, 452), -1 = never ink
PENC = r["pcoords"].astype(np.int32)           # (n, 2) y,x in working res
PT12 = r["ptimes"].astype(np.float64) * 12.0   # pen rank -> source 12fps frame
WGH, WGW = ORD.shape
OM = PENC.shape[0]

WC = sorted(json.load(open(f"{VIZ}/wordclock.json")), key=lambda e: e["t0"])
words = sorted(set(e["word"] for e in WC))
marks = sorted(set(e["mark"] for e in WC))
wi = {w: i for i, w in enumerate(words)}
mi = {m: i for i, m in enumerate(marks)}

# ---- text atlases -------------------------------------------------------
# One glyph record per string: tight alpha bitmap plus the offset from the
# PIL anchor point to the bitmap's top-left, so C blits at (x+dx, y+dy) and
# lands on the same pixels ImageDraw.text(x, y, anchor=...) would.
f_title = ImageFont.truetype(FONT_B, 48)
f_lbl = ImageFont.truetype(FONT_B, 25)
f_tiny = ImageFont.truetype(FONT_R, 17)
f_mark = ImageFont.truetype(FONT_R, 20)
f_bar = ImageFont.truetype(FONT_R, 20)
f_tc = ImageFont.truetype(FONT_B, 56)
f_kara = ImageFont.truetype(FONT_B, 84)

def glyph(s, font, anchor, adv=0.0):
    img = Image.new("L", (2048, 512), 0)
    ImageDraw.Draw(img).text((256, 256), s, font=font, fill=255, anchor=anchor)
    bx = img.getbbox()
    if bx is None:                              # never happens for our strings
        return struct.pack("<HHhhf", 0, 0, 0, 0, adv)
    crop = np.array(img.crop(bx), np.uint8)
    h, w = crop.shape
    return struct.pack("<HHhhf", w, h, bx[0] - 256, bx[1] - 256, adv) + crop.tobytes()

LANE_NAMES = ["lead vox", "drums", "bass", "bells", "pluck", "pads",
              "ahh arps", "wub sub", "stamp"]
SECT_NAMES = ["sitting open", "big pass", "wind-down", "stamp+ring"]
EV_NAMES = ["arp up-dn", "arp", "SEAM", "arp", "arp", "stamp"]
TC_CHARS = "0123456789:."

groups = [
    [glyph("lonerclub (v4pid)", f_title, "la")],
    [glyph(s, f_lbl, "lm") for s in LANE_NAMES],
    [glyph(s, f_tiny, "la") for s in SECT_NAMES],
    [glyph(s, f_mark, "la") for s in EV_NAMES],
    [glyph(str(n), f_bar, "la") for n in range(1, 100)],
    [glyph(c, f_tc, "la", adv=f_tc.getlength(c)) for c in TC_CHARS],
    [glyph(s, f_kara, "lm") for s in words],
    [glyph(s, f_title, "lm") for s in marks],
]

# ---- wg.bin -------------------------------------------------------------
out = open(f"{VIZ}/wg.bin", "wb")
out.write(b"WGSB")
out.write(struct.pack("<IIII", 1, WGH, WGW, OM))
out.write(ink8.tobytes())
out.write(ORD.tobytes())
out.write(PENC.tobytes())
out.write(PT12.tobytes())
out.write(struct.pack("<I", len(WC)))
for e in WC:
    out.write(struct.pack("<ddddHH", e["t0"], e["t1"], e["v0"], e["v1"],
                          wi[e["word"]], mi[e["mark"]]))
out.write(struct.pack("<I", len(groups)))
for g in groups:
    out.write(struct.pack("<I", len(g)))
    for rec in g:
        out.write(rec)
out.close()
sz = sum(len(rec) for g in groups for rec in g)
print(f"wg.bin: {WGH}x{WGW} maps, {OM} pen points, {len(WC)} words, "
      f"{sum(len(g) for g in groups)} glyphs ({sz/1024:.0f}K of atlas)")
