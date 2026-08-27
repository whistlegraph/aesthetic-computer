"""Sleeves cut straight out of Camille's score — crops, not renderings.

Nothing here is generated. Each cover is a square window onto the actual
loner-score.png at full resolution, placed on a flat ground and recoloured.
The line is always hers.

Only the grey drawing is ever carried across: the black lyric words and the
LONER title are dropped by the mask, so no crop can accidentally include text.
"""
import json
import os
import numpy as np
from PIL import Image, ImageFilter

HERE = os.path.dirname(os.path.abspath(__file__))
COVERS = os.path.dirname(HERE)
REFS = os.path.join(COVERS, "refs")
SIDE = 3000

SCORE = Image.open(os.path.join(REFS, "loner-score.png")).convert("RGBA")
SW, SH = SCORE.size
_a = np.array(SCORE)
_alpha, _mean = _a[..., 3], _a[..., :3].astype(int).mean(axis=2)
GREY = (_alpha > 40) & (_mean > 90) & (_mean < 215)      # drawing only, never text
# A black word's anti-aliased edge is grey, so it survives the band above and
# leaves a ghost of the lyric in the crop. Grow the text a little and subtract.
_dark = Image.fromarray(np.where((_alpha > 40) & (_mean <= 90), 255, 0).astype(np.uint8))
_halo = np.array(_dark.filter(ImageFilter.MaxFilter(9))) > 0
GREY &= ~_halo
COVER = np.where(GREY, 255 - np.clip(_mean, 0, 255) + 60, 0).clip(0, 255).astype(np.uint8)
INK = Image.fromarray(COVER)

CELLS = json.load(open(os.path.join(REFS, "cells", "cells.json")))
BY_WORD = {c["word"]: c for c in CELLS}

PAPER = (247, 246, 243)
NIGHT = (9, 10, 24)
INDIGO = (18, 14, 44)


def crop(cx, cy, w):
    """A square window of the ink coverage, in score pixels."""
    w = int(min(w, SW, SH))
    x0 = int(np.clip(cx - w / 2, 0, SW - w))
    y0 = int(np.clip(cy - w / 2, 0, SH - w))
    return INK.crop((x0, y0, x0 + w, y0 + w))


def sleeve(mask, bg, ink, bleed=1.0):
    im = Image.new("RGBA", (SIDE, SIDE), bg + (255,))
    size = int(SIDE * bleed)
    m = mask.resize((size, size), Image.LANCZOS)
    layer = Image.new("RGBA", (size, size), ink + (0,))
    layer.putalpha(m)
    im.alpha_composite(layer, ((SIDE - size) // 2, (SIDE - size) // 2))
    return im


def save(im, name):
    p = os.path.join(COVERS, f"lonerclub-cover-{name}.png")
    im.convert("RGB").save(p)
    print("→", os.path.basename(p))


P = BY_WORD["pass"]["box"]                       # the finished figure
PW, PH = P[2] - P[0], P[3] - P[1]
PCX = (P[0] + P[2]) / 2


# n · knot — the densest place in the drawing, cropped until it stops being a
# person and becomes a tangle
save(sleeve(crop(PCX, P[1] + PH * 0.34, PW * 0.60), NIGHT, (238, 236, 244)), "n-knot")

# o · head — the face, its two eyes, and the line falling through it
save(sleeve(crop(PCX - PW * 0.02, P[1] + PH * 0.11, PW * 0.46), PAPER, (108, 110, 118)), "o-head")

# p · figure — the whole last cell, hot ink on night, nothing cropped away
save(sleeve(crop(PCX, P[1] + PH * 0.5, max(PW, PH) * 1.06), NIGHT, (236, 74, 172)), "p-figure")

# q · fall — the long lines running off the bottom of the figure
save(sleeve(crop(PCX + PW * 0.10, P[3] - PH * 0.18, PW * 0.78), INDIGO, (206, 222, 255)), "q-fall")

# s · two states — one step of the accumulation, the pair either side of a
# single word, kept at the spacing the score prints them at
A, B = BY_WORD["wait"]["box"], BY_WORD["ting"]["box"]
w = max(B[2] - A[0], max(A[3], B[3]) - min(A[1], B[1])) * 1.06
save(sleeve(crop((A[0] + B[2]) / 2, (min(A[1], B[1]) + max(A[3], B[3])) / 2, w),
            PAPER, (116, 118, 126)), "s-two-states")
