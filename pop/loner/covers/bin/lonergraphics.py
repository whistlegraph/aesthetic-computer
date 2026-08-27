"""Sleeves made from the recordings' own last frames.

Every loner whistlegraph was filmed being drawn. The final frame of each
recording is therefore the finished graphic as it actually exists — on notebook
paper, in whatever pen was to hand, with the pencil case still in shot. These
are documents, not renderings: each cover below is a square window onto one of
those frames, cropped to the drawing.

Crops are given as fractions of the frame so they survive a re-extract at a
different resolution.
"""
import os
import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
COVERS = os.path.dirname(HERE)
FRAMES = os.path.join(COVERS, "refs", "endframes")
# The recordings are TikTok-sourced and top out at 1080x1920, so a crop of the
# drawing is only 400-800 px across. 1600 clears DistroKid's minimum while
# keeping the upscale to 2-4x; 3000 would be 4-8x and visibly soft.
SIDE = 1600

# id, output name, (centre x, centre y, width) as fractions of the frame
PICKS = [
    ("7076361738786213166", "v-blue",    (0.245, 0.470, 0.480)),
    ("7100768279983181099", "w-pencil",  (0.560, 0.615, 0.760)),
    ("7108062006980201771", "x-close",   (0.520, 0.585, 0.720)),
    ("7021262898479549702", "y-red",     (0.430, 0.470, 0.760)),
    ("7173130377798716714", "z-graphite", (0.470, 0.520, 0.780)),
    ("7168612922757877035", "aa-lined",  (0.470, 0.470, 0.780)),
]


def square(img, cx, cy, w):
    W, H = img.size
    side = int(w * W)
    x0 = int(np.clip(cx * W - side / 2, 0, max(0, W - side)))
    y0 = int(np.clip(cy * H - side / 2, 0, max(0, H - side)))
    return img.crop((x0, y0, x0 + side, y0 + side))


for frame_id, name, (cx, cy, w) in PICKS:
    src = Image.open(os.path.join(FRAMES, f"{frame_id}.png")).convert("RGB")
    out = square(src, cx, cy, w).resize((SIDE, SIDE), Image.LANCZOS)
    p = os.path.join(COVERS, f"lonerclub-cover-{name}.png")
    out.save(p)
    print("→", os.path.basename(p), f"from {frame_id}")
