"""Cut every drawing state out of Camille's LONER score.

The score is one whistlegraph per lyric word: 23 cells that accumulate from a
bare head-circle ("sitting") to the finished figure ("pass"). Any of them is a
canonical image of the graph, so this pulls them all out as separate PNGs —
grey drawing only, the black lyric word left behind — for use as cover subjects.
"""
import json
import os
import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
COVERS = os.path.dirname(HERE)
REFS = os.path.join(COVERS, "refs")
OUT = os.path.join(REFS, "cells")
os.makedirs(OUT, exist_ok=True)

WORDS = ["sitting", "curled", "up", "in", "my", "self",
         "i", "think", "of", "a", "stone", "just",
         "wait", "ting", "very", "pa", "tient",
         "ly", "for", "time", "to", "pass"]

im = Image.open(os.path.join(REFS, "loner-score.png")).convert("RGBA")
W, H = im.size
a = np.array(im)
alpha, rgb = a[..., 3], a[..., :3].astype(int)
mean = rgb.mean(axis=2)
grey = (alpha > 40) & (mean > 90) & (mean < 215)   # the drawing
dark = (alpha > 40) & (mean <= 90)                 # the words and the title


def bands(mask, axis, gap, floor):
    """Runs of rows (axis=1) or columns (axis=0) that carry ink."""
    prof = mask.sum(axis=axis)
    on = prof > floor
    out, start, blank = [], None, 0
    for i, v in enumerate(on):
        if v:
            if start is None:
                start = i
            blank = 0
        elif start is not None:
            blank += 1
            if blank >= gap:
                out.append((start, i - blank))
                start = None
    if start is not None:
        out.append((start, len(on) - 1))
    return out


# The black lyric words sit under each row of drawings, so they mark the row
# boundaries far more reliably than the drawings do — the figures' trailing
# lines run down into the next row and bridge any gap you'd look for.
word_rows = [r for r in bands(dark, 1, 25, 4) if r[1] - r[0] < H * 0.06]
rows, prev = [], int(H * 0.14)          # start below the LONER title
for (wy0, wy1) in word_rows:
    rows.append((prev, wy0 - 1))
    prev = wy1 + 1
print(f"{len(rows)} rows of figures")

cells, k = [], 0
for ri, (y0, y1) in enumerate(rows):
    band = grey[y0:y1 + 1]
    cols = [c for c in bands(band, 0, int(W * 0.012), 2) if c[1] - c[0] > W * 0.02]
    print(f"  row {ri + 1}: {len(cols)} cells")
    for (x0, x1) in cols:
        sub = grey[y0:y1 + 1, x0:x1 + 1]
        ys, xs = np.nonzero(sub)
        pad = int(W * 0.012)
        Y0, Y1 = max(0, y0 + ys.min() - pad), min(H, y0 + ys.max() + pad + 1)
        X0, X1 = max(0, x0 + xs.min() - pad), min(W, x0 + xs.max() + pad + 1)
        m = grey[Y0:Y1, X0:X1]
        src = rgb[Y0:Y1, X0:X1]
        out = np.full(src.shape, 255, np.uint8)
        out[m] = src[m].astype(np.uint8)     # paint the drawing only
        fig = Image.fromarray(out)
        word = WORDS[k] if k < len(WORDS) else f"cell{k:02d}"
        name = f"{k:02d}-{word}"
        fig.save(os.path.join(OUT, f"{name}.png"))
        side = int(max(fig.size) * 1.22)
        sq = Image.new("RGB", (side, side), (255, 255, 255))
        sq.paste(fig, ((side - fig.width) // 2, (side - fig.height) // 2))
        sq.save(os.path.join(OUT, f"{name}-square.png"))
        cells.append({"index": k, "word": word, "size": list(fig.size),
                      "box": [int(X0), int(Y0), int(X1), int(Y1)]})
        k += 1

json.dump(cells, open(os.path.join(OUT, "cells.json"), "w"), indent=1)
print(f"{k} cells → {OUT}")
