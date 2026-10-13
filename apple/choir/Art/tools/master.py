"""Master Fía's extracted pieces: name them, drop duplicates, clean them,
and write PNG + SVG masters with an index.

usage: python master.py <run_dir> <out_dir>
"""
import json
import subprocess
import sys
from pathlib import Path

import numpy as np
from PIL import Image
from scipy import ndimage as ndi

run, out = Path(sys.argv[1]), Path(sys.argv[2])
pieces = json.loads((run / "pieces.json").read_text())

# What each extracted piece is, read off run2's contact sheet (index → kind).
# Kinds: word-<w>, daisy-petal-<center>, daisy-loop, glyph-<g>; None = junk
# (paper edges, partial crops, two drawings fused).
kind = {}
def put(k, *ids):
    for i in ids: kind[i] = k
put("word-discover", 1, 5, 7, 8, 86, 88, 89, 106)
put("word-send", 2, 4, 84, 107, 108)
put("word-reply", 3, 6, 83, 110)
put("word-chat", 13, 14, 20, 92, 93, 98)
put("glyph-asterisk", 9, 91, 114)
put("glyph-x", 10, 15, 19, 21, 22, 90, 94, 97, 99, 100, 113, 116, 119, 139, 140, 141)
put("glyph-plus", 17, 96, 138)
put("glyph-question", 23, 25, 28, 101, 104, 146)
put("glyph-bang", 103, 121, 126, 144)
put("glyph-c-arrow", 11, 34, 36, 112, 134, 152, 153)
put("glyph-c-cross", 37, 154)
put("daisy-petal-o", 16, 18, 31, 95, 117, 118, 131, 136)
put("daisy-petal-dot", 32, 35, 130)
put("daisy-petal-x", 26, 27, 29, 33, 39, 127, 128, 132, 135, 145, 147, 149, 151, 156)
put("daisy-petal-star", 24, 102, 120, 125, 133, 143)
put("daisy-loop", *[i for i in range(42, 82) if i not in (54, 55, 63, 66, 70, 77, 80)], 38, 148, 155)

def grid_cross(part, cy, cx):
    """A grid intersection: a small + whose ink lies on the horizontal and
    vertical lines through its centre (the paper's grid). Fía's own small
    marks are dots, diagonal x's and asterisks."""
    ys, xs = np.nonzero(part)
    if len(ys) < 12:
        return True
    span = max(ys.max() - ys.min(), xs.max() - xs.min()) + 1
    band = max(2.0, span * 0.14)
    on_axes = (np.abs(ys - cy) <= band) | (np.abs(xs - cx) <= band)
    fill = len(ys) / float((ys.max() - ys.min() + 1) * (xs.max() - xs.min() + 1))
    return on_axes.mean() > 0.8 and fill < 0.55

# The kind map above is keyed to the run where it was read (run2). Pieces
# from a later run carry their kind over by photo and box overlap.
def carry_kinds(reference):
    ref = json.loads((Path(reference) / "pieces.json").read_text())
    def iou(a, b):
        x0, y0 = max(a[0], b[0]), max(a[1], b[1])
        x1, y1 = min(a[2], b[2]), min(a[3], b[3])
        inter = max(0, x1 - x0) * max(0, y1 - y0)
        area = lambda r: (r[2] - r[0]) * (r[3] - r[1])
        return inter / float(area(a) + area(b) - inter)
    carried = {}
    for i, item in enumerate(pieces):
        best = max(((iou(item["box"], r["box"]), j) for j, r in enumerate(ref) if r["photo"] == item["photo"]), default=(0, -1))
        if best[0] > 0.5 and best[1] in kind:
            carried[i] = kind[best[1]]
    return carried

if len(sys.argv) > 3:
    kind = carry_kinds(sys.argv[3])

# Takes that are partial (cut by the photo edge or fused with a stray mark).
DROP = {"img_8396-10", "img_8401-09", "img_8395-13", "img_8395-07"}

def clean(alpha):
    """Drop grid residue: small marks that are faint, or outside the drawing."""
    ink = alpha > 64
    labels, n = ndi.label(ink)
    if n == 0:
        return alpha
    sizes = ndi.sum(ink, labels, range(1, n + 1))
    means = ndi.mean(alpha, labels, range(1, n + 1))
    big = sizes >= sizes.max() * 0.12
    core = np.isin(labels, 1 + np.flatnonzero(big))
    ys, xs = np.nonzero(core)
    y0, y1, x0, x1 = ys.min(), ys.max(), xs.min(), xs.max()
    keep = np.zeros(n, bool)
    for i in range(n):
        if big[i]:
            keep[i] = True
            continue
        part = labels == i + 1
        cy, cx = ndi.center_of_mass(part)
        inside = y0 <= cy <= y1 and x0 <= cx <= x1
        keep[i] = inside and means[i] > 175 and sizes[i] >= sizes.max() * 0.004 and not grid_cross(part, cy, cx)
    mask = ndi.binary_dilation(np.isin(labels, 1 + np.flatnonzero(keep)), iterations=2)
    return np.where(mask, alpha, 0).astype(np.uint8)

def crop(alpha, margin=0.06):
    ys, xs = np.nonzero(alpha > 24)
    y0, y1, x0, x1 = ys.min(), ys.max() + 1, xs.min(), xs.max() + 1
    m = int(margin * max(y1 - y0, x1 - x0))
    out = np.zeros((y1 - y0 + 2 * m, x1 - x0 + 2 * m), np.uint8)
    out[m:m + y1 - y0, m:m + x1 - x0] = alpha[y0:y1, x0:x1]
    return out

def signature(alpha, size=48):
    img = Image.fromarray(alpha).resize((size, size), Image.BILINEAR)
    v = np.asarray(img, np.float32).ravel()
    v -= v.mean()
    return v / (np.linalg.norm(v) + 1e-6)

# Load, clean and crop every named piece.
named = []
for i, item in enumerate(pieces):
    k = kind.get(i)
    if not k or item["id"] in DROP:
        continue
    alpha = np.asarray(Image.open(run / "pieces" / f"{item['id']}.png"))[..., 3]
    alpha = crop(clean(alpha))
    named.append({"kind": k, "source": item["id"], "photo": item["photo"], "alpha": alpha,
                  "res": int(max(alpha.shape)), "sig": signature(alpha)})

# One master per drawing: within a kind, takes that look alike (the same
# drawing in overlapping photos) collapse to the highest-resolution take.
masters = []
for k in sorted({n["kind"] for n in named}):
    takes = sorted([n for n in named if n["kind"] == k], key=lambda n: -n["res"])
    chosen = []
    for take in takes:
        twin = next((c for c in chosen if float(c["sig"] @ take["sig"]) > 0.72), None)
        if twin:
            twin.setdefault("also", []).append(take["source"])
        else:
            chosen.append(take)
    # Steadier order: left-to-right reading isn't known, so biggest first.
    for nn, c in enumerate(chosen, 1):
        c["name"] = f"choir-{k}-{nn:02d}"
        masters.append(c)

png_dir, svg_dir = out / "png", out / "svg"
png_dir.mkdir(parents=True, exist_ok=True)
svg_dir.mkdir(parents=True, exist_ok=True)
index = []
for m in masters:
    a = m["alpha"]
    # Master PNG: ink black, soft alpha, longest side ≤ 1024.
    img = Image.fromarray(np.dstack([np.zeros_like(a)] * 3 + [a]), "RGBA")
    if max(img.size) > 1024:
        img.thumbnail((1024, 1024), Image.LANCZOS)
    img.save(png_dir / f"{m['name']}.png")
    # SVG: potrace over the alpha at 50%, smoothed; fill is choir's ink.
    bitmap = Image.fromarray(np.where(a > 110, 0, 255).astype(np.uint8)).convert("1")
    pbm = svg_dir / f"{m['name']}.pbm"
    bitmap.save(pbm)
    subprocess.run(["potrace", str(pbm), "-s", "-o", str(svg_dir / f"{m['name']}.svg"),
                    "--turdsize", "8", "--alphamax", "1.1", "--opttolerance", "0.25",
                    "--color", "#1c1816"], check=True)
    pbm.unlink()
    index.append({"name": m["name"], "kind": m["kind"], "source": m["source"], "photo": m["photo"],
                  "duplicates": m.get("also", []), "px": [img.width, img.height]})

(out / "index.json").write_text(json.dumps(index, indent=1))
print(f"{len(named)} named takes → {len(masters)} masters")
for k in sorted({m['kind'] for m in index}):
    print(f"  {k}: {sum(1 for m in index if m['kind'] == k)}")
