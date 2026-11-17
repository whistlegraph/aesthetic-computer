# choir speech bubbles, 26.10.26: Fía's hand-drawn page of things people say in
# choir (her photo IMG_8819), cut into one transparent PNG per bubble.
#   pop/.venv/bin/python marketing/choir/bubbles/cut.py <photo.png>
# Writes bubbles/cut/NN.png (ink only) and bubbles/cut/NN-filled.png (ink on a
# white bubble, for laying over color). compose.py arranges them.
import os, sys
import numpy as np
from PIL import Image
from scipy import ndimage as ndi

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "cut"); os.makedirs(OUT, exist_ok=True)

im = Image.open(sys.argv[1]).convert("RGB").rotate(-90, expand=True)  # she shot it sideways
g = np.array(im.convert("L")).astype(float)

# The paper: the biggest bright region. Crop to it, a little inside the edge.
paper = ndi.binary_opening(g > 170, iterations=4)
lab, n = ndi.label(paper)
biggest = 1 + np.argmax(ndi.sum(paper, lab, range(1, n + 1)))
ys, xs = np.where(lab == biggest)
y0, y1, x0, x1 = ys.min() + 30, ys.max() - 30, xs.min() + 30, xs.max() - 30
g = g[y0:y1, x0:x1]

# Ink is darker than the paper around it; the lamp makes the paper uneven, so
# compare against a blurred copy rather than one threshold.
bg = ndi.gaussian_filter(ndi.maximum_filter(g, 41), 30)
dark = np.clip((bg - g) / bg, 0, 1)
# Pen is near black and the grid dots never are, so start from the darkest
# strokes and let them grow only a few pixels into their own soft edge.
core = dark > 0.6
lab, n = ndi.label(core, structure=np.ones((3, 3)))
core = np.isin(lab, 1 + np.where(np.asarray(ndi.sum(core, lab, range(1, n + 1))) >= 30)[0])
ink = ndi.binary_dilation(core, iterations=3) & (dark > 0.28)

# Each bubble is a closed outline: fill it, and the text inside comes with it.
# Where two bubbles touch, erode until they part, then grow each one back.
shape = ndi.binary_fill_holes(ndi.binary_closing(ink, iterations=6))
parts, _ = ndi.label(shape)
rim = np.unique(np.r_[parts[0], parts[-1], parts[:, 0], parts[:, -1]])
shape &= ~np.isin(parts, rim[rim > 0])  # the paper's shadowed edge, not a bubble
seeds, n = ndi.label(ndi.binary_erosion(shape, iterations=45))
sizes = ndi.sum(seeds > 0, seeds, range(1, n + 1))
keep = [i + 1 for i, s in enumerate(sizes) if s > 0.0005 * shape.size]
seeds = np.where(np.isin(seeds, keep), seeds, 0)
near = ndi.distance_transform_edt(seeds == 0, return_distances=False, return_indices=True)
parts, _ = ndi.label(shape)
shape &= np.isin(parts, np.unique(parts[seeds > 0]))  # stray specks belong to no bubble
lab = np.where(shape, seeds[near[0], near[1]], 0)

# "Painter, LA" sits on the bubble below it like a name tag on a post; the
# two stay one piece.
objs = ndi.find_objects(lab)

alpha = (np.clip((dark - 0.3) / 0.25, 0, 1) * ndi.binary_dilation(ink, iterations=2) * 255).astype(np.uint8)
pad = 12
order = sorted(keep, key=lambda i: (objs[i - 1][0].start // 400, objs[i - 1][1].start))
for k, i in enumerate(order, 1):
    sy, sx = objs[i - 1]
    ya, yb = max(sy.start - pad, 0), min(sy.stop + pad, g.shape[0])
    xa, xb = max(sx.start - pad, 0), min(sx.stop + pad, g.shape[1])
    mine = ndi.binary_dilation(lab[ya:yb, xa:xb] == i, iterations=4)
    bits, m = ndi.label(alpha[ya:yb, xa:xb] * mine > 0, structure=np.ones((3, 3)))
    big = np.isin(bits, 1 + np.where(np.asarray(ndi.sum(bits > 0, bits, range(1, m + 1))) >= 40)[0])
    a = alpha[ya:yb, xa:xb] * big
    ink_rgba = np.zeros(a.shape + (4,), np.uint8); ink_rgba[..., :3] = (28, 24, 22); ink_rgba[..., 3] = a
    Image.fromarray(ink_rgba).save(os.path.join(OUT, f"{k:02d}.png"))
    body = ndi.binary_fill_holes(lab[ya:yb, xa:xb] == i)
    fill = Image.new("RGBA", (xb - xa, yb - ya), (255, 255, 255, 0))
    fill.putalpha(Image.fromarray((body * 255).astype(np.uint8)))
    Image.alpha_composite(fill, Image.fromarray(ink_rgba)).save(os.path.join(OUT, f"{k:02d}-filled.png"))
print(f"{len(order)} bubbles → {OUT}")
