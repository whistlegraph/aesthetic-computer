# Fía, 26.10.26: "If any of my writing isn't legible feel free to make lil
# edits!" Run after cut.py, before compose.py:
#   pop/.venv/bin/python marketing/choir/bubbles/legible.py
# Each fix is rebuilt only from her own letters, borrowed from other bubbles.
import os
import numpy as np
from PIL import Image
from scipy import ndimage as ndi

CUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "cut")
INK = (28, 24, 22)

def load(n):
    return np.array(Image.open(os.path.join(CUT, f"{n}.png")))[..., 3].copy(), \
           np.array(Image.open(os.path.join(CUT, f"{n}-filled.png")))[..., 3]

def save(n, alpha, body):
    ink = np.zeros(alpha.shape + (4,), np.uint8); ink[..., :3] = INK; ink[..., 3] = alpha
    Image.fromarray(ink).save(os.path.join(CUT, f"{n}.png"))
    fill = np.zeros_like(ink); fill[..., :3] = 255; fill[..., 3] = body
    Image.alpha_composite(Image.fromarray(fill), Image.fromarray(ink)).save(os.path.join(CUT, f"{n}-filled.png"))

def glyph(n, label):
    """One of her pen strokes in bubble n, by its label in the stroke atlas."""
    a, _ = load(n)
    lab, _ = ndi.label(a > 60, structure=np.ones((3, 3)))
    sy, sx = ndi.find_objects(lab)[label - 1]
    return np.where(lab[sy, sx] == label, a[sy, sx], 0).astype(float)

def fit(g, h):
    im = Image.fromarray(g.astype(np.uint8))
    return np.array(im.resize((max(1, round(im.width * h / im.height)), h), Image.LANCZOS))

def row(parts, gap):
    """parts: (alpha, drop) where drop moves a glyph down from the shared top line."""
    H = max(p.shape[0] + d for p, d in parts); W = sum(p.shape[1] for p, _ in parts) + gap * (len(parts) - 1)
    out = np.zeros((H, W)); x = 0
    for p, d in parts:
        out[d:d + p.shape[0], x:x + p.shape[1]] = np.maximum(out[d:d + p.shape[0], x:x + p.shape[1]], p); x += p.shape[1] + gap
    return out

# --- 01: "on Hyperallergic" ran into the bubble's edge and the end scrawled.
# Keep her "on", "Hy" and "perall"; rebuild "ergic" from her e and r in
# "before", g in "good", c in "ceramics", and the "!" of "applying!" flipped
# into an i. Then set the line a little smaller so it clears the outline.
a, body = load("01")
lab, _ = ndi.label(body > 0); low = lab == lab[330, 300]; inside = ndi.distance_transform_edt(low)
text = (a > 0) & (inside > 13)
tl, m = ndi.label(text, structure=np.ones((3, 3)))
line = {}
for i, (sy, sx) in enumerate(ndi.find_objects(tl)):
    if (sy.start + sy.stop) / 2 > 335 and (tl[sy, sx] == i + 1).sum() >= 20:
        line[sx.start] = (np.where(tl[sy, sx] == i + 1, a[sy, sx], 0), sy.start)
xs = sorted(line)                         # on, Hy, perall, (scrawled ergic)
on, perall = line[xs[0]][0], line[xs[2]][0]
for x in xs: g, y0 = line[x]; a[y0:y0 + g.shape[0], x:x + g.shape[1]][g > 0] = 0

xh = 36                                    # her x-height on this line
e = fit(glyph("08", 22), xh)               # "before"
r = fit(glyph("08", 21), xh)
gg = fit(glyph("11", 6), int(xh * 1.7))    # "good"
bang = glyph("08", 24); dot = glyph("08", 25)  # "!" of "applying!"
i_ = np.zeros((dot.shape[0] + 6 + bang.shape[0], max(dot.shape[1], bang.shape[1])))
i_[:dot.shape[0], :dot.shape[1]] = dot[::-1, ::-1]; i_[dot.shape[0] + 6:, :bang.shape[1]] = bang[::-1, ::-1]
i_ = fit(i_, int(xh * 1.45))
c = fit(glyph("10", 12), xh)               # "ceramics"
top = int(xh * 0.45)
ergic = row([(e, top), (r, top), (gg, top), (i_, 0), (c, top)], 3)
# "on Hyperall" keeps her own spacing; "ergic" sits on the baseline of "on".
y_top = min(line[x][1] for x in xs[:3]); y_bot = max(line[x][1] + line[x][0].shape[0] for x in xs[:3])
kept = np.zeros((y_bot - y_top, xs[2] + perall.shape[1] - xs[0]))
for x in xs[:3]:
    g, y0 = line[x]; kept[y0 - y_top:y0 - y_top + g.shape[0], x - xs[0]:x - xs[0] + g.shape[1]] = g
base = line[xs[0]][1] + on.shape[0] - y_top          # bottom of "on"
drop = base - top - xh
full = row([(kept, max(0, -drop)), (ergic, max(0, drop))], 4)

taken = ndi.binary_dilation(a > 40, iterations=6)       # keep clear of line two
cx = int(np.mean(np.where(low)[1])); best = None
for scale in np.arange(1.0, 0.6, -0.02):     # largest size that fits the empty space
    im = Image.fromarray(full.astype(np.uint8)); w, h = round(im.width * scale), round(im.height * scale)
    g = np.array(im.resize((w, h), Image.LANCZOS)).astype(float); ink = g > 40
    for y0 in range(320, a.shape[0] - h, 4):
        for x0 in range(cx - w // 2 - 60, cx - w // 2 + 61, 10):
            if x0 < 0 or x0 + w > a.shape[1]: continue
            if not (ink & ((inside[y0:y0 + h, x0:x0 + w] < 16) | taken[y0:y0 + h, x0:x0 + w])).any():
                best = (scale, y0, x0, g); break
        if best: break
    if best: break
scale, y0, x0, g = best
a[y0:y0 + h, x0:x0 + w] = np.maximum(a[y0:y0 + h, x0:x0 + w], g)
save("01", a, body)
print(f"01: line rebuilt at {scale:.2f}×")

# --- 06: "insure my" ran together into "insremy". Slide "insure" left and
# "my art?" right so the words have air between them.
def nudge(n, moves):
    a, body = load(n)
    lab, _ = ndi.label(a > 60, structure=np.ones((3, 3)))
    grown = ndi.grey_dilation(lab, size=(5, 5)) * (a > 0)   # carry each stroke's soft edge with it
    out = a.copy()
    for labels, dx in moves: out[np.isin(grown, labels)] = 0
    for labels, dx in moves:
        m = np.isin(grown, labels); ys, xs = np.where(m)
        out[ys, xs + dx] = np.maximum(out[ys, xs + dx], a[ys, xs])
    save(n, out, body)

nudge("06", [([14, 10, 12, 11], -14), ([13, 9, 8, 7, 6, 15], 12)])
print("06: words spaced")
