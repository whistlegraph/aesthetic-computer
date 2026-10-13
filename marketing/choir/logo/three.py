# choir logo round 4, 26.10.12: jeffrey liked #3 of thick.py (A4 at weight 5).
# Variations: background colors, nearby weights, size in the tile, a tilt. 1–12.
# python3 marketing/choir/logo/three.py
import os
from PIL import Image, ImageDraw
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "thick.py")).read().split("ROWS = [")[0])   # cutout(), weight(), tile(), font()
a4 = cutout(os.path.join(HERE, "flowers", "IMG_8396-136.png"))
BLUE, RED, YELLOW, PINK, SAGE, WHITE = (61, 120, 220), (207, 64, 64), (250, 214, 92), (244, 196, 206), (176, 199, 160), (255, 255, 255)

import numpy as np
from collections import deque

def comps(a):
    m = np.array(a) > 128; H, W = m.shape; lab = np.zeros((H, W), int); out = []; n = 0
    for y, x in zip(*np.where(m)):
        if lab[y, x]: continue
        n += 1; q = deque([(y, x)]); lab[y, x] = n; pts = []
        while q:
            cy, cx = q.popleft(); pts.append((cy, cx))
            for dy, dx in ((1, 0), (-1, 0), (0, 1), (0, -1)):
                yy, xx = cy + dy, cx + dx
                if 0 <= yy < H and 0 <= xx < W and m[yy, xx] and not lab[yy, xx]: lab[yy, xx] = n; q.append((yy, xx))
        out.append(np.array(pts))
    return out

def anchor(a, how):
    """The point of the flower that goes at the tile's center, as a fraction of its box."""
    w, h = a.size
    if how == "box": return 0.5, 0.5
    if how == "up": return 0.5, 0.535          # sits a touch high, the way eyes like it
    m = np.array(a).astype(float)
    if how == "mass":
        ys, xs = np.nonzero(m > 128); return xs.mean() / w, ys.mean() / h
    if how == "dot":   # her center dot: the smallest real blob near the middle
        cs = [c for c in comps(a) if 30 < len(c) < 0.05 * (m > 128).sum()]
        if not cs: return 0.5, 0.5
        c = min(cs, key=lambda c: ((c[:, 1].mean() / w - 0.5) ** 2 + (c[:, 0].mean() / h - 0.5) ** 2))
        return c[:, 1].mean() / w, c[:, 0].mean() / h

def make(k=5, bg=CREAM, fg=INK, scale=0.78, how="box", size=400):
    a = weight(a4, k)
    bb = a.getbbox(); a = a.crop(bb)
    t = Image.new("RGB", (size, size), bg)
    a.thumbnail((int(size * scale), int(size * scale)), Image.LANCZOS)
    ax, ay = anchor(a, how)
    x, y = int(size / 2 - ax * a.width), int(size / 2 - ay * a.height)
    t.paste(Image.new("RGB", a.size, fg), (x, y), a)
    m = Image.new("L", (size, size), 0); ImageDraw.Draw(m).rounded_rectangle((0, 0, size - 1, size - 1), radius=int(size * 0.22), fill=255)
    out = Image.new("RGBA", (size, size), (0, 0, 0, 0)); out.paste(t, (0, 0), m); return out

# rows: line weight; columns: how it's centered (box, her center dot, ink mass, optical lift)
VARS = [dict(k=k, how=h) for k in (3, 5, 7, 9) for h in ("box", "dot", "mass", "up")]
T, G, L, COLS = 400, 44, 54, 4
rows = (len(VARS) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (226, 219, 204)); d = ImageDraw.Draw(sheet); f = font(38)
for i, v in enumerate(VARS):
    r, c = divmod(i, COLS); t = make(**v)
    t.save(os.path.expanduser(f"~/Desktop/choir-flowers/three-{i + 1}.png"))
    x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 10), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-three.jpg"), quality=90)
print("ok", sheet.size)
