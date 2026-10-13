# choir logo round 3, 26.10.12: Fía (23:15) "one more row with this one and try
# different thicknesses for all three drawings" — A4, E4 and her new IMG_8410,
# each at four line weights, ink on cream, numbered 1–12.
# python3 marketing/choir/logo/thick.py
import os
from collections import deque
import numpy as np
from PIL import Image, ImageDraw, ImageFilter
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "sheet.py")).read().split("files = sorted")[0])   # tile(), font(), colors

def photo_cutout(path):
    """Her pen on dotted paper: dark and not blue is ink; tiny blobs are the dots."""
    rgb = np.array(Image.open(path).convert("RGB")).astype(int)
    lum = rgb.mean(-1); blue = rgb[..., 2] - rgb[..., 0]
    ink = (lum < 95) & (blue < 25)
    H, W = ink.shape; lab = np.zeros((H, W), int); sizes = [0]; n = 0
    for y, x in zip(*np.where(ink)):
        if lab[y, x]: continue
        n += 1; q = deque([(y, x)]); lab[y, x] = n; c = 0
        while q:
            cy, cx = q.popleft(); c += 1
            for dy in (-1, 0, 1):
                for dx in (-1, 0, 1):
                    yy, xx = cy + dy, cx + dx
                    if 0 <= yy < H and 0 <= xx < W and ink[yy, xx] and not lab[yy, xx]: lab[yy, xx] = n; q.append((yy, xx))
        sizes.append(c)
    big = max(sizes); keep = np.isin(lab, [i for i, s in enumerate(sizes) if i and s >= big * 0.02])
    alpha = np.clip((140 - lum) * 4, 0, 255) * keep
    ys, xs = np.where(keep)
    return Image.fromarray(alpha.astype(np.uint8)).crop((xs.min(), ys.min(), xs.max() + 1, ys.max() + 1))

def weight(alpha, k):
    """Normalize to 900 px, pad, then grow (k>0) or shrink (k<0) the stroke by
    about k px with a round blur-and-threshold, so ends stay round."""
    a = alpha.copy(); a.thumbnail((900, 900), Image.LANCZOS)
    pad = 40; p = Image.new("L", (a.width + 2 * pad, a.height + 2 * pad), 0); p.paste(a, (pad, pad))
    p = p.point(lambda v: 255 if v > 90 else 0)
    if k:
        r = abs(k) * 0.9
        cut = 128 - min(110, int(abs(k) * 9)) if k > 0 else 128 + min(110, int(abs(k) * 14))
        p = p.filter(ImageFilter.GaussianBlur(r)).point(lambda v: 255 if v > cut else 0)
    return p.filter(ImageFilter.GaussianBlur(1.2))

ROWS = [("A4", cutout(os.path.join(HERE, "flowers", "IMG_8396-136.png"))),
        ("E4", cutout(os.path.join(HERE, "flowers", "IMG_8398-171.png"))),
        ("new", photo_cutout(os.path.join(HERE, "flowers-new-8410.jpg")))]
WEIGHTS = [-2, 0, 5, 9]           # thin, as drawn, bold, heavy (at 900 px)
T, G, L = 400, 44, 54
W = len(WEIGHTS) * T + (len(WEIGHTS) + 1) * G; H = len(ROWS) * (T + L) + (len(ROWS) + 1) * G
sheet = Image.new("RGB", (W, H), (226, 219, 204)); d = ImageDraw.Draw(sheet); f = font(38)
n = 0
for r, (name, a) in enumerate(ROWS):
    for c, k in enumerate(WEIGHTS):
        n += 1; t = tile(weight(a, k), CREAM, INK, T)
        t.save(os.path.expanduser(f"~/Desktop/choir-flowers/weight-{n}-{name}.png"))
        x = G + c * (T + G); y = G + r * (T + L + G)
        sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 10), str(n), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-weights.jpg"), quality=90)
print("ok", sheet.size)
