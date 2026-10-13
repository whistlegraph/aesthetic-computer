# choir logo variations, 26.10.12: Fía's four hand-drawn Cs (system/public/choir/mark.png)
# seated as a theatre of people. Each C is a person; every C opens toward the stage.
# python3 marketing/choir/logo/theatre.py  → ~/Desktop/choir-theatre/*.png
import math, random, os
from collections import deque
import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
MARK = os.path.join(HERE, "../../../system/public/choir/mark.png")
OUT = os.path.expanduser("~/Desktop/choir-theatre"); os.makedirs(OUT, exist_ok=True)
CREAM, INK = (246, 241, 230, 255), (28, 24, 22)

def strokes():
    a = np.array(Image.open(MARK).convert("RGBA"))
    ink = (a[..., 3] > 128) & (a[..., :3].mean(-1) < 128)
    H, W = ink.shape; lab = np.zeros((H, W), int); n = 0; out = []
    for y, x in zip(*np.where(ink)):
        if lab[y, x]: continue
        n += 1; q = deque([(y, x)]); lab[y, x] = n
        while q:
            cy, cx = q.popleft()
            for dy in (-2, -1, 0, 1, 2):
                for dx in (-2, -1, 0, 1, 2):
                    yy, xx = cy + dy, cx + dx
                    if 0 <= yy < H and 0 <= xx < W and ink[yy, xx] and not lab[yy, xx]: lab[yy, xx] = n; q.append((yy, xx))
    for i in range(1, n + 1):
        ys, xs = np.where(lab == i)
        if len(ys) < 2000: continue
        y0, y1, x0, x1 = ys.min(), ys.max() + 1, xs.min(), xs.max() + 1
        m = (lab[y0:y1, x0:x1] == i)
        alpha = (a[y0:y1, x0:x1, 3] * m).astype(np.uint8)
        rgba = np.zeros((y1 - y0, x1 - x0, 4), np.uint8); rgba[..., :3] = INK; rgba[..., 3] = alpha
        # A C opens away from its ink: from the ink's centroid toward its box's middle.
        cy, cx = ys.mean() - y0, xs.mean() - x0
        open_deg = math.degrees(math.atan2(-((y1 - y0) / 2 - cy), (x1 - x0) / 2 - cx))  # y up
        # Square canvas so rotation never clips; normalize so every C opens toward +x (right).
        side = int(math.hypot(*rgba.shape[:2])) + 4
        sq = Image.new("RGBA", (side, side), (0, 0, 0, 0))
        sq.paste(Image.fromarray(rgba), ((side - rgba.shape[1]) // 2, (side - rgba.shape[0]) // 2))
        out.append(sq.rotate(-open_deg, resample=Image.BICUBIC))
    return out

CS = strokes()

def seat(canvas, c, x, y, size, facing_deg):
    """Place a C of `size` px at (x, y), opening toward facing_deg (0 = right, 90 = up)."""
    s = c.resize((int(size), int(size)), Image.LANCZOS).rotate(facing_deg, resample=Image.BICUBIC)
    canvas.alpha_composite(s, (int(x - size / 2), int(y - size / 2)))

def face(x, y, sx, sy):  # the angle from (x, y) toward the stage, y up
    return math.degrees(math.atan2(-(sy - y), sx - x))

def amphitheatre(seed, W=2800, H=1700, rows=5, spread=180, stage=(1400, 1450), r0=260, gap=190, first=4, grow=1.0):
    rnd = random.Random(seed); im = Image.new("RGBA", (W, H), CREAM)
    for r in range(rows):
        rad = r0 + r * gap; count = first + int(r * (3 + grow * 1.6))
        for k in range(count):
            t = (k + 0.5) / count
            ang = math.radians(90 + spread / 2 - t * spread + rnd.uniform(-3, 3))
            x = stage[0] + rad * math.cos(ang); y = stage[1] - rad * math.sin(ang)
            size = (150 + r * 26) * rnd.uniform(0.88, 1.1)
            seat(im, rnd.choice(CS), x, y, size, face(x, y, *stage) + rnd.uniform(-14, 14))
    return im

def in_the_round(seed, W=1600, H=1600, rows=4):
    rnd = random.Random(seed); im = Image.new("RGBA", (W, H), CREAM); c = (W / 2, H / 2)
    for r in range(rows):
        rad = 150 + r * 165; count = 6 + r * 6
        off = rnd.uniform(0, 360)
        for k in range(count):
            ang = math.radians(off + k * 360 / count + rnd.uniform(-4, 4))
            x, y = c[0] + rad * math.cos(ang), c[1] - rad * math.sin(ang)
            seat(im, rnd.choice(CS), x, y, (130 + r * 22) * rnd.uniform(0.9, 1.1), face(x, y, *c) + rnd.uniform(-12, 12))
    return im

def crowds(seed, W=2000, H=1400):
    """Little half-daisies gathering, each crowd facing its own stage, a big one in the middle."""
    rnd = random.Random(seed); im = Image.new("RGBA", (W, H), CREAM)
    groups = [(1000, 1000, 3, 1.0), (420, 600, 2, 0.75), (1580, 580, 2, 0.75), (330, 1180, 2, 0.6), (1670, 1160, 2, 0.6), (1000, 330, 1, 0.7)]
    for gx, gy, rows, sc in groups:
        for r in range(rows):
            rad = (170 + r * 150) * sc; count = 4 + r * 4
            for k in range(count):
                t = (k + 0.5) / count; ang = math.radians(90 + 85 - t * 170 + rnd.uniform(-4, 4))
                x, y = gx + rad * math.cos(ang), gy - rad * math.sin(ang)
                seat(im, rnd.choice(CS), x, y, (140 + r * 20) * sc * rnd.uniform(0.9, 1.1), face(x, y, gx, gy) + rnd.uniform(-15, 15))
    return im

def grown(W=2400, H=1700):
    """Her half daisy exactly as drawn, with rows of new Cs seated around it."""
    im = Image.new("RGBA", (W, H), CREAM); mark = Image.open(MARK).convert("RGBA")
    mw = 760; mh = int(mark.height * mw / mark.width); mx, my = (W - mw) // 2, H - mh - 140
    im.alpha_composite(mark.resize((mw, mh), Image.LANCZOS), (mx, my))
    stage = (W / 2, my + mh * 0.62); rnd = random.Random(7)
    for r, (rad, count, size) in enumerate([(620, 11, 150), (820, 15, 170), (1010, 19, 185)]):
        for k in range(count):
            t = (k + 0.5) / count; ang = math.radians(90 + 82 - t * 164 + rnd.uniform(-2, 2))
            x, y = stage[0] + rad * math.cos(ang), stage[1] - rad * math.sin(ang) * 0.82
            seat(im, rnd.choice(CS), x, y, size * rnd.uniform(0.9, 1.08), face(x, y, *stage) + rnd.uniform(-12, 12))
    return im

def trim(im, pad=90):
    a = np.array(im); diff = np.abs(a[..., :3].astype(int) - CREAM[:3]).sum(-1) > 30
    ys, xs = np.where(diff); box = (max(xs.min() - pad, 0), max(ys.min() - pad, 0), min(xs.max() + pad, im.width), min(ys.max() + pad, im.height))
    return im.crop(box)

for name, im in [("1-amphitheatre", amphitheatre(3)), ("2-amphitheatre-wide", amphitheatre(11, rows=6, spread=200, r0=230, gap=165, grow=1.4)),
                 ("3-in-the-round", in_the_round(5)), ("4-crowds-forming", crowds(2)), ("5-half-daisy-grown", grown())]:
    trim(im).convert("RGB").save(os.path.join(OUT, name + ".png")); print(name)
