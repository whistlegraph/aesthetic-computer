# choir logo contact sheet, 26.10.12: Fía's own hand-drawn flowers (cut from her
# photos IMG_8396–8398, in flowers/) each on a colored icon tile, addressed A1, A2…
# so she can pick by letter-number.  python3 marketing/choir/logo/sheet.py
import os, glob
from collections import deque
import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.expanduser("~/Desktop/choir-flowers"); os.makedirs(OUT, exist_ok=True)
CREAM, INK = (246, 241, 230), (28, 24, 22)
# (background, flower) pairs from the app's palette, plus a few warm ones.
PAIRS = [(CREAM, INK), ((61, 120, 220), (255, 255, 255)), ((207, 64, 64), (255, 250, 240)), (INK, CREAM),
         ((250, 214, 92), INK), ((244, 196, 206), INK), ((176, 199, 160), INK), ((255, 255, 255), INK)]
COLS, TILE, GAP, LABEL = 5, 360, 44, 46

def cutout(path):
    """Ink → alpha; drop the paper's grey dots (small components)."""
    im = Image.open(path).convert("RGBA"); paper = Image.new("RGBA", im.size, (255, 255, 255, 255))
    g = np.array(Image.alpha_composite(paper, im).convert("L")).astype(int)
    ink = g < 110
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
    big = max(sizes); keep = np.isin(lab, [i for i, s in enumerate(sizes) if i and s >= big * 0.004 and s >= 40])
    alpha = np.clip((150 - g) * 4, 0, 255) * keep
    ys, xs = np.where(keep)
    return Image.fromarray(alpha.astype(np.uint8)).crop((xs.min(), ys.min(), xs.max() + 1, ys.max() + 1))

def tile(alpha, bg, fg, size=TILE):
    t = Image.new("RGB", (size, size), bg)
    a = alpha.copy(); a.thumbnail((int(size * 0.66), int(size * 0.66)), Image.LANCZOS)
    t.paste(Image.new("RGB", a.size, fg), ((size - a.width) // 2, (size - a.height) // 2), a)
    m = Image.new("L", (size, size), 0); ImageDraw.Draw(m).rounded_rectangle((0, 0, size - 1, size - 1), radius=int(size * 0.22), fill=255)
    out = Image.new("RGBA", (size, size), (0, 0, 0, 0)); out.paste(t, (0, 0), m)
    return out

def font(px):
    for f in ["/System/Library/Fonts/Supplemental/American Typewriter.ttc", "/System/Library/Fonts/Helvetica.ttc"]:
        if os.path.exists(f): return ImageFont.truetype(f, px)
    return ImageFont.load_default()

files = sorted(glob.glob(os.path.join(HERE, "flowers", "*.png")))
rows = (len(files) + COLS - 1) // COLS
W = COLS * TILE + (COLS + 1) * GAP; H = rows * (TILE + LABEL) + (rows + 1) * GAP + 30
sheet = Image.new("RGB", (W, H), (226, 219, 204)); d = ImageDraw.Draw(sheet); f = font(34)
key = []
for i, path in enumerate(files):
    r, c = divmod(i, COLS); addr = f"{chr(65 + r)}{c + 1}"
    bg, fg = PAIRS[i % len(PAIRS)]
    t = tile(cutout(path), bg, fg)
    t.save(os.path.join(OUT, f"{addr}.png"))
    x = GAP + c * (TILE + GAP); y = GAP + r * (TILE + LABEL + GAP)
    sheet.paste(t, (x, y), t)
    d.text((x + TILE / 2, y + TILE + 10), addr, fill=INK, font=f, anchor="mt")
    key.append(f"{addr}\t{os.path.basename(path)}")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-sheet.jpg"), quality=88)
open(os.path.join(OUT, "key.txt"), "w").write("\n".join(key) + "\n")
print(len(files), "flowers", sheet.size)
