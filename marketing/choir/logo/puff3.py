# choir logo round 14, 26.10.12: jeffrey: "more subtle relief", "more texture".
# puff2.py's thin puffy flowers, gentler light and shadow, with paper grain on
# the ground and a little tooth in the paint. 1–12.  python3 marketing/choir/logo/puff3.py
import os
import numpy as np
from PIL import Image, ImageDraw, ImageFilter
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "puff2.py")).read().split("T, G, L, COLS = 320")[0])   # place(), rounded(), GLYPHS, MUTED
rng = np.random.default_rng(7)

def grain(shape, scale, amount):
    """Paper tooth: fine noise plus a softer mottle."""
    fine = rng.normal(0, 1, shape)
    soft = np.array(Image.fromarray(((rng.normal(0, 1, shape) * 40 + 128).clip(0, 255)).astype(np.uint8)).filter(ImageFilter.GaussianBlur(scale))).astype(float)
    soft = (soft - soft.mean()) / max(soft.std(), 1e-6)
    return (fine * 0.6 + soft * 0.4) * amount

def relief(alpha, bg, ink, depth=0.45, paper=True, gloss=0.25):
    s0 = alpha.size[0]; s = s0 * 2
    al = alpha.resize((s, s), Image.LANCZOS)
    A = np.array(al.filter(ImageFilter.GaussianBlur(1))).astype(float) / 255
    h = np.array(al.filter(ImageFilter.GaussianBlur(s * 0.006))).astype(float) / 255
    h = h / max(h.max(), 1e-6) + grain(h.shape, 2, 0.015) * A        # tooth in the paint
    gy, gx = np.gradient(h)
    k = s * 0.035 * depth
    nx, ny, nz = -gx * k, -gy * k, np.ones_like(h)
    n = np.sqrt(nx ** 2 + ny ** 2 + nz ** 2); nx, ny, nz = nx / n, ny / n, nz / n
    diff = np.clip(nx * -0.45 + ny * -0.55 + nz * 0.70, 0, 1)
    flat = 0.70                                                        # the diffuse value on a flat top
    col = np.array(ink, float)[None, None, :] * (1 + (diff[..., None] - flat) * 0.9 * depth / 0.45) + 255 * (diff ** 40 * gloss * depth)[..., None]
    t = np.array(Image.new("RGB", (s, s), bg)).astype(float)
    if paper: t = t + grain((s, s), 3, 4.5)[..., None]                 # paper grain
    sh = np.array(al.filter(ImageFilter.GaussianBlur(s * 0.010))).astype(float) / 255
    sh = np.roll(np.roll(sh, int(s * 0.010), 0), int(s * 0.004), 1)
    t = t * (1 - 0.16 * depth / 0.45 * sh[..., None])
    t = t * (1 - A[..., None]) + np.clip(col, 0, 255) * A[..., None]
    return Image.fromarray(t.clip(0, 255).astype(np.uint8)).resize((s0, s0), Image.LANCZOS)

T, G, L, COLS = 320, 36, 46, 6
# the upright glyph (A4) reads heavy, so it gets a thinner line (jeffrey, 23:3x)
LINE = {"new": 6, "seven": 4}
tiles = [rounded(relief(place(g, LINE[name], how, T, 0.72), bg, ink)) for name, g, k, how in GLYPHS for bg, ink in MUTED]
rows = (len(tiles) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (240, 236, 229)); d = ImageDraw.Draw(sheet); f = font(32)
for i, t in enumerate(tiles):
    t.save(os.path.expanduser(f"~/Desktop/choir-flowers/puff3-{i + 1}.png"))
    r, c = divmod(i, COLS); x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 8), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-puff3.jpg"), quality=92)
print("ok", sheet.size)
