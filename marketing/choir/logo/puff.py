# choir logo round 12, 26.10.12: jeffrey: show both glyphs (Fía's new IMG_8410
# flower and the "original number 7", A4 at weight 7), colored inks on pale
# grounds, flat and as raised puffy paint. 1–12.  python3 marketing/choir/logo/puff.py
import os, colorsys
import numpy as np
from PIL import Image, ImageDraw, ImageFilter
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "shadow.py")).read().split("ROWS = [")[0])   # weight(), anchor(), hls(), new, a4

GLYPHS = [("new", new, 5, "dot"), ("seven", a4, 7, "mass")]
PAIRS = [(hls(100, 0.87, 0.30), hls(100, 0.24, 0.45)),   # green on sage
         (hls(220, 0.87, 0.30), hls(220, 0.24, 0.45)),   # navy on pale blue
         (hls(350, 0.87, 0.30), hls(350, 0.24, 0.45))]   # oxblood on rose

def place(glyph, k, how, size, scale):
    a = weight(glyph, k); a = a.crop(a.getbbox()); a.thumbnail((int(size * scale), int(size * scale)), Image.LANCZOS)
    ax, _ = anchor(a, how) if how == "dot" else (0.5, 0.5)
    if how == "mass": ax, _ = anchor(a, "mass")
    x, y = int(size / 2 - ax * a.width), int(size / 2 - 0.515 * a.height)
    c = Image.new("L", (size, size), 0); c.paste(a, (x, y)); return c

def flat(alpha, bg, ink):
    t = Image.new("RGB", alpha.size, bg); t.paste(Image.new("RGB", alpha.size, ink), (0, 0), alpha); return t

def puffy(alpha, bg, ink):
    """Puffy paint: rendered at 2x from a smoothed height map, lit from the upper
    left (diffuse + a small gloss), with a soft shadow on the ground."""
    s0 = alpha.size[0]; s = s0 * 2
    al = alpha.resize((s, s), Image.LANCZOS)
    A = np.array(al.filter(ImageFilter.GaussianBlur(1))).astype(float) / 255
    h = np.array(al.filter(ImageFilter.GaussianBlur(s * 0.006))).astype(float) / 255
    h = h / max(h.max(), 1e-6)
    gy, gx = np.gradient(h)
    k = s * 0.035
    nx, ny, nz = -gx * k, -gy * k, np.ones_like(h)
    n = np.sqrt(nx ** 2 + ny ** 2 + nz ** 2); nx, ny, nz = nx / n, ny / n, nz / n
    lx, ly, lz = -0.45, -0.55, 0.70
    diff = np.clip(nx * lx + ny * ly + nz * lz, 0, 1)
    gloss = diff ** 24 * 0.55
    base = np.array(ink, float)
    col = base[None, None, :] * (0.62 + 0.55 * diff[..., None]) + 255 * gloss[..., None]
    t = np.array(Image.new("RGB", (s, s), bg)).astype(float)
    sh = np.array(al.filter(ImageFilter.GaussianBlur(s * 0.012))).astype(float) / 255
    sh = np.roll(np.roll(sh, int(s * 0.014), 0), int(s * 0.006), 1)
    t = t * (1 - 0.28 * sh[..., None])
    t = t * (1 - A[..., None]) + np.clip(col, 0, 255) * A[..., None]
    return Image.fromarray(t.astype(np.uint8)).resize((s0, s0), Image.LANCZOS)

def rounded(img):
    s = img.size[0]; m = Image.new("L", (s, s), 0)
    ImageDraw.Draw(m).rounded_rectangle((0, 0, s - 1, s - 1), radius=int(s * 0.22), fill=255)
    out = Image.new("RGBA", (s, s), (0, 0, 0, 0)); out.paste(img, (0, 0), m); return out

T, G, L, COLS = 320, 36, 46, 6
tiles = []
for name, g, k, how in GLYPHS:
    for style in ("flat", "puffy"):
        for bg, ink in PAIRS:
            al = place(g, k + (6 if style == "puffy" else 0), how, T, 0.72)
            tiles.append(rounded(flat(al, bg, ink) if style == "flat" else puffy(al, bg, ink)))
rows = (len(tiles) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (240, 236, 229)); d = ImageDraw.Draw(sheet); f = font(32)
for i, t in enumerate(tiles):
    t.save(os.path.expanduser(f"~/Desktop/choir-flowers/puff-{i + 1}.png"))
    r, c = divmod(i, COLS); x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 8), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-puff.jpg"), quality=90)
print("ok", sheet.size)
