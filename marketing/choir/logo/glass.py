# choir logo round 15, 26.10.12: jeffrey: "a more glassy surface they are on".
# puff3.py's subtle relief flowers on a glass tile: light falling from the top,
# a soft sheen across the upper left, a bright rim on top fading to a darker
# lower edge. 1–12.  python3 marketing/choir/logo/glass.py
import os
import numpy as np
from PIL import Image, ImageDraw, ImageFilter
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "puff3.py")).read().split("T, G, L, COLS = 320, 36")[0])   # relief(), place(), GLYPHS, MUTED

def glassy(img):
    """img: the square RGB tile (flower already on it). Returns a rounded RGBA glass icon."""
    s = img.size[0]
    t = np.array(img).astype(float)
    y = np.linspace(0, 1, s)[:, None, None]
    t = t * (1.06 - 0.12 * y) + 10 * (1 - y)                        # light from the top
    # a soft sheen: a big blurred ellipse across the upper left
    sheen = Image.new("L", (s, s), 0)
    ImageDraw.Draw(sheen).ellipse((-0.35 * s, -0.55 * s, 0.95 * s, 0.42 * s), fill=255)
    sheen = np.array(sheen.filter(ImageFilter.GaussianBlur(s * 0.08))).astype(float)[..., None] / 255
    t = t * (1 - 0.18 * sheen) + 255 * 0.18 * sheen
    # the rim: bright along the top edge, a touch darker along the bottom
    m = Image.new("L", (s, s), 0); ImageDraw.Draw(m).rounded_rectangle((0, 0, s - 1, s - 1), radius=int(s * 0.22), fill=255)
    inner = m.filter(ImageFilter.MinFilter(5)).filter(ImageFilter.GaussianBlur(1.5))
    rim = (np.array(m).astype(float) - np.array(inner).astype(float)).clip(0, 255)[..., None] / 255
    up = (1 - y) ** 1.5
    t = t * (1 - rim * up * 0.75) + 255 * rim * up * 0.75
    t = t * (1 - rim * y * 0.25)
    inner_glow = np.array(m.filter(ImageFilter.MinFilter(int(s * 0.03) | 1)).filter(ImageFilter.GaussianBlur(s * 0.03))).astype(float)[..., None] / 255
    t = t * (0.93 + 0.07 * inner_glow)                              # depth at the edges
    out = Image.new("RGBA", (s, s), (0, 0, 0, 0)); out.paste(Image.fromarray(t.clip(0, 255).astype(np.uint8)), (0, 0), m)
    return out

T, G, L, COLS = 320, 36, 46, 6
LINE = {"new": 6, "seven": 4}
tiles = [glassy(relief(place(g, LINE[name], how, T, 0.72), bg, ink, paper=False)) for name, g, k, how in GLYPHS for bg, ink in MUTED]
rows = (len(tiles) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (240, 236, 229)); d = ImageDraw.Draw(sheet); f = font(32)
for i, t in enumerate(tiles):
    t.save(os.path.expanduser(f"~/Desktop/choir-flowers/glass-{i + 1}.png"))
    r, c = divmod(i, COLS); x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 8), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-glass.jpg"), quality=92)
print("ok", sheet.size)
