# choir logo round 10, 26.10.12: jeffrey: "a subtle dropshadow or sharp dropshadow
# colored shadow on logo", "better vertical centering", "a tad smaller".
# Fía's IMG_8410 flower at 0.72 of the tile, centered on her dot side to side and
# on its outline (lifted a hair) top to bottom. Rows: colors from hues.py/taupe.py;
# columns: no shadow, soft, sharp in a deeper shade, sharp in an accent. 1–20.
# python3 marketing/choir/logo/shadow.py
import os, colorsys
from PIL import Image, ImageDraw, ImageFilter
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "finals.py")).read().split("VARS = [")[0])
PAPER = (250, 246, 238)
def hls(h, l, s): return tuple(int(v * 255) for v in colorsys.hls_to_rgb(h / 360, l, s))
def deeper(c, f=0.72): return tuple(int(v * f) for v in c)

def icon(bg, fg, shadow=None, accent=None, size=400, scale=0.72):
    a = weight(new, 5); a = a.crop(a.getbbox()); a.thumbnail((int(size * scale), int(size * scale)), Image.LANCZOS)
    ax, _ = anchor(a, "dot")
    x, y = int(size / 2 - ax * a.width), int(size / 2 - 0.515 * a.height)
    t = Image.new("RGB", (size, size), bg)
    if shadow == "soft":
        s = Image.new("L", (size, size), 0); s.paste(a, (x, y + int(size * 0.02)))
        s = s.filter(ImageFilter.GaussianBlur(size * 0.018)).point(lambda v: int(v * 0.35))
        t.paste(Image.new("RGB", (size, size), deeper(bg, 0.45)), (0, 0), s)
    elif shadow in ("deep", "accent"):
        off = int(size * 0.022)
        t.paste(Image.new("RGB", a.size, deeper(bg) if shadow == "deep" else accent), (x + off, y + off), a)
    t.paste(Image.new("RGB", a.size, fg), (x, y), a)
    m = Image.new("L", (size, size), 0); ImageDraw.Draw(m).rounded_rectangle((0, 0, size - 1, size - 1), radius=int(size * 0.22), fill=255)
    out = Image.new("RGBA", (size, size), (0, 0, 0, 0)); out.paste(t, (0, 0), m); return out

ROWS = [((196, 184, 168), INK, (207, 64, 64)),        # gray-brown (taupe 2)
        (hls(100, 0.72, 0.22), INK, (207, 64, 64)),    # sage (hues 4)
        (hls(350, 0.72, 0.22), INK, (61, 120, 220)),   # dusty rose (hues 12)
        (hls(200, 0.36, 0.22), PAPER, (250, 214, 92)), # slate (hues 19)
        ((118, 104, 92), PAPER, (244, 196, 206))]      # umber (taupe 6)
T, G, L, COLS = 360, 40, 50, 4
W = COLS * T + (COLS + 1) * G; H = len(ROWS) * (T + L) + (len(ROWS) + 1) * G
sheet = Image.new("RGB", (W, H), (236, 231, 222)); d = ImageDraw.Draw(sheet); f = font(34)
n = 0
for r, (bg, fg, acc) in enumerate(ROWS):
    for c, sh in enumerate((None, "soft", "deep", "accent")):
        n += 1; t = icon(bg, fg, sh, acc, T)
        t.save(os.path.expanduser(f"~/Desktop/choir-flowers/shadow-{n}.png"))
        x = G + c * (T + G); y = G + r * (T + L + G)
        sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 9), str(n), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-shadow.jpg"), quality=90)
print("ok", sheet.size)
