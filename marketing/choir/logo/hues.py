# choir logo round 9, 26.10.12: jeffrey: "across those two sets … various hues".
# The gray-brown's muted softness (taupe.py) swept around the hue wheel, light
# (ink line) and deep (cream line). 1–24.  python3 marketing/choir/logo/hues.py
import os, colorsys
from PIL import Image, ImageDraw
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "finals.py")).read().split("VARS = [")[0])
PAPER = (250, 246, 238)
HUES = [25, 45, 70, 100, 140, 175, 200, 220, 250, 285, 320, 350]   # degrees, warm first
def hls(h, l, s): return tuple(int(v * 255) for v in colorsys.hls_to_rgb(h / 360, l, s))
WAYS = [(hls(h, 0.72, 0.22), INK) for h in HUES] + [(hls(h, 0.36, 0.22), PAPER) for h in HUES]
T, G, L, COLS = 300, 34, 46, 6
rows = (len(WAYS) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (236, 231, 222)); d = ImageDraw.Draw(sheet); f = font(32)
for i, (bg, fg) in enumerate(WAYS):
    t = make_from(new, k=5, how="dot", scale=0.8, bg=bg, fg=fg, size=T)
    t.save(os.path.expanduser(f"~/Desktop/choir-flowers/hue-{i + 1}.png"))
    r, c = divmod(i, COLS); x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 8), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-hues.jpg"), quality=90)
print("ok", sheet.size)
