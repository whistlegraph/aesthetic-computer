# choir logo round 11, 26.10.12: jeffrey: "lighter backgrounds, colored inks".
# shadow.py's flower (smaller, re-centered) in deep colored ink on pale grounds:
# 12 same-family pairs around the wheel, then 6 cross pairs. 1–18.
# python3 marketing/choir/logo/inks.py
import os, colorsys
from PIL import Image, ImageDraw
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "shadow.py")).read().split("ROWS = [")[0])   # icon(), hls()
HUES = [25, 45, 70, 100, 140, 175, 200, 220, 250, 285, 320, 350]
PAIRS = [(hls(h, 0.87, 0.30), hls(h, 0.24, 0.45)) for h in HUES] + [
    (hls(45, 0.88, 0.35), hls(220, 0.28, 0.50)),   # butter + navy
    (hls(350, 0.88, 0.30), hls(140, 0.22, 0.40)),  # blush + forest
    (hls(100, 0.86, 0.22), hls(350, 0.26, 0.50)),  # sage + oxblood
    (hls(200, 0.88, 0.25), hls(25, 0.30, 0.55)),   # sky + rust
    (hls(30, 0.86, 0.18), hls(250, 0.28, 0.40)),   # oat + plum-indigo
    (hls(285, 0.88, 0.22), hls(70, 0.24, 0.40))]   # lilac + olive
T, G, L, COLS = 320, 36, 46, 6
rows = (len(PAIRS) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (240, 236, 229)); d = ImageDraw.Draw(sheet); f = font(32)
for i, (bg, ink) in enumerate(PAIRS):
    t = icon(bg, ink, None, None, T)
    t.save(os.path.expanduser(f"~/Desktop/choir-flowers/ink-{i + 1}.png"))
    r, c = divmod(i, COLS); x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 8), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-inks.jpg"), quality=90)
print("ok", sheet.size)
