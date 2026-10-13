# choir logo round 18, 26.10.12: jeffrey: "more rose colors", "more yum /
# licious", "more puffed out". The upright A4 flower, thicker, deeper relief and
# more shine. Rows: roses on pinks, roses on warm pales, puffed blues, crosses. 1–16.
# python3 marketing/choir/logo/roses.py
import os
from PIL import Image, ImageDraw
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "nine.py")).read().split("\n# jeffrey:")[0])   # relief(), place(), glassy(), rounded(), hls, a4
def candy(ink, bg, k=6, depth=0.95, gloss=0.75, glass=False):
    img = relief(place(a4, k, "mass", 320, 0.72), bg, ink, depth, paper=not glass, gloss=gloss)
    return glassy(img) if glass else rounded(img)
RASP, CHERRY, STRAWB, CORAL, MAUVE = hls(340, 0.42, 0.62), hls(352, 0.40, 0.70), hls(345, 0.62, 0.70), hls(8, 0.58, 0.68), hls(330, 0.42, 0.28)
BLUSH, PINK, CREAMY, OATY, BUTTER = hls(345, 0.92, 0.55), hls(340, 0.88, 0.60), hls(40, 0.94, 0.45), hls(38, 0.87, 0.28), hls(48, 0.89, 0.60)
NAVY, CERUL, PALEB = hls(220, 0.30, 0.50), hls(208, 0.42, 0.60), hls(212, 0.91, 0.45)
VARS = [(RASP, BLUSH), (CHERRY, PINK), (STRAWB, BLUSH), (MAUVE, PINK),
        (RASP, CREAMY), (CORAL, CREAMY), (CHERRY, OATY), (STRAWB, BUTTER),
        (NAVY, PALEB), (CERUL, PALEB), (NAVY, CREAMY), (CERUL, BUTTER),
        (RASP, PALEB), (NAVY, BLUSH), (CORAL, PALEB), (CERUL, BLUSH)]
T, G, L, COLS = 320, 36, 46, 4
rows = (len(VARS) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (242, 238, 232)); d = ImageDraw.Draw(sheet); f = font(32)
for i, (ink, bg) in enumerate(VARS):
    t = candy(ink, bg); t.save(os.path.expanduser(f"~/Desktop/choir-flowers/rose-{i + 1}.png"))
    r, c = divmod(i, COLS); x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 8), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-roses.jpg"), quality=92)
print("ok", sheet.size)
