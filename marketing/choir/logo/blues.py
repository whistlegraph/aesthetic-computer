# choir logo round 17, 26.10.12: jeffrey: "lets explore more blues". nine.py's
# flower (upright A4, weight 4, subtle relief, paper). Rows: navy depth on oat,
# blue hues on oat, blue on pale blue, blue on other pale grounds. 1–16.
# python3 marketing/choir/logo/blues.py
import os
from PIL import Image, ImageDraw
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "nine.py")).read().split("\n# jeffrey:")[0])   # one(), hls, OAT, CREAM
NAVY = lambda l: hls(220, l, 0.45)
VARS = [dict(ink=NAVY(l)) for l in (0.18, 0.24, 0.30, 0.38)] + \
       [dict(ink=hls(h, 0.30, s)) for h, s in ((195, 0.45), (210, 0.55), (215, 0.25), (235, 0.40))] + \
       [dict(ink=NAVY(0.26), bg=hls(212, l, s)) for l, s in ((0.92, 0.35), (0.88, 0.30), (0.84, 0.28), (0.80, 0.22))] + \
       [dict(ink=NAVY(0.26), bg=b) for b in (CREAM, hls(48, 0.89, 0.45), hls(30, 0.84, 0.10), hls(350, 0.89, 0.35))]
T, G, L, COLS = 320, 36, 46, 4
rows = (len(VARS) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (240, 236, 229)); d = ImageDraw.Draw(sheet); f = font(32)
for i, v in enumerate(VARS):
    t = one(**v); t.save(os.path.expanduser(f"~/Desktop/choir-flowers/blue-{i + 1}.png"))
    r, c = divmod(i, COLS); x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 8), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-blues.jpg"), quality=92)
print("ok", sheet.size)
