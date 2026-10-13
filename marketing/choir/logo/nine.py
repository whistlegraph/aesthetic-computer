# choir logo round 16, 26.10.12: jeffrey: "variations on 9" — puff3.py #9, the
# upright A4 flower, thin (weight 4), moss on oat, subtle relief with texture.
# Rows: line weight (+ one on glass), relief depth, ink, ground. 1–16.
# python3 marketing/choir/logo/nine.py
import os
from PIL import Image, ImageDraw
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "glass.py")).read().split("\nT, G, L, COLS = 320, 36")[0])   # glassy(), relief(), place()
OAT, MOSS = hls(40, 0.87, 0.25), hls(75, 0.30, 0.35)
def one(k=4, depth=0.45, ink=MOSS, bg=OAT, glass=False):
    img = relief(place(a4, k, "mass", 320, 0.72), bg, ink, depth, paper=not glass)
    return glassy(img) if glass else rounded(img)
# jeffrey: "9 was the winning color combo … vary it with contrast and subtle changes"
# moss = hls(75, .30, .35) on oat = hls(40, .87, .25). Rows: ink depth (contrast),
# ground lightness, ink hue, ground warmth.
VARS = [dict(ink=hls(75, l, 0.35)) for l in (0.20, 0.25, 0.30, 0.36)] + \
       [dict(bg=hls(40, l, 0.25)) for l in (0.80, 0.84, 0.87, 0.91)] + \
       [dict(ink=hls(h, 0.29, s)) for h, s in ((58, 0.40), (68, 0.38), (85, 0.30), (100, 0.28))] + \
       [dict(bg=hls(h, 0.87, s)) for h, s in ((32, 0.32), (45, 0.18), (55, 0.30), (70, 0.16))]
T, G, L, COLS = 320, 36, 46, 4
rows = (len(VARS) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (240, 236, 229)); d = ImageDraw.Draw(sheet); f = font(32)
for i, v in enumerate(VARS):
    t = one(**v); t.save(os.path.expanduser(f"~/Desktop/choir-flowers/nine-{i + 1}.png"))
    r, c = divmod(i, COLS); x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 8), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-nine.jpg"), quality=92)
print("ok", sheet.size)
