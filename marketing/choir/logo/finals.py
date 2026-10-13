# choir logo round 5, 26.10.12: jeffrey picked #7 of three.py (A4, weight 7,
# centered on ink mass) and #11 of thick.py (Fía's new IMG_8410 flower, weight 5).
# Each at four weights around it × two sizes. 1–8 = #7's family, 9–16 = #11's.
# python3 marketing/choir/logo/finals.py
import os
from PIL import Image, ImageDraw
HERE = os.path.dirname(os.path.abspath(__file__))
src = open(os.path.join(HERE, "three.py")).read()
exec(src.split("# rows: line weight")[0])          # a4, weight(), anchor(), make(), tile helpers
new = photo_cutout(os.path.join(HERE, "flowers-new-8410.jpg"))

def make_from(src_alpha, **kw):
    global a4
    keep = a4; a4 = src_alpha
    try: return make(**kw)
    finally: a4 = keep

VARS = [(a4, dict(k=k, how="mass", scale=s)) for s in (0.78, 0.86) for k in (6, 7, 8, 9)] + \
       [(new, dict(k=k, how="dot", scale=s)) for s in (0.78, 0.86) for k in (4, 5, 6, 7)]
T, G, L, COLS = 400, 44, 54, 4
rows = (len(VARS) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (226, 219, 204)); d = ImageDraw.Draw(sheet); f = font(38)
for i, (a, v) in enumerate(VARS):
    r, c = divmod(i, COLS); t = make_from(a, **v)
    t.save(os.path.expanduser(f"~/Desktop/choir-flowers/final-{i + 1}.png"))
    x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 10), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-finals.jpg"), quality=90)
print("ok", sheet.size)
