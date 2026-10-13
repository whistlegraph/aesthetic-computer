# choir logo round 6, 26.10.12: jeffrey picked #10 of finals.py (Fía's IMG_8410
# flower, weight 5, 0.78 of the tile, centered on her dot). Rows: size, tilt,
# centering, color. 1–16.  python3 marketing/choir/logo/ten.py
import os
from PIL import Image, ImageDraw
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "finals.py")).read().split("VARS = [")[0])   # make_from(), new, colors

def tilted(alpha, deg):
    return alpha.rotate(deg, resample=Image.BICUBIC, expand=True) if deg else alpha

BASE = dict(k=5, how="dot", scale=0.78)
VARS = [dict(BASE, scale=s) for s in (0.72, 0.78, 0.84, 0.9)] + \
       [("tilt", t) for t in (-12, -6, 6, 12)] + \
       [dict(BASE, how=h) for h in ("box", "mass", "up", "dot")] + \
       [dict(BASE, bg=b, fg=f) for b, f in ((WHITE, INK), (BLUE, WHITE), (RED, (255, 250, 240)), (INK, CREAM))]
T, G, L, COLS = 400, 44, 54, 4
rows = (len(VARS) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (226, 219, 204)); d = ImageDraw.Draw(sheet); f = font(38)
for i, v in enumerate(VARS):
    if isinstance(v, tuple):
        t = make_from(tilted(new, v[1]), **BASE)
    else:
        t = make_from(new, **v)
    r, c = divmod(i, COLS)
    t.save(os.path.expanduser(f"~/Desktop/choir-flowers/ten-{i + 1}.png"))
    x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 10), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-ten.jpg"), quality=90)
print("ok", sheet.size)
