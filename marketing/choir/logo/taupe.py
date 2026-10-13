# choir logo round 7, 26.10.12: jeffrey: "can the logo sit on a graybrownish".
# #10 of finals.py (Fía's IMG_8410 flower) on gray-browns, light to dark. 1–8.
# python3 marketing/choir/logo/taupe.py
import os
from PIL import Image, ImageDraw
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "finals.py")).read().split("VARS = [")[0])
SHADES = [((214, 204, 190), INK), ((196, 184, 168), INK), ((176, 163, 147), INK), ((158, 144, 128), INK),
          ((139, 125, 110), CREAM), ((118, 104, 92), CREAM), ((96, 84, 74), CREAM), ((72, 63, 56), CREAM)]
T, G, L, COLS = 400, 44, 54, 4
W = COLS * T + (COLS + 1) * G; H = 2 * (T + L) + 3 * G
sheet = Image.new("RGB", (W, H), (236, 231, 222)); d = ImageDraw.Draw(sheet); f = font(38)
for i, (bg, fg) in enumerate(SHADES):
    t = make_from(new, k=5, how="dot", scale=0.8, bg=bg, fg=fg)
    t.save(os.path.expanduser(f"~/Desktop/choir-flowers/taupe-{i + 1}.png"))
    r, c = divmod(i, COLS); x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 10), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-taupe.jpg"), quality=90)
print("ok", sheet.size)
