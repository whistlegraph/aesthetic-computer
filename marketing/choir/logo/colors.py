# choir logo round 8, 26.10.12: jeffrey: "test some color variations".
# #10 of finals.py (Fía's IMG_8410 flower) on 16 colorways: earthy, bright, and
# colored lines on cream.  python3 marketing/choir/logo/colors.py
import os
from PIL import Image, ImageDraw
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "finals.py")).read().split("VARS = [")[0])
PAPER = (250, 246, 238)
WAYS = [  # earthy
    ((196, 184, 168), INK), ((176, 186, 158), INK), ((128, 132, 92), PAPER), ((196, 112, 84), PAPER),
    ((214, 168, 160), INK), ((222, 184, 96), INK), ((104, 120, 140), PAPER), ((112, 44, 52), PAPER),
    # brights and deep
    ((61, 120, 220), PAPER), ((207, 64, 64), PAPER), ((44, 86, 60), PAPER), ((186, 172, 214), INK),
    # colored line on cream
    (CREAM, (207, 64, 64)), (CREAM, (61, 120, 220)), (CREAM, (128, 132, 92)), (CREAM, (118, 104, 92))]
T, G, L, COLS = 400, 44, 54, 4
rows = (len(WAYS) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (236, 231, 222)); d = ImageDraw.Draw(sheet); f = font(38)
for i, (bg, fg) in enumerate(WAYS):
    t = make_from(new, k=5, how="dot", scale=0.8, bg=bg, fg=fg)
    t.save(os.path.expanduser(f"~/Desktop/choir-flowers/color-{i + 1}.png"))
    r, c = divmod(i, COLS); x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 10), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-colors.jpg"), quality=90)
print("ok", sheet.size)
