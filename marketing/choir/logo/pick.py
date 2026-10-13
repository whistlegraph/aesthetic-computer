# choir logo round 2, 26.10.12: Fía liked A4 and E4 from sheet.py; each in the
# app's colorways (cream like today's icon, blue, red, ink), numbered 1–8.
# python3 marketing/choir/logo/pick.py
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from PIL import Image, ImageDraw
import importlib.util
spec = importlib.util.spec_from_file_location("sheet", os.path.join(os.path.dirname(__file__), "sheet.py"))
HERE = os.path.dirname(os.path.abspath(__file__))
src = open(os.path.join(HERE, "sheet.py")).read().split("files = sorted")[0]   # helpers only
exec(src)
PICKS = [("A4", "IMG_8396-136.png"), ("E4", "IMG_8398-171.png")]
WAYS = [(CREAM, INK), ((61, 120, 220), (255, 255, 255)), ((207, 64, 64), (255, 250, 240)), (INK, CREAM)]
T, G, L = 420, 48, 56
W = len(WAYS) * T + (len(WAYS) + 1) * G; H = len(PICKS) * (T + L) + (len(PICKS) + 1) * G
sheet = Image.new("RGB", (W, H), (226, 219, 204)); d = ImageDraw.Draw(sheet); f = font(40)
n = 0
for r, (addr, name) in enumerate(PICKS):
    a = cutout(os.path.join(HERE, "flowers", name))
    for c, (bg, fg) in enumerate(WAYS):
        n += 1; t = tile(a, bg, fg, T)
        t.save(os.path.expanduser(f"~/Desktop/choir-flowers/pick-{n}-{addr}.png"))
        x = G + c * (T + G); y = G + r * (T + L + G)
        sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 12), str(n), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-pick.jpg"), quality=90)
print("ok", sheet.size)
