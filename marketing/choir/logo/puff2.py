# choir logo round 13, 26.10.12: jeffrey: "a thinner set", "other less primary
# colors". puff.py's puffy finish, thinner, both glyphs, six muted pairs. 1–12.
# python3 marketing/choir/logo/puff2.py
import os
from PIL import Image, ImageDraw
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "puff.py")).read().split("T, G, L, COLS")[0])   # place(), puffy(), rounded(), GLYPHS, hls
MUTED = [(hls(22, 0.88, 0.45), hls(14, 0.40, 0.48)),    # terracotta on peach
         (hls(290, 0.86, 0.12), hls(310, 0.30, 0.30)),  # plum on lilac-gray
         (hls(40, 0.87, 0.25), hls(75, 0.30, 0.35)),    # moss on oat
         (hls(190, 0.87, 0.18), hls(185, 0.28, 0.40)),  # teal on mist
         (hls(345, 0.89, 0.30), hls(335, 0.38, 0.25)),  # mauve on blush
         (hls(38, 0.84, 0.30), hls(36, 0.38, 0.62))]    # ochre on sand
T, G, L, COLS = 320, 36, 46, 6
tiles = [rounded(puffy(place(g, k + 2, how, T, 0.72), bg, ink)) for name, g, k, how in GLYPHS for bg, ink in MUTED]
rows = (len(tiles) + COLS - 1) // COLS
W = COLS * T + (COLS + 1) * G; H = rows * (T + L) + (rows + 1) * G
sheet = Image.new("RGB", (W, H), (240, 236, 229)); d = ImageDraw.Draw(sheet); f = font(32)
for i, t in enumerate(tiles):
    t.save(os.path.expanduser(f"~/Desktop/choir-flowers/puff2-{i + 1}.png"))
    r, c = divmod(i, COLS); x = G + c * (T + G); y = G + r * (T + L + G)
    sheet.paste(t, (x, y), t); d.text((x + T / 2, y + T + 8), str(i + 1), fill=INK, font=f, anchor="mt")
sheet.save(os.path.expanduser("~/Desktop/choir-flowers-puff2.jpg"), quality=90)
print("ok", sheet.size)
