# choir app icon, 26.10.12: jeffrey: "number 2 from the ai ones … for the app icon"
# = gens/juicy-navy.png (gpt-image-2.5-flare puffy paint, navy on pale blue).
# Cropped in so the flower fills ~80% of the icon ("the logo should be larger").
# python3 marketing/choir/logo/appicon-ai.py
import os
import numpy as np
from PIL import Image
HERE = os.path.dirname(os.path.abspath(__file__))
FILL = 0.80
im = Image.open(os.path.join(HERE, "gens/juicy-navy.png")).convert("RGB")
a = np.array(im).astype(int); W = a.shape[1]
ink = (a.mean(-1) < 140) & (a[..., 2] - a[..., 0] > 30)          # the navy line, not its shadow
ys, xs = np.where(ink)
x0, x1, y0, y1 = xs.min(), xs.max(), ys.min(), ys.max()
cx, cy = (x0 + x1) / 2, (y0 + y1) / 2
side = max(x1 - x0, y1 - y0) / FILL
side = min(side, W)
left = int(min(max(cx - side / 2, 0), W - side)); top = int(min(max(cy - side / 2, 0), W - side))
icon = im.crop((left, top, left + int(side), top + int(side))).resize((1024, 1024), Image.LANCZOS)
out = os.path.join(HERE, "../../../apple/choir/Resources/Assets.xcassets/AppIcon.appiconset/icon-1024.png")
icon.save(out); icon.save(os.path.expanduser("~/Desktop/choir-flowers/appicon-ai-1024.png"))
print("ok", (x0, y0, x1, y1), int(side))
