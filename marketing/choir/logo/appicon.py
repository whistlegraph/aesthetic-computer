# choir app icon, 26.10.12: jeffrey picked blues.py #4 — Fía's upright A4 flower,
# weight 4, subtle relief on paper, navy hls(220, .38, .45) on oat hls(40, .87, .25).
# Full-bleed 1024 (iOS rounds it). python3 marketing/choir/logo/appicon.py
import os
from PIL import Image
HERE = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(HERE, "nine.py")).read().split("\n# jeffrey:")[0])
S = 1024
# weight() thickens at a fixed 900 px, so k=4 matches the 320 px tile.
icon = relief(place(a4, 4, "mass", S, 0.72), OAT, hls(220, 0.38, 0.45), 0.45, paper=True).convert("RGB")
out = os.path.join(HERE, "../../../apple/choir/Resources/Assets.xcassets/AppIcon.appiconset/icon-1024.png")
icon.save(out); icon.save(os.path.expanduser("~/Desktop/choir-flowers/appicon-1024.png")); print("ok", icon.size)
