# choir launch screen, 26.10.12: jeffrey: "a flash of unstyled content … it should
# instead show new logo with nice background behind it". The navy flower cut out
# of the icon (gens/beige-linen.png) as LaunchLogo, on the app's paper color.
# python3 marketing/choir/logo/launch.py
import os
import numpy as np
from PIL import Image, ImageFilter
HERE = os.path.dirname(os.path.abspath(__file__))
ASSETS = os.path.join(HERE, "../../../apple/choir/Resources/Assets.xcassets")
im = Image.open(os.path.join(HERE, "gens/beige-linen.png")).convert("RGB")
a = np.array(im).astype(int); r, g, b = a[..., 0], a[..., 1], a[..., 2]
m = ((b - r) > 12) | (a.mean(-1) < 120)                          # navy paint (and its dark edges)
mask = Image.fromarray((m * 255).astype(np.uint8)).filter(ImageFilter.MaxFilter(9)).filter(ImageFilter.MinFilter(9))  # fill the highlights
alpha = mask.filter(ImageFilter.GaussianBlur(1.6))
logo = im.convert("RGBA"); logo.putalpha(alpha)
logo = logo.crop(alpha.getbbox())
side = max(logo.size); sq = Image.new("RGBA", (side, side), (0, 0, 0, 0)); sq.paste(logo, ((side - logo.width) // 2, (side - logo.height) // 2))
d = os.path.join(ASSETS, "LaunchLogo.imageset"); os.makedirs(d, exist_ok=True)
sq.resize((540, 540), Image.LANCZOS).save(os.path.join(d, "launch-logo@3x.png"))   # 180 pt
sq.resize((360, 360), Image.LANCZOS).save(os.path.join(d, "launch-logo@2x.png"))
open(os.path.join(d, "Contents.json"), "w").write('{"images":[{"idiom":"universal","filename":"launch-logo@2x.png","scale":"2x"},{"idiom":"universal","filename":"launch-logo@3x.png","scale":"3x"}],"info":{"author":"xcode","version":1}}\n')
# Theme.desk (Sources/Theme.swift), so the launch hands straight into the room.
c = os.path.join(ASSETS, "LaunchBackground.colorset"); os.makedirs(c, exist_ok=True)
open(os.path.join(c, "Contents.json"), "w").write('{"colors":[{"idiom":"universal","color":{"color-space":"srgb","components":{"red":"0.886","green":"0.859","blue":"0.800","alpha":"1.000"}}}],"info":{"author":"xcode","version":1}}\n')
print("ok", sq.size)
