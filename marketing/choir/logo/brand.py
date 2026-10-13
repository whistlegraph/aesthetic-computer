# choir brand pipeline, 26.10.12. ONE master → every logo surface. See BRAND.md.
# jeffrey: "next time we do that it does it for us / has a md file or pipeline
# for logo placements". Change MASTER (or replace its file), run this, rebuild.
#   python3 marketing/choir/logo/brand.py
import os, json
import numpy as np
from PIL import Image, ImageDraw, ImageFilter, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "../../.."))
MASTER = os.path.join(HERE, "gens/beige-linen.png")      # the icon art: full-bleed square, flower on its ground
DESK = (226, 219, 204)                                    # Theme.desk (Sources/Theme.swift) and --desk on choir.ac
INK = (28, 24, 22)
ASSETS = os.path.join(REPO, "apple/choir/Resources/Assets.xcassets")
WEB = os.path.join(REPO, "system/public/choir")
OUT = os.path.expanduser("~/Desktop/choir-brand"); os.makedirs(OUT, exist_ok=True)

def font(px):
    for f in ["/System/Library/Fonts/Supplemental/AmericanTypewriter.ttc", "/System/Library/Fonts/Supplemental/Georgia.ttf"]:
        try: return ImageFont.truetype(f, px)
        except OSError: pass
    return ImageFont.load_default()

def cutout(icon):
    """The flower alone, on transparency: the navy paint (blue over red) with its
    highlights filled back in, soft edged."""
    a = np.array(icon).astype(int); r, b = a[..., 0], a[..., 2]
    m = ((b - r) > 12) | (a.mean(-1) < 120)
    mask = Image.fromarray((m * 255).astype(np.uint8)).filter(ImageFilter.MaxFilter(9)).filter(ImageFilter.MinFilter(9))
    out = icon.convert("RGBA"); out.putalpha(mask.filter(ImageFilter.GaussianBlur(1.6)))
    out = out.crop(out.getchannel("A").getbbox())
    s = max(out.size); sq = Image.new("RGBA", (s, s), (0, 0, 0, 0)); sq.paste(out, ((s - out.width) // 2, (s - out.height) // 2))
    return sq

def imageset(name, img, pts):
    d = os.path.join(ASSETS, f"{name}.imageset"); os.makedirs(d, exist_ok=True)
    for f in os.listdir(d):
        if f.endswith(".png"): os.remove(os.path.join(d, f))
    imgs = []
    for scale in (2, 3):
        fn = f"{name.lower()}@{scale}x.png"; img.resize((pts * scale, pts * scale), Image.LANCZOS).save(os.path.join(d, fn))
        imgs.append({"idiom": "universal", "filename": fn, "scale": f"{scale}x"})
    json.dump({"images": imgs, "info": {"author": "xcode", "version": 1}}, open(os.path.join(d, "Contents.json"), "w"))

icon = Image.open(MASTER).convert("RGB").resize((1024, 1024), Image.LANCZOS)
flower = cutout(icon)
done = []

# iOS: app icon, launch screen, the in-app logo (header, spinner, badges)
icon.save(os.path.join(ASSETS, "AppIcon.appiconset/icon-1024.png")); done.append("app icon")
imageset("LaunchLogo", flower, 180); done.append("launch screen logo")
c = os.path.join(ASSETS, "LaunchBackground.colorset"); os.makedirs(c, exist_ok=True)
json.dump({"colors": [{"idiom": "universal", "color": {"color-space": "srgb", "components": {k: f"{v / 255:.3f}" for k, v in zip(("red", "green", "blue"), DESK)} | {"alpha": "1.000"}}}],
           "info": {"author": "xcode", "version": 1}}, open(os.path.join(c, "Contents.json"), "w")); done.append("launch background")
imageset("Logo", flower, 160); done.append("in-app logo (SingingMark, FlowerSpinner)")
imageset("Mark", flower, 120); done.append("template badge (Image(\"Mark\"))")

# Web: favicon, home-screen icon, splash logo, link preview
icon.resize((512, 512), Image.LANCZOS).save(os.path.join(WEB, "icon.png")); done.append("web icon.png (apple-touch)")
flower.resize((64, 64), Image.LANCZOS).save(os.path.join(WEB, "favicon.png")); done.append("web favicon.png")
flower.resize((640, 640), Image.LANCZOS).save(os.path.join(WEB, "logo.png")); done.append("web splash logo.png")
flower.resize((256, 256), Image.LANCZOS).save(os.path.join(WEB, "mark.png")); done.append("web mark.png (privacy/terms/support header, web-room @choir badge)")
og = Image.new("RGB", (1200, 630), DESK); d = ImageDraw.Draw(og)
f = flower.resize((420, 420), Image.LANCZOS); og.paste(f, (110, 105), f)
d.text((600, 250), "choir", fill=INK, font=font(96), anchor="lm")
d.text((604, 360), "Comments on culture", fill=INK, font=font(40), anchor="lm")
og.save(os.path.join(WEB, "social.png")); done.append("web social.png (link preview)")

# Hand-offs: Instagram avatar, the master and cutout for anything else
icon.resize((1080, 1080), Image.LANCZOS).save(os.path.join(OUT, "instagram-avatar-1080.png"))
flower.save(os.path.join(OUT, "flower-cutout.png")); icon.save(os.path.join(OUT, "icon-1024.png")); done.append(f"hand-offs in {OUT}")
print("\n".join("✓ " + x for x in done))
