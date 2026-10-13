# choir app icon, 26.10.12 (final for 1.0): jeffrey: "2 from flowers-beige" =
# gens/beige-linen.png — gpt-image-2.5-flare, Fía's flower as glossy navy puffy
# paint on soft linen beige. Already full-bleed with the flower large; used as is.
# python3 marketing/choir/logo/appicon-final.py
import os
from PIL import Image
HERE = os.path.dirname(os.path.abspath(__file__))
icon = Image.open(os.path.join(HERE, "gens/beige-linen.png")).convert("RGB").resize((1024, 1024), Image.LANCZOS)
icon.save(os.path.join(HERE, "../../../apple/choir/Resources/Assets.xcassets/AppIcon.appiconset/icon-1024.png"))
print("ok", icon.size)
