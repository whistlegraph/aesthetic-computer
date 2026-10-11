#!/usr/bin/env python3
"""Regenerate pals_sprite.h from fedac/plymouth/pals.png (alpha only, <=192 px wide)."""
from pathlib import Path
from PIL import Image

here = Path(__file__).resolve().parent
src = here.parents[2] / "fedac" / "plymouth" / "pals.png"
im = Image.open(src).convert("RGBA")
W = 192
if im.width > W:
    im = im.resize((W, int(im.height * W / im.width)), Image.LANCZOS)
a = im.split()[3]
w, h = a.size
data = a.tobytes()
out = here / "pals_sprite.h"
with out.open("w") as f:
    f.write("// pals_sprite.h — generated from fedac/plymouth/pals.png (alpha channel only).\n")
    f.write("// Regenerate: python3 gen-sprite.py. Do not edit by hand.\n")
    f.write(f"#define PALS_W {w}\n#define PALS_H {h}\n")
    f.write("static const unsigned char PALS_ALPHA[PALS_W * PALS_H] = {\n")
    for i in range(0, len(data), 24):
        f.write("  " + ",".join(str(b) for b in data[i:i + 24]) + ",\n")
    f.write("};\n")
print(f"wrote {out} ({w}x{h})")
