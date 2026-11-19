#!/usr/bin/env python3
"""Bakes Resources/chunky8.ttf from rax/chunky-font.mjs (MatrixChunky8.bdf).

One square per lit pixel run, 100 units a pixel on an 800-unit em, so the face
is exact at whole multiples of 8 device pixels. Needs fonttools:

    python3 -m venv /tmp/ft && /tmp/ft/bin/pip install fonttools
    /tmp/ft/bin/python toolchain/macos/blueberry-wallpaper/bake-chunky8.py
"""
import json, os, re
from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen

HERE = os.path.dirname(os.path.abspath(__file__))
SOURCE = os.path.join(HERE, "../../../rax/chunky-font.mjs")
OUT = os.path.join(HERE, "Resources/chunky8.ttf")
PX = 100

text = open(SOURCE, encoding="utf8").read()
glyphs = json.loads(re.search(r"export const glyphs = (\{.*\});", text).group(1))
# Marks the wallpaper writes that the BDF lacks: [width, height, xoff, yoff, advance, rows].
glyphs["·"] = [1, 1, 1, 4, 4, [128]]            # ·
glyphs["•"] = [2, 2, 0, 3, 3, [192, 192]]        # •
glyphs["…"] = [5, 1, 0, 1, 6, [168]]             # …
glyphs["–"] = [3, 1, 0, 4, 4, [224]]             # –
glyphs["—"] = [5, 1, 0, 4, 6, [248]]             # —


def outline(width, height, xoff, yoff, rows):
    pen = TTGlyphPen(None)
    # The BDF sits letters one pixel above its baseline; descenders reach it.
    bottom = yoff - 1
    for r, bits in enumerate(rows):
        y = bottom + height - 1 - r
        x = 0
        while x < width:
            if bits & (0x80 >> x):
                start = x
                while x < width and bits & (0x80 >> x):
                    x += 1
                x0, x1 = (xoff + start) * PX, (xoff + x) * PX
                y0, y1 = y * PX, (y + 1) * PX
                pen.moveTo((x0, y0)); pen.lineTo((x0, y1)); pen.lineTo((x1, y1)); pen.lineTo((x1, y0))
                pen.closePath()
            else:
                x += 1
    return pen.glyph()


names, cmap, outlines, metrics = [".notdef"], {}, {".notdef": TTGlyphPen(None).glyph()}, {".notdef": (4 * PX, 0)}
for ch, (w, h, xoff, yoff, advance, rows) in sorted(glyphs.items()):
    name = "u%04X" % ord(ch)
    names.append(name)
    cmap[ord(ch)] = name
    outlines[name] = outline(w, h, xoff, yoff, rows)
    metrics[name] = (advance * PX, xoff * PX)

fb = FontBuilder(8 * PX, isTTF=True)
fb.setupGlyphOrder(names)
fb.setupCharacterMap(cmap)
fb.setupGlyf(outlines)
fb.setupHorizontalMetrics(metrics)
fb.setupHorizontalHeader(ascent=8 * PX, descent=-PX, lineGap=0)
fb.setupNameTable({"familyName": "MatrixChunky8", "styleName": "Regular"})
fb.setupOS2(sTypoAscender=8 * PX, sTypoDescender=-PX, sTypoLineGap=0, usWinAscent=8 * PX, usWinDescent=PX,
            sxHeight=5 * PX, sCapHeight=7 * PX)
fb.setupPost(isFixedPitch=0)
fb.save(OUT)
print("baked", OUT, len(names) - 1, "glyphs")
