#!/usr/bin/env python3
"""Build Atkinson Hyperlegible Mono Tight: the same letters in a narrower cell.

Atkinson Hyperlegible Mono sets each glyph in a 632-unit cell. This takes
static Regular, Bold, Italic and Bold Italic instances from the variable
fonts and narrows the cell (600 by default, the width of Menlo) by trimming
the side bearings. No outline is scaled, so letter shapes are unchanged.

The source carries no Reserved Font Name; the result is renamed and ships
under the same SIL Open Font License (AtkinsonHyperlegibleMonoTight-OFL.txt).

    pip install fonttools
    brew install --cask font-atkinson-hyperlegible-mono
    python3 build-atkinson-tight.py ~/Library/Fonts OUT_DIR [600]
"""
import sys
from fontTools.ttLib import TTFont
from fontTools.varLib.instancer import instantiateVariableFont
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.pens.transformPen import TransformPen
from fontTools.pens.recordingPen import DecomposingRecordingPen

FAMILY = "Atkinson Hyperlegible Mono Tight"
JOBS = [
    ("AtkinsonHyperlegibleMono[wght].ttf", 400, "Regular"),
    ("AtkinsonHyperlegibleMono[wght].ttf", 700, "Bold"),
    ("AtkinsonHyperlegibleMono-Italic[wght].ttf", 400, "Italic"),
    ("AtkinsonHyperlegibleMono-Italic[wght].ttf", 700, "Bold Italic"),
]


def build(src_dir, out_dir, new_adv):
    for fname, wght, style in JOBS:
        f = instantiateVariableFont(TTFont(f"{src_dir}/{fname}"), {"wght": wght})
        glyphs, glyf, hmtx = f.getGlyphSet(), f["glyf"], f["hmtx"]
        cell = max(hmtx[g][0] for g in f.getGlyphOrder())
        dx = (new_adv - cell) / 2  # trim both side bearings equally

        # Redraw every glyph shifted by dx. Composites are flattened so a
        # base and its accent move together.
        shifted = {}
        for g in f.getGlyphOrder():
            rec = DecomposingRecordingPen(glyphs)
            glyphs[g].draw(rec)
            pen = TTGlyphPen(None)
            rec.replay(TransformPen(pen, (1, 0, 0, 1, dx, 0)))
            shifted[g] = pen.glyph()
        for g, glyph in shifted.items():
            advance = hmtx[g][0]
            glyf[g] = glyph
            glyph.recalcBounds(glyf)
            hmtx[g] = (round(advance * new_adv / cell), getattr(glyph, "xMin", 0) or 0)

        # Hinting and variation tables describe the old outlines.
        for table in ("fpgm", "prep", "cvt ", "hdmx", "VDMX", "LTSH", "STAT"):
            if table in f:
                del f[table]

        bold, italic = "Bold" in style, "Italic" in style
        os2 = f["OS/2"]
        os2.xAvgCharWidth = new_adv
        os2.usWeightClass = wght
        os2.fsSelection = (os2.fsSelection & ~0b1100001) | (0x20 if bold else 0) | (0x01 if italic else 0) \
            | (0 if bold or italic else 0x40)
        f["head"].macStyle = (1 if bold else 0) | (2 if italic else 0)
        f["hhea"].advanceWidthMax = max(hmtx[g][0] for g in f.getGlyphOrder())

        ps = "AtkinsonHyperlegibleMonoTight-" + style.replace(" ", "")
        name = f["name"]
        for record in list(name.names):
            if record.nameID in (16, 17, 21, 22, 25):
                name.removeNames(nameID=record.nameID)
        for name_id, value in ((1, FAMILY), (2, style), (3, f"{FAMILY} {style} slab"), (4, f"{FAMILY} {style}"), (6, ps)):
            name.setName(value, name_id, 3, 1, 0x409)
            name.setName(value, name_id, 1, 0, 0)

        f.save(f"{out_dir}/{ps}.ttf")
        print(f"{ps}: cell {cell} -> {new_adv}")


if __name__ == "__main__":
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    build(sys.argv[1], sys.argv[2], int(sys.argv[3]) if len(sys.argv) > 3 else 600)
