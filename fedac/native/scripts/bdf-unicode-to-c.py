#!/usr/bin/env python3
# bdf-unicode-to-c.py — Extract selected Unicode codepoints from a BDF file
# into a sorted C header. The table is sorted by codepoint so lookups use
# binary search at runtime.
#
# Usage: ./bdf-unicode-to-c.py input.bdf output.h
#
# Covers the set of non-ASCII chars aesthetic.computer's web UI actually
# uses (arrows, em-dashes, Greek, common symbols, box drawing). Stays
# well under 1000 glyphs so binary bloat is bounded.

import gzip
import os
import sys


def ranges():
    return [
        (0x00A0, 0x00FF),  # Latin-1 Supplement (accented chars, typographic)
        (0x0100, 0x017F),  # Latin Extended-A
        (0x0391, 0x03CE),  # Greek (alpha..omega)
        (0x2010, 0x2027),  # General Punctuation (em-dash, ellipsis, quotes)
        (0x2030, 0x2044),  # Per mille, primes, fractions
        (0x2190, 0x21FF),  # Arrows
        (0x2200, 0x22FF),  # Math operators
        (0x2500, 0x257F),  # Box Drawing
        (0x2580, 0x259F),  # Block Elements
        (0x25A0, 0x25FF),  # Geometric Shapes
        (0x2600, 0x26FF),  # Misc Symbols (star, heart, note, sun, etc)
        (0x2700, 0x27BF),  # Dingbats (checkmark, cross, ballot)
    ]


def want(cp):
    for lo, hi in ranges():
        if lo <= cp <= hi:
            return True
    return False


def open_bdf(path):
    if path.endswith(".gz"):
        return gzip.open(path, "rt", encoding="latin-1")
    return open(path, "r", encoding="latin-1")


def parse(path):
    glyphs = {}
    font_ascent = 8
    with open_bdf(path) as f:
        enc = -1
        dwidth = 0
        bbx = (0, 0, 0, 0)
        rows = []
        in_bitmap = False
        for line in f:
            s = line.strip()
            if s.startswith("FONT_ASCENT "):
                try:
                    font_ascent = int(s.split()[1])
                except Exception:
                    pass
            elif s.startswith("STARTCHAR"):
                enc = -1
                dwidth = 0
                bbx = (0, 0, 0, 0)
                rows = []
                in_bitmap = False
            elif s.startswith("ENCODING "):
                try:
                    enc = int(s.split()[1])
                except Exception:
                    enc = -1
            elif s.startswith("DWIDTH "):
                parts = s.split()
                try:
                    dwidth = int(parts[1])
                except Exception:
                    dwidth = 0
            elif s.startswith("BBX "):
                parts = s.split()
                try:
                    bbx = (int(parts[1]), int(parts[2]), int(parts[3]), int(parts[4]))
                except Exception:
                    bbx = (0, 0, 0, 0)
            elif s == "BITMAP":
                in_bitmap = True
                rows = []
            elif s == "ENDCHAR":
                if enc != -1 and want(enc):
                    # Pack rows into bytes. Unifont glyphs can be up to 16 wide,
                    # stored as 2 bytes per row. We simplify to max width 16.
                    w, h, xoff, yoff = bbx
                    if w > 0 and h > 0 and w <= 16 and h <= 16 and len(rows) == h:
                        row_bytes = []
                        bytes_per_row = (w + 7) // 8
                        for r in rows:
                            try:
                                hb = int(r, 16)
                            except ValueError:
                                hb = 0
                            # Normalise to 2 bytes (MSB-padded)
                            if bytes_per_row == 1:
                                row_bytes.append((hb & 0xFF) << 8)
                            else:
                                row_bytes.append(hb & 0xFFFF)
                        glyphs[enc] = (w, h, xoff, yoff, dwidth, row_bytes)
                enc = -1
                in_bitmap = False
            elif in_bitmap and s:
                rows.append(s)
    return font_ascent, glyphs


def emit(ascent, glyphs, out_path):
    out = []
    out.append("// Auto-generated Unicode glyph subset from unifont — do not edit")
    out.append("// Produced by bdf-unicode-to-c.py")
    out.append("")
    out.append("#ifndef AC_FONT_UNICODE_H")
    out.append("#define AC_FONT_UNICODE_H")
    out.append("")
    out.append("#include <stdint.h>")
    out.append("")
    out.append("typedef struct {")
    out.append("    uint32_t codepoint;")
    out.append("    uint8_t  width;   // BBX width")
    out.append("    uint8_t  height;  // BBX height")
    out.append("    int8_t   xoff;")
    out.append("    int8_t   yoff;")
    out.append("    uint8_t  dwidth;")
    out.append("    uint16_t rows[16]; // up to 16 rows, MSB-padded")
    out.append("} UnicodeGlyph;")
    out.append("")
    out.append(f"static const int unicode_ascent = {ascent};")
    out.append(f"static const int unicode_glyph_count = {len(glyphs)};")
    out.append("")
    out.append("static const UnicodeGlyph unicode_glyphs[] = {")
    for cp in sorted(glyphs.keys()):
        w, h, xoff, yoff, dw, rs = glyphs[cp]
        rows_str = ", ".join("0x%04X" % r for r in rs)
        # Pad rows to 16 entries
        pad = 16 - len(rs)
        if pad > 0:
            rows_str += ", " + ", ".join(["0"] * pad)
        out.append(
            f"    {{0x{cp:04X}, {w}, {h}, {xoff}, {yoff}, {dw}, {{{rows_str}}}}},"
        )
    out.append("};")
    out.append("")
    out.append("#endif")
    out.append("")
    with open(out_path, "w") as f:
        f.write("\n".join(out))


def main():
    if len(sys.argv) < 3:
        print(f"usage: {sys.argv[0]} input.bdf output.h", file=sys.stderr)
        return 2
    ascent, glyphs = parse(sys.argv[1])
    emit(ascent, glyphs, sys.argv[2])
    print(f"[bdf-unicode] extracted {len(glyphs)} glyphs → {sys.argv[2]}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
