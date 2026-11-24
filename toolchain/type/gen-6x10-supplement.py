#!/usr/bin/env python3
# gen-6x10-supplement, 26.11.24 — build 6x10 glyphs the X11 face never
# shipped, out of the glyphs it did ship, so they share its hand.
#
#   curl -so /tmp/6x10.bdf https://assets.aesthetic.computer/type/6x10.bdf
#   curl -so /tmp/unifont.bdf.gz https://assets.aesthetic.computer/type/unifont-16.0.03.bdf.gz
#   python3 toolchain/type/gen-6x10-supplement.py /tmp/6x10.bdf \
#     system/backend/bdf-supplement-6x10.json /tmp/6x10-sheet.png /tmp/unifont.bdf.gz
#
# Writes the table bdf-supplement.mjs serves, a contact sheet to look at
# before shipping (grey = transformed, black = composed), and a -left.txt of
# what it would not guess. Standard library only.
#
# Three moves, each keyed off Unicode's own description of the character:
#   1. composition  — NFD says "base + marks"; the marks are lifted from the
#                     font's own accented pairs (é − e is the acute).
#   2. transforms   — TURNED rotates, REVERSED mirrors, SMALL CAPITAL squeezes
#                     a capital to x-height, WITH STROKE / WITH BAR crosses it.
#   3. the rest start from unifont's 8x16 drawing, fitted band by band into
#      the 6x10 cell (ascenders, x-height, descender; six columns into five)
#      by dropping the rows and columns that carry the least shape. These are
#      drafts: a glyph that reads badly gets drawn by hand in
#      bdf-supplement.mjs, which always wins.
#   4. anything unifont draws double-wide is listed, not squeezed.
# The hand-drawn table in bdf-supplement.mjs always wins over these.
import json, sys, struct, zlib, unicodedata
from collections import Counter

# ── The 6x10 BDF as {codepoint: [10 strings of 6]} — row 0 is the top of the
#    cell, row 7 sits on the baseline, rows 8-9 are descenders.

ASC = 8
def load_bdf(path):
    glyphs, cur, bits = {}, None, None
    for line in open(path, encoding="latin-1"):
        p = line.split()
        if not p: continue
        if p[0] == "ENCODING": cur = int(p[1])
        elif p[0] == "BBX": w, h, xo, yo = map(int, p[1:5])
        elif p[0] == "BITMAP": bits = []
        elif p[0] == "ENDCHAR":
            grid = [["."] * 6 for _ in range(10)]
            for i, hexrow in enumerate(bits):
                v = int(hexrow, 16); nb = len(hexrow) * 4
                y = yo + h - 1 - i; r = ASC - 1 - y
                for b in range(w):
                    if v >> (nb - 1 - b) & 1:
                        x = xo + b
                        if 0 <= r < 10 and 0 <= x < 6: grid[r][x] = "#"
            glyphs[cur] = ["".join(row) for row in grid]
            bits = None
        elif bits is not None: bits.append(p[0])
    return glyphs

class bdf:
    load = staticmethod(load_bdf)

ROWS, COLS, XH = 10, 6, 3  # x-height band is rows 3-7; caps rows 1-7
BLOCKS = [(0x0180, 0x024F), (0x0250, 0x02AF), (0x1E00, 0x1EFF), (0x0400, 0x04FF),
          (0x0370, 0x03FF), (0x0100, 0x017F), (0x00C0, 0x00FF)]

font = bdf.load(sys.argv[1])
blank = lambda: [["."] * COLS for _ in range(ROWS)]
grid = lambda rows: [list(r) for r in rows]
flat = lambda g: ["".join(r) for r in g]
lit = lambda g, r, c: 0 <= r < ROWS and 0 <= c < COLS and g[r][c] == "#"

def bounds(g):
    pts = [(r, c) for r in range(ROWS) for c in range(COLS) if g[r][c] == "#"]
    if not pts: return None
    rs, cs = [p[0] for p in pts], [p[1] for p in pts]
    return min(rs), max(rs), min(cs), max(cs)

def orr(a, b):
    return [["#" if a[r][c] == "#" or b[r][c] == "#" else "." for c in range(COLS)] for r in range(ROWS)]

# ── Marks, lifted from the font: precomposed minus base, the most common shape.
ABOVE = {"\u0300", "\u0301", "\u0302", "\u0303", "\u0304", "\u0306", "\u0307", "\u0308",
         "\u0309", "\u030A", "\u030B", "\u030C", "\u030F", "\u0311", "\u031B"}
BELOW = {"\u0323", "\u0324", "\u0325", "\u0326", "\u0327", "\u0328", "\u032D", "\u032E", "\u0330", "\u0331"}
seen = {}
for cp, rows in font.items():
    d = unicodedata.normalize("NFD", chr(cp))
    if len(d) != 2 or ord(d[0]) not in font or not d[0].islower(): continue
    base, glyph = grid(font[ord(d[0])]), grid(rows)
    # Only the mark's own rows count: above the x-height, or the descender.
    zone = range(0, XH) if d[1] not in BELOW else range(8, ROWS)
    diff = [["#" if r in zone and glyph[r][c] == "#" and base[r][c] != "#" else "." for c in range(COLS)] for r in range(ROWS)]
    b = bounds(diff)
    if not b: continue
    # Center the mark on column 2, where the font's 5-wide letters center.
    bb = bounds(base)
    shift = 2 - round((bb[2] + bb[3]) / 2) if bb else 0
    key = tuple("".join(diff[r][max(0, c - shift)] if 0 <= c - shift < COLS else "." for c in range(COLS)) for r in range(ROWS))
    seen.setdefault(d[1], Counter())[key] += 1
MARK = {m: [list(r) for r in counter.most_common(1)[0][0]] for m, counter in seen.items()}

# Marks the font never pairs with a lowercase letter, drawn once in its manner.
def drawn(rows_by_index):
    g = blank()
    for r, s in rows_by_index.items(): g[r] = list(s)
    return g
MARK.setdefault("\u0309", drawn({0: ".##...", 1: "..#..."}))   # hook above
MARK.setdefault("\u031B", drawn({2: "....#.", 3: "....#."}))   # horn
MARK.setdefault("\u0323", drawn({9: "..#..."}))                 # dot below
MARK.setdefault("\u0324", drawn({9: ".#.#.."}))                 # diaeresis below
MARK.setdefault("\u0325", drawn({8: "..#...", 9: ".#.#.."}))    # ring below (squashed)
MARK.setdefault("\u0326", drawn({8: "..#...", 9: ".#...."}))    # comma below
MARK.setdefault("\u0331", drawn({9: ".###.."}))                 # macron below
MARK.setdefault("\u032D", drawn({9: ".#.#.."}))                 # circumflex below
MARK.setdefault("\u0330", drawn({9: ".##.#."}))                 # tilde below
MARK.setdefault("\u030F", drawn({0: "#..#..", 1: ".#..#."}))    # double grave
MARK.setdefault("\u0311", drawn({0: ".###..", 1: "#...#."}))    # inverted breve
MARK.setdefault("\u0307", drawn({1: "..#..."}))
MARK.setdefault("\u032E", drawn({8: ".#.#..", 9: "..#..."}))   # breve below

# Stacked marks get one row each: the mark nearest the letter on row 1, the
# outer one on row 0.
ONE_ROW = {"\u0301": "...#..", "\u0300": ".#....", "\u0302": "..#...", "\u0303": ".##.#.",
           "\u0304": ".###..", "\u0306": ".#.#..", "\u0307": "..#...", "\u0308": ".#.#..",
           "\u030C": "..#...", "\u0309": "..#...", "\u030A": "..#..."}
SIDE = {"\u031B"}  # the horn sits at the upper right, beside the letter

def mark_rows(m):
    b = bounds(MARK[m]); return (b[0], b[1]) if b else (0, 0)

def recenter(mark, g):
    b, mb = bounds(g), bounds(mark)
    if not b or not mb: return mark
    shift = round((b[2] + b[3]) / 2) - round((mb[2] + mb[3]) / 2)
    out = blank()
    for r in range(ROWS):
        for c in range(COLS):
            if mark[r][c] == "#" and 0 <= c + shift < COLS: out[r][c + shift] = "#"
    return out

def squeeze(g, top, bottom, into_top):
    """Fit rows top..bottom into into_top..bottom by dropping a repeated row
    (nearest the middle) or, failing that, merging the top two — what the
    font does to É, Ě and Â."""
    rows = [g[r][:] for r in range(top, bottom + 1)]
    room = bottom - into_top + 1
    if room < 3 or len(rows) - room > 3: return None  # too tight to read
    while len(rows) > room:
        mid = len(rows) / 2
        dup = [i for i in range(1, len(rows)) if rows[i] == rows[i - 1]]
        if dup:
            i = min(dup, key=lambda i: abs(i - mid)); rows.pop(i)
        else:
            # The interior row most like a neighbour carries the least shape.
            ham = lambda a, b: sum(x != y for x, y in zip(a, b))
            inner = range(1, len(rows) - 1)
            i = min(inner, key=lambda i: (min(ham(rows[i], rows[i - 1]), ham(rows[i], rows[i + 1])), abs(i - mid)))
            rows.pop(i)
    out = blank()
    for r in range(0, into_top): out[r] = g[r][:] if r < top else ["."] * COLS
    for i, row in enumerate(rows): out[into_top + i] = row
    for r in range(bottom + 1, ROWS): out[r] = g[r][:]
    return out

def compose(base, marks):
    """Base + marks, in three steps: over it, under it, beside it (the horn)."""
    if any(m not in MARK for m in marks if m not in SIDE): return None
    g = grid(base)
    above = [m for m in marks if m in ABOVE and m not in SIDE]
    below = [m for m in marks if m in BELOW]
    side = [m for m in marks if m in SIDE]
    tall = (bounds(g) or (XH,))[0] < XH  # a capital or an ascender

    if len(above) == 1:
        m = recenter(MARK[above[0]], g)
        if tall:
            # The mark rises to rows 0-1 and the body squeezes to rows 2-7,
            # as the font draws Ě and Â.
            mb = bounds(m)
            lift = mb[1] - 1 if mb else 0
            if lift > 0: m = m[lift:] + [["."] * COLS for _ in range(lift)]
            if bounds(g)[0] < 2: g = squeeze(g, bounds(g)[0], 7, 2)
            if g is None: return None
        if any(m[r][c] == "#" and g[r][c] == "#" for r in range(ROWS) for c in range(COLS)): return None
        g = orr(g, m)
    elif len(above) == 2 and all(m in ONE_ROW for m in above):
        if tall and bounds(g)[0] < 2: g = squeeze(g, bounds(g)[0], 7, 2)
        if g is None: return None
        if above[0] in ("\u0302", "\u0306") and above[1] in ("\u0301", "\u0300", "\u0309", "\u0303"):
            # Vietnamese ấ ầ ẩ ẫ ặ: the hat over the letter, the tone at its corner.
            g = orr(g, recenter(MARK[above[0]], g))
            row, col = {"\u0301": (0, 4), "\u0300": (0, 0), "\u0309": (0, 4), "\u0303": (0, 4)}[above[1]]
            g[row][col] = "#"
        else:
            # Two marks, one row each: nearest the letter on row 1.
            for row, m in ((1, above[0]), (0, above[1])):
                one = blank(); one[row] = list(ONE_ROW[m])
                g = orr(g, recenter(one, g))
    elif above:
        return None

    for m in below:
        if bounds(g)[1] > 7: return None  # a descender is already there
        g = orr(g, recenter(MARK[m], g))

    for m in side:
        b = bounds([row[:] for row in grid(base)])  # the letter itself, not its marks
        col = min(b[3] + 1, COLS - 1)  # just outside it, in the spacing column
        top = 2 if tall and above else b[0] - 1  # beside the body's top, wherever it now is
        g[top][col] = "#"; g[top + 1][col] = "#"
    return g

def rotate(g):
    """TURNED: half a turn about the middle of the x-height band (row 5)."""
    b = bounds(g); out = blank()
    if not b: return out
    for r in range(ROWS):
        for c in range(COLS):
            if g[r][c] == "#":
                rr, cc = 10 - r, b[2] + b[3] - c
                if 0 <= rr < ROWS and 0 <= cc < COLS: out[rr][cc] = "#"
    return out

def mirror(g):
    b = bounds(g); out = blank()
    if not b: return out
    for r in range(ROWS):
        for c in range(COLS):
            if g[r][c] == "#": out[r][b[2] + b[3] - c] = "#"
    return out

def small_cap(capital):
    g = grid(capital); b = bounds(g)
    return squeeze(g, b[0], b[1], XH) if b else None

def stroke(g, diagonal=False, ascender=False):
    g = [r[:] for r in g]; b = bounds(g)
    if not b: return None
    if diagonal:
        h = b[1] - b[0]
        for i in range(h + 1):
            c = b[2] + round(i * (b[3] - b[2] + 1) / max(1, h)) - 1
            r = b[1] - i
            if 0 <= c < COLS: g[r][c] = "#"
        return g
    # Through the ascender for b d h k l, through the middle of everything else.
    r = 2 if ascender else (max(b[0], XH) + min(b[1], 7)) // 2 if b[0] >= XH - 2 else (b[0] + min(b[1], 7)) // 2
    left, right = max(0, b[2] - (1 if b[3] - b[2] < 2 else 0)), min(COLS - 2, b[3] + (1 if b[3] - b[2] < 2 else 0))
    for c in range(left, right + 1): g[r][c] = "#"
    return g

def letter(name):
    try: return ord(unicodedata.lookup(name))
    except KeyError: return None

# ── Unifont, fitted. Its baseline sits under row 13: ascenders and cap tops
#    in rows 3-5, x-height rows 6-13, descender rows 14-15, letters in
#    columns 1-6.
def load_unifont(path, wanted):
    import gzip
    glyphs, cur, bits, w = {}, None, None, 0
    for line in gzip.open(path, "rt", encoding="latin-1"):
        p = line.split()
        if not p: continue
        if p[0] == "ENCODING": cur = int(p[1])
        elif p[0] == "BBX": w = int(p[1])
        elif p[0] == "BITMAP": bits = [] if cur in wanted else None
        elif p[0] == "ENDCHAR":
            if bits is not None and w == 8:
                glyphs[cur] = [["#" if int(h, 16) >> (7 - c) & 1 else "." for c in range(8)] for h in bits]
            bits = None
        elif bits is not None: bits.append(p[0])
    return glyphs

def thin(rows, keep):
    """Drop the interior rows most like a neighbour until `keep` remain."""
    rows = [r[:] for r in rows]
    ham = lambda a, b: sum(x != y for x, y in zip(a, b))
    while len(rows) > keep:
        mid = len(rows) / 2
        if len(rows) <= 2: rows.pop(); continue
        i = min(range(1, len(rows) - 1), key=lambda i: (min(ham(rows[i], rows[i - 1]), ham(rows[i], rows[i + 1])), abs(i - mid)))
        rows.pop(i)
    return rows

def fit_unifont(u):
    orrow = lambda rows: ["#" if any(r[c] == "#" for r in rows) else "." for c in range(len(rows[0]))]
    rows = [orrow(u[0:3]), orrow(u[3:5]), u[5]] + thin(u[6:14], 5) + [u[14], u[15]]
    cols = [i for i in range(8) if any(r[i] == "#" for r in rows)]
    if not cols: return None
    lo, hi = cols[0], cols[-1]
    columns = thin([[r[c] for r in rows] for c in range(lo, hi + 1)], 5)
    g = blank()
    left = (5 - len(columns)) // 2 if len(columns) < 5 else 0
    for x, col in enumerate(columns):
        for r in range(ROWS): g[r][left + x] = col[r]
    return g

out, how, left = {}, {}, []
for lo, hi in BLOCKS:
    for cp in range(lo, hi + 1):
        ch = chr(cp); name = unicodedata.name(ch, "")
        if not name or cp in font or unicodedata.category(ch)[0] not in "L": continue
        made, why = None, None
        d = unicodedata.normalize("NFD", ch)
        if len(d) > 1 and ord(d[0]) in font:
            made, why = compose(font[ord(d[0])], list(d[1:])), "compose"
        if made is None:
            words = name.split()
            case = "CAPITAL" if "CAPITAL" in words and "SMALL CAPITAL" not in name else "SMALL"
            script = words[0]
            letter_at = words.index("LETTER") + 1 if "LETTER" in words else None
            if "SMALL CAPITAL" in name and script == "LATIN":
                base = letter(f"LATIN CAPITAL LETTER {' '.join(words[words.index('CAPITAL') + 1:])}")
                if base in font: made, why = small_cap(font[base]), "small capital"
            elif "TURNED" in words or "REVERSED" in words:
                rest = [w for w in words[letter_at:] if w not in ("TURNED", "REVERSED")] if letter_at else []
                base = letter(f"{script} {case} LETTER {' '.join(rest)}")
                src = font.get(base) or out.get(base)
                if src:
                    made = rotate(grid(src)) if "TURNED" in words else mirror(grid(src))
                    why = "turned" if "TURNED" in words else "reversed"
            elif "WITH STROKE" in name or "WITH BAR" in name or "WITH DIAGONAL STROKE" in name:
                base = letter(name.split(" WITH ")[0])
                src = font.get(base) or out.get(base)
                tall = chr(base) in "bdhklƀɓđ" if base else False
                # A level bar through c or e is just an e: theirs slant.
                slant = "DIAGONAL" in name or (base and chr(base) in "ceCE")
                if src: made, why = stroke(grid(src), slant, tall), "stroke"
        if made:
            out[cp], how[cp] = flat(made), why
        else:
            left.append(cp)

# Unifont drafts for what is left, then compose anything whose base only
# just appeared (ǯ from ʒ).
uni = load_unifont(sys.argv[4], set(left)) if len(sys.argv) > 4 else {}
for cp in left[:]:
    d = unicodedata.normalize("NFD", chr(cp))
    if len(d) > 1: continue
    made = fit_unifont(uni[cp]) if cp in uni else None
    if made: out[cp], how[cp] = flat(made), "unifont"; left.remove(cp)
for cp in left[:]:
    d = unicodedata.normalize("NFD", chr(cp))
    if len(d) > 1 and ord(d[0]) in out:
        made = compose(out[ord(d[0])], list(d[1:]))
        if made: out[cp], how[cp] = flat(made), "compose"; left.remove(cp)
for cp in left[:]:
    made = fit_unifont(uni[cp]) if cp in uni else None
    if made: out[cp], how[cp] = flat(made), "unifont"; left.remove(cp)

json.dump({f"{cp:04X}": rows for cp, rows in sorted(out.items())}, open(sys.argv[2], "w"), indent=0)
print(f"made {len(out)}:", dict(Counter(how.values())))
print(f"left for a hand: {len(left)}")
open(sys.argv[3].replace(".png", "-left.txt"), "w").write(
    "\n".join(f"U+{cp:04X} {chr(cp)} {unicodedata.name(chr(cp))}" for cp in left) + "\n")

# ── Contact sheet: every made glyph at 4x, gridded, beside its base where it has one.
cells = sorted(out)
cols, scale, pad = 24, 4, 3
cw, ch = COLS * scale + pad * 2, ROWS * scale + pad * 2
rows_n = (len(cells) + cols - 1) // cols
W, H = cols * cw, rows_n * ch
px = bytearray([235] * (W * H))
for i, cp in enumerate(cells):
    ox, oy = (i % cols) * cw + pad, (i // cols) * ch + pad
    tint = {"compose": 0, "small capital": 60, "turned": 30, "reversed": 30, "stroke": 90, "unifont": 120}[how[cp]]
    for r in range(ROWS):
        for c in range(COLS):
            on = out[cp][r][c] == "#"
            v = tint if on else (255 if r < 8 else 248)  # descender rows shaded
            for y in range(scale):
                for x in range(scale):
                    px[(oy + r * scale + y) * W + ox + c * scale + x] = v
    for c in range(COLS * scale):  # baseline tick
        px[(oy + 8 * scale) * W + ox + c] = 200 if px[(oy + 8 * scale) * W + ox + c] > 100 else px[(oy + 8 * scale) * W + ox + c]
raw = b"".join(b"\x00" + bytes(px[y * W:(y + 1) * W]) for y in range(H))
chunk = lambda t, d: struct.pack(">I", len(d)) + t + d + struct.pack(">I", zlib.crc32(t + d) & 0xFFFFFFFF)
open(sys.argv[3], "wb").write(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", W, H, 8, 0, 0, 0, 0))
                              + chunk(b"IDAT", zlib.compress(raw, 9)) + chunk(b"IEND", b""))
json.dump([f"{cp:04X}" for cp in cells], open(sys.argv[3].replace(".png", "-order.json"), "w"))
