#!/usr/bin/env python3
# gen-6x10-supplement, 26.11.24 — build 6x10 glyphs the X11 face never
# shipped, out of the glyphs it did ship, so they share its hand.
#
#   curl -so /tmp/6x10.bdf https://assets.aesthetic.computer/type/6x10.bdf
#   python3 toolchain/type/gen-6x10-supplement.py /tmp/6x10.bdf \
#     system/backend/bdf-supplement-6x10.json /tmp/6x10-sheet.png
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
#   3. everything else is listed, not guessed — those are for a hand.
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
ABOVE = {"̀", "́", "̂", "̃", "̄", "̆", "̇", "̈",
         "̉", "̊", "̋", "̌", "̏", "̑", "̛"}
BELOW = {"̣", "̤", "̥", "̦", "̧", "̨", "̭", "̮", "̰", "̱"}
seen = {}
for cp, rows in font.items():
    d = unicodedata.normalize("NFD", chr(cp))
    if len(d) != 2 or ord(d[0]) not in font or not d[0].islower(): continue
    base, glyph = grid(font[ord(d[0])]), grid(rows)
    diff = [["#" if glyph[r][c] == "#" and base[r][c] != "#" else "." for c in range(COLS)] for r in range(ROWS)]
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
MARK.setdefault("̉", drawn({0: ".##...", 1: "..#..."}))   # hook above
MARK.setdefault("̛", drawn({2: "....#.", 3: "....#."}))   # horn
MARK.setdefault("̣", drawn({9: "..#..."}))                 # dot below
MARK.setdefault("̤", drawn({9: ".#.#.."}))                 # diaeresis below
MARK.setdefault("̥", drawn({8: "..#...", 9: ".#.#.."}))    # ring below (squashed)
MARK.setdefault("̦", drawn({8: "..#...", 9: ".#...."}))    # comma below
MARK.setdefault("̱", drawn({9: ".###.."}))                 # macron below
MARK.setdefault("̭", drawn({9: ".#.#.."}))                 # circumflex below
MARK.setdefault("̰", drawn({9: ".##.#."}))                 # tilde below
MARK.setdefault("̏", drawn({0: "#..#..", 1: ".#..#."}))    # double grave
MARK.setdefault("̑", drawn({0: ".###..", 1: "#...#."}))    # inverted breve
MARK.setdefault("̇", drawn({1: "..#..."}))

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
    g = grid(base)
    above = [m for m in marks if m in ABOVE]
    below = [m for m in marks if m in BELOW]
    if any(m not in MARK for m in marks): return None
    if above:
        b = bounds(g)
        if b and b[0] < XH:  # a capital or ascender: make room the way the font does
            need = max(mark_rows(m)[1] for m in above[:1]) + 1
            if b[0] < need + 1: g = squeeze(g, b[0], 7, need + 1)
            if g is None: return None
        if len(above) == 1:
            m = recenter(MARK[above[0]], g)
            if any(m[r][c] == "#" and g[r][c] == "#" for r in range(ROWS) for c in range(COLS)): return None
            g = orr(g, m)
        else:
            # Vietnamese stacks (ấ ẫ ặ): the first mark over the letter, the
            # second as a single pixel pair at its upper right.
            first = recenter(MARK[above[0]], g)
            g = orr(g, first)
            tail = {"́": (0, 4), "̀": (0, 0), "̉": (0, 4), "̃": (0, 4)}.get(above[1])
            if not tail: return None
            g[tail[0]][tail[1]] = "#"
    for m in below:
        b = bounds(g)
        if b and b[1] > 7: return None  # descender already there
        g = orr(g, recenter(MARK[m], g))
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
    tint = {"compose": 0, "small capital": 60, "turned": 30, "reversed": 30, "stroke": 90}[how[cp]]
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
