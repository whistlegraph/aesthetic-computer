#!/usr/bin/env python3
# gen-chunky8-supplement, 26.11.24 — MatrixChunky8 glyphs for the letters it
# never shipped (Latin Extended-B, Vietnamese, most Cyrillic and Greek), so
# compact text stops drawing them as "?".
#
#   curl -so /tmp/MatrixChunky8.bdf https://assets.aesthetic.computer/type/MatrixChunky8.bdf
#   curl -so /tmp/6x10.bdf https://assets.aesthetic.computer/type/6x10.bdf
#   python3 toolchain/type/gen-chunky8-supplement.py /tmp/MatrixChunky8.bdf \
#     /tmp/6x10.bdf system/backend/bdf-supplement-6x10.json \
#     system/backend/bdf-supplement-chunky8.json /tmp/chunky8-sheet.png
#
# MatrixChunky8 is an 8-row cell, proportional: capitals in rows 0-6,
# x-height rows 2-6, one descender row (7), marks in rows 0-1; letters three
# wide plus a spacing column (M and W five). Two moves:
#   1. composition in its own hand: its base letter + marks lifted from its
#      own accented pairs; a capital under a mark squeezes to the x-height
#      body, as its É and Ě do;
#   2. the rest fitted from the finished 6x10 glyph (the font, then
#      bdf-supplement-6x10.json): x-height maps row for row, the three rows
#      above fold into two, the two descender rows into one, and columns
#      thin toward three — stopping at four or five while what is left still
#      differs (ж ш Ѡ need the room).
# Output rows carry the spacing column, so a glyph's width is its advance.
# Standard library only.
import json, struct, sys, unicodedata, zlib
from collections import Counter

ROWS, XH = 8, 2

def load_bdf(path, rows, ascent):
    glyphs, cur, bits, bbx, dw = {}, None, None, None, None
    for line in open(path, encoding="latin-1"):
        p = line.split()
        if not p: continue
        if p[0] == "ENCODING": cur = int(p[1])
        elif p[0] == "DWIDTH": dw = int(p[1])
        elif p[0] == "BBX": bbx = tuple(map(int, p[1:5]))
        elif p[0] == "BITMAP": bits = []
        elif p[0] == "ENDCHAR":
            w, h, xo, yo = bbx
            width = max(dw or 0, w + max(xo, 0))
            g = [["."] * width for _ in range(rows)]
            for i, hx in enumerate(bits):
                v = int(hx, 16); nb = len(hx) * 4; r = ascent - 1 - (yo + h - 1 - i)
                for b in range(w):
                    if v >> (nb - 1 - b) & 1 and 0 <= r < rows and 0 <= xo + b < width: g[r][xo + b] = "#"
            glyphs[cur] = g
            bits = None
        elif bits is not None: bits.append(p[0])
    return glyphs

chunky = load_bdf(sys.argv[1], 8, 8)
six = {cp: [list(r) for r in g] for cp, g in load_bdf(sys.argv[2], 10, 8).items()}
for hexcp, rows in json.load(open(sys.argv[3])).items():
    six.setdefault(int(hexcp, 16), [list(r) for r in rows])

def ink(g):
    pts = [(r, c) for r in range(len(g)) for c in range(len(g[0])) if g[r][c] == "#"]
    if not pts: return None
    return min(p[0] for p in pts), max(p[0] for p in pts), min(p[1] for p in pts), max(p[1] for p in pts)

def trim(g):
    """Ink columns plus one spacing column: the glyph's advance."""
    b = ink(g)
    if not b: return None
    return [row[b[2]:b[3] + 1] + ["."] for row in g]

ham = lambda a, b: sum(x != y for x, y in zip(a, b))

def thin(lines, keep, floor=None, enough=None):
    """Drop the interior line most like a neighbour until `keep` remain — or,
    with `enough`, until every line differs from its neighbours by that much."""
    lines = [l[:] for l in lines]
    while len(lines) > (floor or keep):
        if len(lines) <= 2: lines.pop(); continue
        mid = len(lines) / 2
        scores = {i: min(ham(lines[i], lines[i - 1]), ham(lines[i], lines[i + 1])) for i in range(1, len(lines) - 1)}
        i = min(scores, key=lambda i: (scores[i], abs(i - mid)))
        if len(lines) <= keep and enough is not None and scores[i] >= enough: break
        lines.pop(i)
    return lines

# ── Marks from the font's own pairs (é − e), only in the mark rows.
ABOVE = {"̀", "́", "̂", "̃", "̄", "̆", "̇", "̈", "̉",
         "̊", "̋", "̌", "̏", "̑"}
BELOW = {"̣", "̤", "̥", "̦", "̧", "̨", "̭", "̮", "̰", "̱"}
seen = {}
for cp, g in chunky.items():
    d = unicodedata.normalize("NFD", chr(cp))
    if len(d) != 2 or ord(d[0]) not in chunky or not d[0].islower(): continue
    base = chunky[ord(d[0])]
    if len(base[0]) != len(g[0]): continue
    zone = range(0, XH) if d[1] not in BELOW else range(7, 8)
    diff = tuple("".join("#" if r in zone and g[r][c] == "#" and base[r][c] != "#" else "." for c in range(len(g[0]))) for r in range(ROWS))
    if any("#" in row for row in diff): seen.setdefault(d[1], Counter())[diff] += 1
MARK = {m: [list(r) for r in counter.most_common(1)[0][0]] for m, counter in seen.items()}

def drawn(width, rows):
    g = [["."] * width for _ in range(ROWS)]
    for r, s in rows.items(): g[r] = list(s.ljust(width, "."))
    return g
for m, rows in {"̉": {0: "##.", 1: ".#."}, "̣": {7: ".#."}, "̤": {7: "#.#"},
                "̥": {7: ".#."}, "̦": {7: ".#."}, "̱": {7: "###"}, "̮": {7: "#.#"},
                "̰": {7: "##."}, "̭": {7: "#.#"}, "̏": {0: "#.#", 1: ".#."},
                "̑": {0: ".#.", 1: "#.#"}, "̇": {1: ".#."}}.items():
    MARK.setdefault(m, drawn(4, rows))
ONE_ROW = {"́": "..#", "̀": "#..", "̂": ".#.", "̃": "##.", "̄": "###",
           "̆": "#.#", "̇": ".#.", "̈": "#.#", "̌": ".#.", "̉": ".#.", "̊": ".#."}

def place(mark, g):
    """The mark's ink, centred over the letter's ink."""
    out = [r[:] for r in g]; b, mb = ink(g), ink(mark)
    if not b or not mb: return out
    shift = (b[2] + b[3]) // 2 - (mb[2] + mb[3]) // 2
    for r in range(ROWS):
        for c in range(len(mark[0])):
            if mark[r][c] == "#" and 0 <= c + shift < len(out[0]): out[r][c + shift] = "#"
    return out

def compose(cp):
    d = unicodedata.normalize("NFD", chr(cp))
    if len(d) < 2 or ord(d[0]) not in chunky: return None
    marks = list(d[1:])
    if any(m not in ABOVE | BELOW | {"̛"} for m in marks): return None
    g = [r[:] for r in chunky[ord(d[0])]]
    b = ink(g)
    if not b: return None
    above = [m for m in marks if m in ABOVE]
    if above and b[0] < XH:  # a capital or ascender: the body to rows 2-6
        body = thin([g[r] for r in range(b[0], 7)], 5)
        g = [["."] * len(g[0]) for _ in range(XH)] + body + [g[7]]
    if len(above) == 1 and above[0] in MARK:
        g = place(MARK[above[0]], g)
    elif len(above) == 2 and all(m in ONE_ROW for m in above):
        for row, m in ((1, above[0]), (0, above[1])):
            one = [["."] * 3 for _ in range(ROWS)]; one[row] = list(ONE_ROW[m])
            g = place(one, g)
    elif above:
        return None
    for m in [m for m in marks if m in BELOW]:
        if m not in MARK or "#" in g[7]: return None
        g = place(MARK[m], g)
    if "̛" in marks:  # the horn, just outside the letter's top right
        b = ink(g); top = max(b[0], XH)
        for row in g: row.append(".")
        g[top - 1][b[3] + 1] = "#"; g[top][b[3] + 1] = "#"
    return trim(g)

def from_six(g6):
    """A finished 6x10 glyph fitted into the chunky cell."""
    orrow = lambda rows: ["#" if any(r[c] == "#" for r in rows) else "." for c in range(len(rows[0]))]
    top = thin([g6[0], g6[1], g6[2]], 2)  # marks + ascender/cap top
    rows = top + [g6[r] for r in range(3, 8)] + [orrow([g6[8], g6[9]])]
    b = ink(rows)
    if not b: return None
    cols = [[rows[r][c] for r in range(ROWS)] for c in range(b[2], b[3] + 1)]
    cols = thin(cols, 3, floor=3, enough=2) if len(cols) > 3 else cols
    g = [[cols[c][r] for c in range(len(cols))] for r in range(ROWS)]
    return trim(g)

BLOCKS = [(0x0180, 0x024F), (0x0250, 0x02AF), (0x1E00, 0x1EFF), (0x0400, 0x04FF),
          (0x0370, 0x03FF), (0x0100, 0x017F), (0x00C0, 0x00FF)]
out, how = {}, {}
for lo, hi in BLOCKS:
    for cp in range(lo, hi + 1):
        ch = chr(cp)
        if cp in chunky or not unicodedata.name(ch, "") or unicodedata.category(ch)[0] != "L": continue
        made = compose(cp); why = "compose"
        if not made and cp in six: made, why = from_six(six[cp]), "6x10"
        if made: out[cp], how[cp] = ["".join(r) for r in made], why

json.dump({f"{cp:04X}": rows for cp, rows in sorted(out.items())}, open(sys.argv[4], "w"), indent=0)
missing = [cp for lo, hi in BLOCKS for cp in range(lo, hi + 1)
           if cp not in chunky and cp not in out and unicodedata.name(chr(cp), "") and unicodedata.category(chr(cp))[0] == "L"]
print(f"made {len(out)}:", dict(Counter(how.values())), f"— left {len(missing)}")

# ── Contact sheet: glyphs at 6x on a strip, proportional, 24 a row.
S, PER = 6, 24
cells = sorted(out)
lines = [cells[i:i + PER] for i in range(0, len(cells), PER)]
W = max(sum(len(out[c][0]) * S + 6 for c in line) for line in lines) + 8
H = len(lines) * (ROWS * S + 10)
px = bytearray([230] * (W * H))
for li, line in enumerate(lines):
    x0, y0 = 4, li * (ROWS * S + 10) + 5
    for cp in line:
        g = out[cp]
        for r in range(ROWS):
            for c in range(len(g[0])):
                v = 0 if g[r][c] == "#" else (255 if r < 7 else 244)
                for y in range(S):
                    for x in range(S): px[(y0 + r * S + y) * W + x0 + c * S + x] = v
        x0 += len(g[0]) * S + 6
raw = b"".join(b"\x00" + bytes(px[y * W:(y + 1) * W]) for y in range(H))
ck = lambda t, d: struct.pack(">I", len(d)) + t + d + struct.pack(">I", zlib.crc32(t + d) & 0xFFFFFFFF)
open(sys.argv[5], "wb").write(b"\x89PNG\r\n\x1a\n" + ck(b"IHDR", struct.pack(">IIBBBBB", W, H, 8, 0, 0, 0, 0))
                              + ck(b"IDAT", zlib.compress(raw, 9)) + ck(b"IEND", b""))
for line in lines: print("".join(chr(c) for c in line))
