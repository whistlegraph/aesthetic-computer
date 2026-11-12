# voices/palindrome.py — the palindrome's vocal score (145 bars).
# The forward half (bars 0–72) is placed with the usual helpers; then every
# event is mirrored about the center Tc. Sung events are re-sung forwards at
# their retrograde place (so "aesthetina" toots come back as "anitehtsea",
# arpeggio paths reversed in time); the raw inflection accents (whispers,
# shouts, angry hits, screams) are played time-reversed, so they swell into
# the mirror point. The center bar's scream rises to the axis and falls away.

CENTER = 72
Tc = at(CENTER, 8)
M = lambda t: 2 * Tc - t

# ── forward half ────────────────────────────────────────────────────────────
for bar in [12, 14, 16, 18, 20, 22, 24, 26, 56, 58, 62, 66, 70]: hook_line(bar, TOOT_V, 1.0, "toot")
for bar in [60, 64, 68]: hook_line(bar, CHOIR, 1.0, "toot")

SCAT = [(0, "bah", 57, .10, None), (3, "kah", 57, .06, None), (6, "trah", 57, .10, 52), (8, "ah", 60, .18, None),
        (11, "kah", 57, .06, None), (14, "trah", 64, .08, 57), (16, "bah", 57, .10, None), (18, "ah", 64, .10, None),
        (19, "kah", 57, .06, None), (22, "trah", 57, .10, 52), (24, "bah", 60, .08, None), (26, "kah", 57, .06, None),
        (27, "ah", 64, .12, None), (30, "trah", 69, .08, 57)]
for bar in [8, 10]:
    for st, nm, m, h, sl in SCAT: ev(at(bar, st), nm, m, h * 1.6, sl, 0.55 if st % 8 else 0.7, "dry", halo=0)

prev = None
for i, (nm, ch) in enumerate(zip(["ah", "eh", "ee", "oh", "oo"], "aeiou")):
    ev(at(29 + i), nm, KEY[ch] - 12, BAR * 1.0, prev, 0.9, "toot", toot=False, vib=0.18, rel=0.6)
    prev = KEY[ch] - 12

for bar in list(range(37, 44, 2)) + list(range(45, 52, 2)):
    for nm, st in KONNA:
        tom = st in (4, 15)
        ev(at(bar, st), nm, 64 if tom else 57, 0.28 if tom else 0.1, None, 0.75 if tom else 0.45, "dry", halo=0, vib=0)

for j, bar in enumerate(range(36, 44, 2)): chord_word(bar, ["tea", "daw", "rib", "ace"][j], BAR * 1.95)
for j, bar in enumerate(range(44, 52)): chord_word(bar, ["tea", "daw", "rib", "ace"][j % 4], BAR * 0.97)
for j, bar in enumerate(range(64, 72, 2)): chord_word(bar, ["ace", "fad", "big", "tea"][j], BAR * 1.95)

PLAN = [(12, 2, "tea", "oo", 1, .6), (14, 2, "tea", "oo", 1, .7)]
PLAN += [(16 + 2 * j, 2, w, "ah", 1, 1) for j, w in enumerate(["tea", "taw", "tea", "daw", "tea", "taw"])]
PLAN += [(28 + 2 * j, 2, "tea", "oh", 2, .9) for j in range(4)]
PLAN += [(36 + 2 * j, 2, w, "ah", 1, .9) for j, w in enumerate(["tea", "daw", "rib", "ace"])]
PLAN += [(44 + j, 1, ["tea", "daw", "rib", "ace"][j % 4], "ee", 1, .8) for j in range(8)]
PLAN += [(52, 4, "tea", "oo", 2, .8)]
PLAN += [(56 + 2 * j, 2, w, "ah", 1, 1) for j, w in enumerate(["tea", "taw", "tea", "taw", "ace", "fad", "big", "tea"])]
for bar, nb, w, v, sl, g in PLAN: choir_block(bar, nb, w, v, sl, g)

for bar in (2, 4, 6, 52):
    for i, st in enumerate(HOOK_STEPS):
        accent(at(bar, st), LETTER_NAME[HOOK[i]], "whisper", 0.5, [-0.4, 0.4][i % 2], "toot")
for i in range(5): accent(at(29 + i, 12), "hah", "breathy", 0.45, 0.0, "toot")
for bar in (43, 51):
    for st in (4, 15): accent(at(bar, st), "toh", "shout", 0.9, 0.0)
for j, bar in enumerate(range(44, 52)):
    accent(at(bar), ["tee", "dah", "ree", "ay"][j % 4], "angry", 0.6, [-0.3, 0.3][j % 2])
for bar in range(59, 72, 4): accent(at(bar, 12), "kah", "angry", 0.7, 0.25)
for bar in (16, 36, 56):
    accent(at(bar - 1, 12), "yee", "shout", 0.8, -0.2, "toot")
    accent(at(bar - 1, 14), "haw", "shout", 0.9, 0.2, "toot")
ev(at(56), "yah", 57, BAR * 1.6, 50, 1.1, "toot", **SCREAM)
ev(at(64), "ay", 64, BAR * 1.6, 57, 1.1, "toot", **SCREAM)
ev(at(54), "ah", 52, BAR * 2, None, 0.9, "toot", path=tuple((j * BEAT, 52 + [0, 2, 4, 5, 7, 9, 11, 12][j]) for j in range(8)), **SCREAM)

# ── the mirror ──────────────────────────────────────────────────────────────
# sung events: same syllable, placed at the retrograde of its note span;
# legato slides re-linked to whichever note now comes first; paths reversed.
fwd = list(events)
mir = []
for t, nm, m, h, sl, g, b, o in fwd:
    o = dict(o)
    if "path" in o:
        ts = [p[0] for p in o["path"]] + [h]
        o["path"] = tuple((h - ts[j + 1], o["path"][j][1]) for j in reversed(range(len(o["path"]))))
        m = o["path"][0][1]
    o.pop("decay", None); o.pop("floor", None)  # coming back, notes hold instead of plucking
    mir.append([M(t + h), nm, m, h, sl is not None, g, b, o])
mir.sort(key=lambda e: e[0])
last = {}
for e in mir:
    t, nm, m, h, legato, g, b, o = e
    p = last.get(b)
    slide = p[1] if legato and p and t - p[0] < 0.9 else None
    events.append((t, nm, m, h, slide, g, b, o))
    last[b] = (t + h, m)

# raw accents: reversed audio that ends where the forward one began
_raw_clip = raw_clip


def raw_clip(name, style):
    if style.endswith("|rev"):
        y = _raw_clip(name, style[:-4])
        return None if y is None else y[::-1].copy()
    return _raw_clip(name, style)


for t, nm, style, g, pan, b in list(accents):
    y = _raw_clip(nm, style)
    if y is None: continue
    accents.append((M(t) - len(y) / FS, nm, style + "|rev", g, -pan, b))

# ── the center: one scream rising to the axis and falling away ──────────────
ev(at(CENTER), "ah", 57, BAR * 0.98, None, 1.2, "toot",
   path=tuple((j * S16 * 2, 57 + [0, 2, 4, 7, 9, 12, 14, 16, 16, 14, 12, 9, 7, 4, 2, 0][j % 16]) for j in range(8))
   + tuple((BAR / 2 + j * S16 * 2, 57 + [16, 14, 12, 9, 7, 4, 2, 0][j]) for j in range(8)), **SCREAM)
accent(Tc - 0.05, "ah", "whisper", 0.7, 0.0, "toot")
