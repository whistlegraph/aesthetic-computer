# voices/phasing.py — the Piano Phase version's vocal score (152 bars).
# Mirrors arrangements/phasing.mjs: typist A at home (left), B phasing (right),
# C the counter. Toots follow A and B; the choir holds the harmony; whispers name
# C's letters as it drifts; screams and shouts land where the phases realign.

CYCLE = 32
COUNTER_C = "aesthetic computer"  # typed on the hook's complement (render.mjs COUNTER_STEPS)
COUNTER_STEPS_C = [1, 2, 4, 5, 7, 9, 11, 13, 15, 17, 18, 20, 21, 23, 24, 25, 26, 27]


def off_b(bar):
    if bar < 16: return None
    if bar < 48: return bar - 16
    if bar < 64: return 0
    if bar < 96: return 8
    if bar < 112: return None
    if bar < 144: return 16 + (bar - 112) // 2
    return 0


def off_c(bar):
    if bar < 64: return None
    if bar < 96: return (bar - 64) // 2
    if bar < 112: return 16 + (bar - 96) // 4
    if bar < 144: return bar - 112
    if bar < 150: return 0
    return None


def word_at(bar):
    if bar < 32: return "tea"
    if bar < 48: return ["tea", "taw"][((bar - 32) >> 2) % 2]
    if bar < 64: return "tea"
    if bar < 96: return ["tea", "taw", "daw", "tea"][((bar - 64) >> 3) % 4]
    if bar < 112: return ["ace", "fad"][((bar - 96) >> 2) % 2]
    if bar < 144: return ["ace", "fad", "big", "tea"][((bar - 112) >> 1) % 4]
    return "tea"


def phased_toots(bar, k, pan, gain):
    pairs = sorted(((s + k) % CYCLE, i) for i, s in enumerate(HOOK_STEPS))
    prev = None
    for j, (st, i) in enumerate(pairs):
        nxt = pairs[j + 1][0] if j + 1 < len(pairs) else CYCLE - 0.5
        hold = (nxt - st) * S16 * 0.95
        mid = KEY[HOOK[i]] - 12
        ev(at(bar) + st * S16, TOOT_V[i], mid, hold, prev, gain, "toot", decay=0.35, floor=0.2, pan=pan)
        prev = mid


# solo: A's letters whispered as they're typed
for bar in (2, 4, 6):
    for i, st in enumerate(HOOK_STEPS):
        accent(at(bar, st), LETTER_NAME[HOOK[i]], "whisper", 0.5, -0.45, "toot")

# toots: A on the left, B on the right
for bar in list(range(8, 16, 2)) + list(range(48, 64, 2)) + list(range(144, 150, 2)):
    phased_toots(bar, 0, -0.45, 0.9)
for bar in list(range(16, 48, 4)) + list(range(48, 64, 2)) + list(range(112, 144, 4)) + list(range(144, 150, 2)):
    phased_toots(bar, off_b(bar), 0.45, 0.85)

# the counter's drift, named in whispers (phase II and the breakdown)
C_LETTER = dict(a="ay", e="ee", s="ehs", t="tee", h="aych", i="ai", c="see", o="oh", m="mah", p="pee", u="yoo", r="rah")
for bar in range(64, 112, 4):
    k = off_c(bar)
    for st, ch in zip(COUNTER_STEPS_C, COUNTER_C):
        if ch == " ": continue
        accent(at(bar) + ((st + k) % CYCLE) * S16, C_LETTER[ch], "whisper", 0.32, 0.15, "toot")

# the aesthetivox choir holding the harmony
for bar in range(32, 48, 2): choir_block(bar, 2, word_at(bar), "ah", 1, 0.7)
for bar in range(48, 64, 2): choir_block(bar, 2, word_at(bar), "ah", 1, 1.0)
for bar in range(64, 96, 4): choir_block(bar, 4, word_at(bar), "oo", 1, 0.75)
for bar in range(96, 112, 4): choir_block(bar, 4, word_at(bar), "oh", 2, 0.85)
for bar in range(112, 144, 2): choir_block(bar, 2, word_at(bar), "ee" if (bar - 112) % 8 < 4 else "ah", 1, 0.9)
for bar in range(144, 150, 2): choir_block(bar, 2, "tea", "ah", 1, 1.0)
choir_block(150, 2, "tea", "oo", 2, 0.7)

# realignments: extended screams where the phases come home
ev(at(48), "yah", 57, BAR * 1.6, 50, 1.1, "toot", **SCREAM)
ev(at(56), "ay", 64, BAR * 1.6, 57, 1.0, "toot", **SCREAM)
ev(at(144), "yah", 57, BAR * 1.6, 50, 1.15, "toot", **SCREAM)
ev(at(146, 8), "ay", 69, BAR * 1.4, 57, 1.0, "toot", **SCREAM)
# over each tuplet ladder: one scream climbing into the realignment
for bar in (46, 142):
    ev(at(bar), "ah", 52, BAR * 2, None, 0.9, "toot",
       path=tuple((j * BEAT, 52 + [0, 2, 4, 5, 7, 9, 11, 12][j]) for j in range(8)), **SCREAM)

# yee-haw into the phase sections
for bar in (16, 64, 112):
    accent(at(bar - 1, 12), "yee", "shout", 0.8, -0.2, "toot")
    accent(at(bar - 1, 14), "haw", "shout", 0.9, 0.2, "toot")
# phase III: angry kah every 8 bars as the typists close in
for bar in range(119, 144, 8): accent(at(bar, 12), "kah", "angry", 0.7, 0.25)
# breakdown breath
for bar in range(98, 112, 4): accent(at(bar, 12), "hah", "breathy", 0.45, 0.0, "toot")
# the last word, whispered after the spacebar
accent(at(151, 12), "yoo", "whisper", 0.6, 0.0, "toot")
