# voices/radio.py — the two-minute cut's vocal score (64 bars); see arrangements/radio.mjs
# intro 0–4 · A1 4–12 · A2 12–20 · break 20–24 · drop 24–40 · C 40–48 · drop II 48–60 · outro 60–64

# the new typed words, toot-sung on their own letters' pitches (vowels follow the word's sound)
WORDS = {
    "tumbleweed": ("tumbleweed", [0, 3, 6, 8, 10, 12, 14, 16, 19, 22], ["uh", "uh", "uh", "uh", "uh", "ee", "ee", "ee", "ee", "ee"]),
    "rainbow": ("rainbow", [0, 3, 6, 10, 12, 16, 22], ["ay", "ay", "ay", "ay", "oh", "oh", "oh"]),
    "mountain": ("mountain", [0, 3, 6, 8, 12, 14, 16, 22], ["ah", "ow", "ow", "ow", "ah", "ah", "ih", "ih"]),
    "adventure": ("adventure", [0, 3, 6, 8, 10, 12, 14, 19, 22], ["ah", "eh", "eh", "eh", "eh", "oo", "oo", "er", "er"]),
    "rodeo": ("rodeo banjo", [0, 2, 4, 6, 8, 10, 12, 14, 16, 19, 22], ["oh", "oh", "ay", "ee", "oh", "ah", "ah", "ah", "oh", "oh"]),
}


def word_line(bar, key, gain=1.0, bus="toot", octave=-1, until=30):
    text, steps, names = WORDS[key]
    notes = [(s, c) for s, c in zip(steps, text) if c != " "]
    ext = [s for s, _ in notes] + [until]; prev = None
    for i, ((s, c), nm) in enumerate(zip(notes, names)):
        mid = KEY[c] + 12 * octave
        gap = (ext[i + 1] - s) * S16
        ev(at(bar, s), nm, mid, gap + 0.4 if i == len(notes) - 1 else gap * 0.95, prev, gain, bus, decay=0.35, floor=0.2)
        prev = mid


# ── intro: instrumental; only the yee-haw call into A1 (below) ──

# the changes (mirror of arrangements/radio.mjs PLAN)
PLAN = [(4, ["tea", "taw", "tea", "daw"], 2), (12, ["tea", "daw", "tea", "taw"], 2), (20, ["tea", "daw", "rib", "ace"], 1),
        (24, ["tea", "taw", "tea", "taw", "ace", "fad", "big", "tea"], 2), (40, ["tea", "taw", "daw", "tea"], 2),
        (48, ["ace", "fad", "big", "tea", "tea", "taw"], 2)]


def chord_at(bar):
    for start, words, per in PLAN:
        i = (bar - start) // per
        if bar >= start and i < len(words): return words[i]
    return "tea"


# ── the lead: lyrics on the hook's ten notes, every time the hook is typed ──
HAND = ["will", "yoo", "please", "hand", "mee", "that", "kum", "pew", "ter", "there"]   # will you please hand me that computer there?
BUY = ["ai", "want", "too", "bai", "uh", "nuh", "ther", "kum", "pew", "ter"]            # i want to buy another computer
GET = ["ai", "need", "too", "get", "uh", "nuh", "ther", "kum", "pew", "ter"]            # i need to get another computer
CHUBBY = ["uh", "chub", "bee", "kay", "bul", "fer", "mai", "char", "jing", "please"]    # (pass) a chubby cable for my charging please


CHUBBY_EMPH = {1: 1.6, 2: 1.5}  # "chub · bee": two detached words


def lyric(bar, words, gain=1.15):
    if words is CHUBBY:  # 11 syllables: "pass" is a pickup into the hook's ten
        ev(at(bar, -3), "pass", KEY["a"] - 12, 3 * S16 * 0.9, None, gain * 1.2, "lyric", decay=0.5, floor=0.3)
    hook_line(bar, words, gain, "lyric", emph=CHUBBY_EMPH if words is CHUBBY else None, stacc=(1, 2) if words is CHUBBY else (),
              end_chord=chord_at(bar + 1))


# whole spoken phrases (bin/phrases.mjs) stretched onto the hook's notes — natural cadence,
# full words — in three cadences; the stitched-syllable version is sprinkled between for texture
LINE_OF = {id(HAND): "hand", id(BUY): "buy", id(GET): "get", id(CHUBBY): "chubby"}


def sung(bar, line, cadence, voice="jeffrey", gain=1.3, oct=-1, pan=0.0):
    starts = [at(bar, st) for st in HOOK_STEPS]
    midis = [KEY[HOOK[i]] + 12 * oct for i in range(10)]
    if line == "chubby": starts = [at(bar, -3)] + starts; midis = [KEY["a"] + 12 * oct] + midis  # "pass" pickup
    phrase_ev(starts, voice, f"{line}-{cadence}", midis, (30 - 22) * S16 + 0.45, gain, "lyric", pan)


PLAN_LYRICS = [  # (bar, line, how): how = a cadence (sung phrase) or "stitch" (syllable by syllable)
    (4, HAND, "plain"), (6, CHUBBY, "plain"), (8, HAND, "excited"), (10, CHUBBY, "deadpan"),
    (12, BUY, "plain"), (16, GET, "stitch"),
    (24, HAND, "excited"), (28, BUY, "plain"), (30, GET, "deadpan"), (32, HAND, "stitch"),
    (34, CHUBBY, "excited"), (36, BUY, "deadpan"), (38, GET, "excited"),
    (48, HAND, "deadpan"), (52, BUY, "excited"), (54, GET, "stitch"), (56, HAND, "plain"), (58, CHUBBY, "plain"), (62, HAND, "deadpan"),
]
for bar, words, how in PLAN_LYRICS:
    if how == "stitch": lyric(bar, words)
    else: sung(bar, LINE_OF[id(words)], how)
# ── Prutti (Pruttivox IVC, eleven_v4) — DRAFT: his clone + performance need his approval before release ──
# he takes the chubby-cable line answering jeffrey, and echoes "com-pu-ter" over jeffrey's last held note
def prutti_line(bar, words, gain=1.15, pan=0.3):
    # two octaves under the typed notes (an octave below jeffrey), with a quiet double an octave up
    # his phrase take (alternating cadence), two octaves under the typed notes
    sung(bar, "chubby", "plain" if bar < 40 else "excited", voice="prutti", gain=gain * 1.3, oct=-2, pan=pan)
    hook_line(bar, words, gain * 0.35, "chord", emph=CHUBBY_EMPH, stacc=(1, 2), voice="prutti", pan=-pan)


def prutti_echo(bar, gain=0.9):  # bar = the hook's bar; lands on its ringing last note
    prev = None
    for st, nm, m, h in ((23, "kum", 52, .2), (25, "pew", 54, .2), (27, "ter", 52, .5)):  # E F♯ E, an octave under jeffrey's held A
        ev(at(bar, st), nm, m, h, prev, gain, "toot", decay=0.3, floor=0.25, voice="prutti", pan=0.5); prev = m


for bar in (26, 50): prutti_line(bar, CHUBBY)
for bar in (28, 36, 52, 54): prutti_echo(bar)
# ── vowel arpeggios: one held vowel walking the chord in 16ths, up and down two octaves,
# re-struck on every note; panned side to side by block so they move around the head
def arp_block(bar, nbars, word, vowel, gain=0.5, pan=0.0):
    tn = tones(word, 57, 76); cyc = tn + tn[-2:0:-1]
    path = tuple((j * S16, cyc[j % len(cyc)]) for j in range(nbars * 16))
    ev(at(bar), vowel, path[0][1], nbars * BAR, None, gain, "choir", path=path, toot=False, halo=0.15, vib=0.0,
       rel=0.3, pan=pan, decay=S16 * 1.6, floor=0.12)


for j, bar in enumerate(range(24, 40, 2)): arp_block(bar, 2, chord_at(bar), "ah", 0.45, [-0.7, 0.7][j % 2])
for j, bar in enumerate(range(40, 48, 2)): arp_block(bar, 2, chord_at(bar), "oo", 0.4, [0.6, -0.6][j % 2])
for j, bar in enumerate(range(48, 60, 2)): arp_block(bar, 2, chord_at(bar), "ee", 0.45, [-0.7, 0.7][j % 2])
# the toots stay as a quiet shadow under the drops
for bar in (24, 26, 28, 30, 32, 34, 36, 38, 48, 50, 52, 54, 56, 58): hook_line(bar, TOOT_V, 0.35, "chord")
word_line(14, "tumbleweed"); word_line(18, "rainbow")
for bar, key in ((40, "mountain"), (42, "rainbow"), (44, "adventure"), (46, "rodeo")):
    word_line(bar, key)
    word_line(bar, key, 0.45, "chord", octave=-2)  # an octave-down double, so the new tunes sit heavy
for bar in (50, 54): word_line(bar, "tumbleweed", 0.5, "chord", octave=-2)

# ── break: chord words a bar each, konnakol, then the rising scream over the ladder ──
for j, bar in enumerate(range(20, 24)): chord_word(bar, ["tea", "daw", "rib", "ace"][j], BAR * 0.97)
for j, bar in enumerate(range(20, 24)):
    accent(at(bar), {"tea": "tee", "daw": "dah", "rib": "ree", "ace": "ay"}[["tea", "daw", "rib", "ace"][j]], "angry", 0.6, [-0.3, 0.3][j % 2])
for bar in (20, 21):
    for nm, st in KONNA:
        tom = st in (4, 15)
        ev(at(bar, st), nm, 64 if tom else 57, 0.28 if tom else 0.1, None, 0.75 if tom else 0.45, "dry", halo=0, vib=0)
for st in (4, 15): accent(at(21, st), "toh", "shout", 0.9, 0.0)
ev(at(22), "ah", 52, BAR * 2, None, 0.9, "toot", path=tuple((j * BEAT, 52 + [0, 2, 4, 5, 7, 9, 11, 12][j]) for j in range(8)), **SCREAM)

# ── chord words in the drops and C ──
for j, bar in enumerate(range(32, 40, 2)): chord_word(bar, ["ace", "fad", "big", "tea"][j], BAR * 1.95)
for j, bar in enumerate(range(48, 56, 2)): chord_word(bar, ["ace", "fad", "big", "tea"][j], BAR * 1.95)
for j, bar in enumerate(range(40, 48, 2)): chord_word(bar, ["tea", "taw", "daw", "tea"][j], BAR * 1.95)

# ── the choir: stretched vowels arpeggiating each chord ──
CHOIR_PLAN = [(4 + 2 * j, 2, w, "oo", 1, .7) for j, w in enumerate(["tea", "taw", "tea", "daw"])]
CHOIR_PLAN += [(12 + 2 * j, 2, w, "ah", 1, .9) for j, w in enumerate(["tea", "daw", "tea", "taw"])]
CHOIR_PLAN += [(24 + 2 * j, 2, w, "ah", 1, 1) for j, w in enumerate(["tea", "taw", "tea", "taw", "ace", "fad", "big", "tea"])]
CHOIR_PLAN += [(40 + 2 * j, 2, w, "oh", 2, .9) for j, w in enumerate(["tea", "taw", "daw", "tea"])]
CHOIR_PLAN += [(48 + 2 * j, 2, w, "ee", 1, 1) for j, w in enumerate(["ace", "fad", "big", "tea", "tea", "taw"])]
CHOIR_PLAN += [(60, 4, "tea", "oo", 2, .6)]
for bar, nb, w, v, sl, g in CHOIR_PLAN: choir_block(bar, nb, w, v, sl, g)

# ── screams, calls, the last word ──
ev(at(24), "yah", 57, BAR * 1.6, 50, 1.1, "toot", **SCREAM)
ev(at(32), "ay", 64, BAR * 1.6, 57, 1.1, "toot", **SCREAM)
ev(at(38, 8), "yah", 69, BAR * 1.4, 57, 1.0, "toot", **SCREAM)
ev(at(48), "yah", 57, BAR * 1.6, 50, 1.1, "toot", **SCREAM)
ev(at(56), "ay", 69, BAR * 2.4, 57, 1.1, "toot", **SCREAM)
for bar in (27, 31, 35, 39, 51, 55, 59): accent(at(bar, 12), "kah", "angry", 0.7, 0.25)
for bar in (4, 24, 40, 48):
    accent(at(bar - 1, 12), "yee", "shout", 0.8, -0.2, "toot")
    accent(at(bar - 1, 14), "haw", "shout", 0.9, 0.2, "toot")
prev = None
for st, nm, ch, h in [(0, "ay", "a", .45), (4, "dee", "d", .45), (8, "ee", "i", .45), (12, "ee", "e", .9), (20, "yoo", "u", 2.6)]:
    ev(at(60, st), nm, KEY[ch] - 12, h, prev, 0.9, "toot", toot=(nm in VOWELS))
    prev = KEY[ch] - 12
accent(at(63, 4), "yoo", "whisper", 0.6, 0.0, "toot")
