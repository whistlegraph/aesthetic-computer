# voices/reveal.py — the slow-reveal cut's vocal score (96 bars); see arrangements/reveal.mjs
# boot 0–8 · kick 8–16 · A 16–32 · keys 32–40 · drop 40–56 · bells 56–64 · C 64–72 · build 72–76 · drop II 76–92 · outro 92–96
#
# A statement is sung every two bars wherever the band is in, each a different line,
# cast only from takes bin/recog.py hears whole. The lead is one voice, alone: nothing
# doubles or harmonises it, and nothing else sings on its notes.

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


# the changes (mirror of arrangements/reveal.mjs PLAN)
PLAN = [(16, ["tea", "taw", "tea", "daw", "tea", "daw", "tea", "taw"], 2), (32, ["tea", "daw", "rib", "ace", "tea", "daw", "rib", "ace"], 1),
        (40, ["tea", "taw", "tea", "taw", "ace", "fad", "big", "tea"], 2), (56, ["tea", "tea", "daw", "tea"], 2),
        (64, ["tea", "taw", "daw", "tea"], 2), (72, ["tea", "daw", "rib", "ace"], 1),
        (76, ["ace", "fad", "big", "tea", "ace", "fad", "big", "tea"], 2)]


def chord_at(bar):
    for start, words, per in PLAN:
        i = (bar - start) // per
        if bar >= start and i < len(words): return words[i]
    return "tea"


# ── the statements: whole spoken phrases (bin/phrases.mjs) stretched onto the hook's notes ──
# The lines are ../lyrics.json. A line is only cast from takes bin/recog.py heard whole
# (out/recog-takes.json); its clean cadences take turns.
LINES = json.load(open(HERE.parent / "lyrics.json"))
NSYL = {k: len(v.replace("-", " ").split()) for k, v in LINES.items()}
HEARD = json.load(open(OUT / "recog-takes.json"))
BANNED = {("jeffrey", "buy", "deadpan")}  # reads as "i don't want to buy"
MOODS = ("plain", "excited", "deadpan", "curious", "shout", "sad", "tired", "whisper")


def cast(voice, line, want=()):
    """the first of the wanted moods that reads back whole; failing that, any take that does."""
    # whisper cannot spell "Prutti", "a mail" or "gamelan"; the rest of those lines must be whole
    need = {"wise": 0.85, "hear": 0.85, "amail": 0.6, "gamelan": 0.8}.get(line, 1)
    clean = [c for c in MOODS if (voice, line, c) not in BANNED and HEARD.get(f"{voice}/{line}-{c}", {}).get("recall", 0) >= need]
    assert clean, f"no clean take of {voice}/{line}: run bin/phrases.mjs --force, then bin/recog.py takes"
    return next((c for c in want if c in clean), clean[0])


def sung(bar, line, want=(), voice="jeffrey", gain=1.3, oct=-1, pan=0.0):
    """one voice, alone: the line in the first clean mood of `want`."""
    starts = [at(bar, st) for st in HOOK_STEPS]
    midis = [KEY[HOOK[i]] + 12 * oct for i in range(10)]
    n = NSYL[line]
    if n == 11: starts = [at(bar, -3)] + starts; midis = [KEY["a"] + 12 * oct] + midis  # a pickup syllable
    starts, midis = starts[:n], midis[:n]  # a shorter line stops on an earlier note and holds it
    phrase_ev(starts, voice, f"{line}-{cast(voice, line, want)}", midis, at(bar, 30) - starts[-1] + 0.45, gain, "lyric", pan)


# a statement every two bars wherever the band is in, and the mood has an arc: spoken plainly
# and asked curiously as the track arrives, shouted in the drops, sad and tired when the
# drums leave, whispered at the very end. Shouting is kept for two lines only (the first and
# last of the second drop): the lead should stay smooth
J, P = "jeffrey", "prutti"
STATEMENTS = [
    (4, "name", ("whisper",), J),  # before the kick: whispered, once
    (8, "name", ("plain", "excited"), J), (10, "hand", ("plain",), J), (12, "chubby", ("curious", "plain"), J), (14, "plug", ("plain",), J),
    (16, "borrow", ("curious",), J), (18, "buy", ("plain",), J), (20, "get", ("plain",), J), (22, "typing", ("excited",), J),
    (24, "sounds", ("curious", "plain"), J), (26, "letter", ("excited",), J), (28, "cables", ("curious", "excited"), J), (30, "sleep", ("tired", "deadpan"), J),
    # the first drop is a conversation. Prutti types in the clock chat and has no phone; the Pruttivox
    # clone is how he is heard. "where is a mail?", "you are a super star", "copy that" and "gamelan
    # tonight" are his own typed messages (chat-clock, 2026-11-13/15); "a mail moved out…" is jeffrey's reply.
    (40, "hear", ("plain", "excited"), J), (42, "wise", ("plain",), P), (44, "amail", ("plain",), P), (46, "moved", ("plain",), J),
    (48, "buy", ("excited", "plain"), J), (50, "gamelan", ("curious", "shout"), P), (52, "letter", ("excited",), J), (54, "chubby", ("plain",), P),
    (58, "cables", ("sad", "tired"), J), (60, "buy", ("sad", "tired"), J), (62, "sleep", ("whisper", "tired", "sad"), J),
    (76, "typing", ("shout", "excited"), J), (78, "amail", ("plain",), P), (80, "moved", ("shout", "plain"), J), (82, "get", ("excited",), J),
    (84, "chubby", ("plain",), P), (86, "cables", ("excited",), J), (88, "sleep", ("excited",), J), (90, "name", ("shout", "excited"), J),
    (94, "hand", ("whisper", "tired"), J),
]
# ── Prutti (Pruttivox IVC, eleven_v4) — DRAFT: his clone + performance need his approval before release ──
for bar, line, want, who in STATEMENTS:
    if who == P: sung(bar, line, want, voice=P, gain=1.5, pan=0.3)
    else: sung(bar, line, want, gain={4: 0.5, 94: 1.0}.get(bar, 1.3))  # the opening whisper sits inside the music box, not over it


def prutti_echo(bar, gain=0.9):  # lands on the statement's ringing last note
    prev = None
    for st, nm, m, h in ((23, "kum", 52, .2), (25, "pew", 54, .2), (27, "ter", 52, .5)):
        ev(at(bar, st), nm, m, h, prev, gain, "toot", decay=0.3, floor=0.25, voice="prutti", pan=0.5); prev = m


for bar in (48, 82): prutti_echo(bar)  # com-pu-ter, under jeffrey's last note


# ── vowel arpeggios: one held vowel walking the chord in 16ths (their own stem, they orbit in the master) ──
def arp_block(bar, nbars, word, vowel, gain=0.5, pan=0.0):
    tn = tones(word, 57, 76); cyc = tn + tn[-2:0:-1]
    path = tuple((j * S16, cyc[j % len(cyc)]) for j in range(nbars * 16))
    ev(at(bar), vowel, path[0][1], nbars * BAR, None, gain, "choir", path=path, toot=False, halo=0.15, vib=0.0,
       rel=0.3, pan=pan, decay=S16 * 1.6, floor=0.12)


# ── boot 0–8: no voice; a whisper of the word before the kick ──
accent(at(7, 8), "yoo", "whisper", 0.5, 0.0, "toot")

# ── kick 8–16: the choir opens under the first words ──
choir_block(8, 8, "tea", "oo", 2, 0.6)
accent(at(15, 12), "yee", "shout", 0.8, -0.2, "toot"); accent(at(15, 14), "haw", "shout", 0.9, 0.2, "toot")

# ── A 16–32: the choir under the statements ──
for j, w in enumerate(["tea", "taw", "tea", "daw", "tea", "daw", "tea", "taw"]): choir_block(16 + 2 * j, 2, w, "ah", 1, 0.75)

# ── keys 32–40: the switches' own section: konnakol on every beat pattern, chord words, the rising scream ──
for j, bar in enumerate(range(32, 38)):
    w = ["tea", "daw", "rib", "ace"][j % 4]
    chord_word(bar, w, BAR * 0.97)
    accent(at(bar), {"tea": "tee", "daw": "dah", "rib": "ree", "ace": "ay"}[w], "angry", 0.6, [-0.3, 0.3][j % 2])
for bar in (32, 33, 34, 35):
    for nm, st in KONNA:
        tom = st in (4, 15)
        ev(at(bar, st), nm, 64 if tom else 57, 0.28 if tom else 0.1, None, 0.75 if tom else 0.45, "dry", halo=0, vib=0,
           pan=[-0.4, 0.4][bar % 2])
for bar in (33, 35):
    for st in (4, 15): accent(at(bar, st), "toh", "shout", 0.9, 0.0)
ev(at(38), "ah", 52, BAR * 2, None, 0.9, "toot", path=tuple((j * BEAT, 52 + [0, 2, 4, 5, 7, 9, 11, 12][j]) for j in range(8)), **SCREAM)
accent(at(39, 12), "yee", "shout", 0.8, -0.2, "toot"); accent(at(39, 14), "haw", "shout", 0.9, 0.2, "toot")

# ── drop 40–56 ──
for j, bar in enumerate(range(40, 56, 2)):
    w = chord_at(bar)
    choir_block(bar, 2, w, "ah", 1, 0.8)
    arp_block(bar, 2, w, "ah", 0.5, [-0.7, 0.7][(j // 2) % 2])
    chord_word(bar, w, BAR * 1.95)
    accent(at(bar + 1, 12), "kah", "angry", 0.7, 0.25)

# ── bells 56–64: drums and bass out; the choir alone under three sad statements ──
choir_block(56, 8, "tea", "oo", 2, 1.0)
accent(at(63, 12), "yee", "shout", 0.8, -0.2, "toot"); accent(at(63, 14), "haw", "shout", 0.9, 0.2, "toot")

# ── C 64–72: the typed tunes, sung on their own letters, an octave-down double under each ──
for j, (bar, key) in enumerate(((64, "mountain"), (66, "rainbow"), (68, "adventure"), (70, "rodeo"))):
    word_line(bar, key)
    word_line(bar, key, 0.45, "chord", octave=-2)
    choir_block(bar, 2, chord_at(bar), "oh", 2, 0.9)
    arp_block(bar, 2, chord_at(bar), "oo", 0.35, [0.6, -0.6][j % 2])

# ── build 72–76 ──
for j, bar in enumerate(range(72, 76)): chord_word(bar, ["tea", "daw", "rib", "ace"][j], BAR * 0.97)
ev(at(74), "ah", 57, BAR * 2, None, 0.9, "toot", path=tuple((j * BEAT, 57 + [0, 2, 4, 5, 7, 9, 11, 12][j]) for j in range(8)), **SCREAM)
accent(at(75, 12), "yee", "shout", 0.8, -0.2, "toot"); accent(at(75, 14), "haw", "shout", 0.9, 0.2, "toot")

# ── drop II 76–92 ──
for j, bar in enumerate(range(76, 92, 2)):
    w = chord_at(bar)
    choir_block(bar, 2, w, "ee", 1, 0.8)
    arp_block(bar, 2, w, "ee", 0.5, [-0.7, 0.7][(j // 2) % 2])
    chord_word(bar, w, BAR * 1.95)
    accent(at(bar + 1, 12), "kah", "angry", 0.7, 0.25)

# ── outro 92–96: the letters' names, the choir settling, the last whisper ──
choir_block(92, 4, "tea", "oo", 2, 0.6)
prev = None
for st, nm, ch, h in [(0, "ay", "a", .45), (4, "dee", "d", .45), (8, "ee", "i", .45), (12, "ee", "e", .9), (20, "yoo", "u", 2.6)]:
    ev(at(92, st), nm, KEY[ch] - 12, h, prev, 0.9, "toot", toot=(nm in VOWELS))
    prev = KEY[ch] - 12
accent(at(95, 4), "yoo", "whisper", 0.6, 0.0, "toot")
