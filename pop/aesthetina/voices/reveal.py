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
# spot takes (bin/phrases.mjs SPOT): the quiet lines with the voice kept on. A whispered take has no
# pitch to carry the note, and reads back as other words once the band is in
MOODS += ("gentle", "sly", "warm", "mischief", "playful", "clear")


def cast(voice, line, want=()):
    """the first of the wanted moods that reads back whole; failing that, any take that does."""
    # whisper cannot spell "Prutti", "a mail" or "gamelan"; the rest of those lines must be whole
    need = {"wise": 0.85, "hear": 0.85, "amail": 0.6, "gamelan": 0.8, "verkko": 0.6, "costco": 0.85, "laklok": 0, "bat": 0.7}.get(line, 1)   # nor "lær klokken"; "lack lock dot com" it hears right and writes "Lacklock.com", which matches no word   # nor "Verkkokauppa"
    clean = [c for c in MOODS if (voice, line, c) not in BANNED and HEARD.get(f"{voice}/{line}-{c}", {}).get("recall", 0) >= need]
    assert clean, f"no clean take of {voice}/{line}: run bin/phrases.mjs --force, then bin/recog.py takes"
    return next((c for c in want if c in clean), clean[0])


# the lead's octave under the typed hook. −1 sings A3–B4; −2 sings A2–B3, inside the clone's own speaking
# range (a candidate: AESTHETINA_LEAD_OCT=-2, mastered apart with bin/master.mjs --lyric=… --tag=low)
LEAD_OCT = int(__import__("os").environ.get("AESTHETINA_LEAD_OCT", META.get("leadOct", -1)))


def sung(bar, line, want=(), voice="jeffrey", gain=1.3, oct=None, pan=0.0, until=None):
    """one voice, alone: the line in the first clean mood of `want`. until: when the next line
    starts; the last note lets go just before it, so a line never sings over the one after."""
    if oct is None: oct = LEAD_OCT
    starts = [at(bar, st) for st in HOOK_STEPS]
    midis = [KEY[HOOK[i]] + 12 * oct for i in range(10)]
    n = NSYL[line]
    if n == 11: starts = [at(bar, -3)] + starts; midis = [KEY["a"] + 12 * oct] + midis  # a pickup syllable
    starts, midis = starts[:n], midis[:n]  # a shorter line stops on an earlier note and holds it
    end = at(bar, 30) + 0.45
    if until is not None: end = min(end, until - 0.1)
    phrase_ev(starts, voice, f"{line}-{cast(voice, line, want)}", midis, end - starts[-1], gain, "lyric", pan)


# a statement every two bars wherever the band is in, and the mood has an arc: spoken plainly
# and asked curiously as the track arrives, shouted in the drops, sad and tired when the
# drums leave, gentle at the very end. Shouting is kept for three lines of the second drop:
# the lead should stay smooth. The quiet lines are voiced (sly, warm, gentle), not whispered,
# and the reply at bar 80 is the plain take: shouted, it sank under the band
J, P = "jeffrey", "prutti"
STATEMENTS = [
    (4, "name", ("warm",), J),  # before the kick, once: quiet, with the voice on (the whispered take read as other words)
    # the opening is the two of them (@jeffrey 2026-11-20: "can the aesthetina opening be like a dialogue between prutti
    # and jeffrey"), from the first kick, a line each in turn, jeffrey a little left and Prutti a little right:
    # the two names, then where to buy the computer. The lines that opened the track before follow
    # as the first verse ("i want to buy / i need to get another computer" are out of it).
    # ("stereo l and r back and forth … can we have the narrative make more sense? and rhyme a bit?": jeffrey hard-ish
    # left, Prutti right; the two names, then the shopping — costco / verkkokauppa — and then he is typing on it)
    (8, "name", ("plain", "excited"), J), (10, "wise", ("plain",), P), (12, "costco", ("plain", "excited"), J), (14, "verkko", ("excited", "plain"), P),
    (16, "neo", ("excited", "plain"), J), (18, "bat", ("plain",), J),   # @jeffrey 2026-11-20: "laer klokken is the best chat to drain my bat"
    (20, "chubby", ("curious", "plain"), J), (22, "plug", ("plain",), J),
    (24, "sounds", ("curious", "plain"), J), (26, "letter", ("excited",), J), (28, "cables", ("deadpan", "curious"), J), (30, "sleep", ("tired", "deadpan"), J),
    # the first drop is a conversation. Prutti types in the clock chat and has no phone; the Pruttivox
    # clone is how he is heard. "where is a mail?", "you are a super star", "copy that" and "gamelan
    # tonight" are his own typed messages (chat-clock, 2026-11-13/15); "a mail moved out…" is jeffrey's reply.
    (40, "hear", ("plain", "excited"), J), (42, "wise", ("plain",), P), (44, "amail", ("clear", "plain"), P), (46, "moved", ("plain",), J),
    (48, "buy", ("excited", "plain"), J), (50, "gamelan", ("curious", "clear"), P), (52, "letter", ("excited",), J), (54, "chubby", ("plain",), P),
    (58, "cables", ("sad", "tired"), J), (60, "costco", ("sad", "plain"), J), (62, "sleep", ("sly", "warm", "tired"), J),
    # @jeffrey 2026-11-20, his words for both: "maybe ill buy the computer on amazon or costco" / "oh no i wana buy it at
    # verkokauppa!" — the pair follows the macbook neo (sung as eleven syllables each: "the computer" and "it" left out)
    (76, "neo", ("shout", "excited"), J), (78, "costco", ("excited", "plain"), J), (80, "verkko", ("excited", "plain"), P), (82, "amail", ("clear", "plain"), P),
    (84, "moved", ("plain",), J), (86, "cables", ("shout", "deadpan"), J), (88, "sleep", ("excited",), J), (90, "name", ("shout", "excited"), J),
    (94, "hand", ("gentle", "tired"), J),
]
# ── the sign-off (@jeffrey 2026-11-20: "and lets say laklok.com lack lock dot com"): four even syllables on the four
# beats of the outro's first bar, jeffrey, dry and in the middle, climbing the A chord — where the two of them meet
phrase_ev([at(92, 0), at(92, 4), at(92, 8), at(92, 12)], J, f"laklok-{cast(J, 'laklok', ('plain',))}", [57, 57, 61, 64], BEAT * 1.5, 1.4, "lyric", 0.0)


# ── behind it all: hoos and has (@jeffrey 2026-11-20: "in the background screams hoos and has"; the owls and the
# far screams are bin/background.py's, laid in by bin/render.mjs). jeffrey's own shouted syllables, quiet and out to
# the sides: a "hoo" at a phrase's end on a note of the chord's (it answers the owls, and they it), a "hah" on the
# last off-beat. None under the jeffrey/Prutti exchange (bars 40-56 and 76-86). meta "background" turns them on.
if META.get("background"):
    for j, bar in enumerate((9, 17, 29, 37, 67, 91)):
        ev(at(bar, 12), "hoo", (64, 61, 66, 64, 69, 64)[j], 0.4, None, 0.32, "toot", pan=(-0.7, 0.7)[j % 2], halo=0, vib=0.05)
    for j, bar in enumerate((13, 21, 65, 69, 87)):
        accent(at(bar, 14), "hah", "shout", 0.3, (0.7, -0.7)[j % 2], "toot")


# ── Prutti (Pruttivox IVC, eleven_v4) — DRAFT: his clone + performance need his approval before release ──
first = lambda bar, line: at(bar, -3) if NSYL[line] == 11 else at(bar)  # an eleven-syllable line has a pickup
for k, (bar, line, want, who) in enumerate(STATEMENTS):
    nxt = first(*STATEMENTS[k + 1][:2]) if k + 1 < len(STATEMENTS) else None
    if who == P: sung(bar, line, want, voice=P, gain=2.1 * 10 ** (-META.get("pruttiDown", 0) / 20), pan={10: 0.6, 14: 0.6}.get(bar, 0.3), until=nxt)  # his takes peak high and sit 4 dB under jeffrey at the same peak
    else: sung(bar, line, want, gain={4: 0.9, 58: 1.6, 60: 1.6, 94: 1.0}.get(bar, 1.3), pan={8: -0.6, 12: -0.6}.get(bar, 0.0), until=nxt)  # the opening line sits inside the music box (at 0.6 it read as other words); the sad takes are quiet at the same peak, and the piano and bells stand level with them


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
if META.get("screams") != "bank": ev(at(38), "ah", 52, BAR * 2, None, 0.9, "toot", path=tuple((j * BEAT, 52 + [0, 2, 4, 5, 7, 9, 11, 12][j]) for j in range(8)), **SCREAM)   # meta "screams": "bank" — the rise is a real scream from the frothyscreama bank instead (bin/bank-screams.py; bin/render.mjs lays it in)
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
if META.get("screams") != "bank": ev(at(74), "ah", 57, BAR * 2, None, 0.9, "toot", path=tuple((j * BEAT, 57 + [0, 2, 4, 5, 7, 9, 11, 12][j]) for j in range(8)), **SCREAM)   # meta "screams": "bank" — the rise is a real scream from the frothyscreama bank instead (bin/bank-screams.py; bin/render.mjs lays it in)
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
