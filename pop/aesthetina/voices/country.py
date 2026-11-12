# voices/country.py — the square-dance version's vocal score (80 bars)
for bar in [12, 14, 16, 18, 20, 22, 24, 26, 56, 58, 62, 64, 66, 70]: hook_line(bar, TOOT_V, 1.0, "toot")
for bar in [60, 68]: hook_line(bar, CHOIR, 1.0, "toot")

# lift-off scat (ba / ka / tra / a)
SCAT = [(0, "bah", 57, .10, None), (3, "kah", 57, .06, None), (6, "trah", 57, .10, 52), (8, "ah", 60, .18, None),
        (11, "kah", 57, .06, None), (14, "trah", 64, .08, 57), (16, "bah", 57, .10, None), (18, "ah", 64, .10, None),
        (19, "kah", 57, .06, None), (22, "trah", 57, .10, 52), (24, "bah", 60, .08, None), (26, "kah", 57, .06, None),
        (27, "ah", 64, .12, None), (30, "trah", 69, .08, 57)]
for bar in [8, 10]:
    for st, nm, m, h, sl in SCAT: ev(at(bar, st), nm, m, h * 1.6, sl, 0.55 if st % 8 else 0.7, "dry", halo=0)

# stillness: the vowels on the aeiou pitches, a bar each
prev = None
for i, (nm, ch) in enumerate(zip(["ah", "eh", "ee", "oh", "oo"], "aeiou")):
    ev(at(29 + i), nm, KEY[ch] - 12, BAR * 1.0, prev, 0.9, "toot", toot=False, vib=0.18, rel=0.6)
    prev = KEY[ch] - 12

# B: konnakol 5+3+3+5 on the odd bars; TOM carries the accent
for bar in list(range(37, 44, 2)) + list(range(45, 52, 2)):
    for nm, st in KONNA:
        tom = st in (4, 15)
        ev(at(bar, st), nm, 64 if tom else 57, 0.28 if tom else 0.1, None, 0.75 if tom else 0.45, "dry", halo=0, vib=0)

for j, bar in enumerate(range(36, 44, 2)): chord_word(bar, ["tea", "daw", "rib", "ace"][j], BAR * 1.95)
for j, bar in enumerate(range(44, 52)): chord_word(bar, ["tea", "daw", "rib", "ace"][j % 4], BAR * 0.97)
for j, bar in enumerate(range(64, 72, 2)): chord_word(bar, ["ace", "fad", "big", "tea"][j], BAR * 1.95)

# outro: adieu, sung as its letters
prev = None
for st, nm, ch, h in [(0, "ay", "a", .45), (4, "dee", "d", .45), (8, "ee", "i", .45), (12, "ee", "e", .9), (20, "yoo", "u", 2.6)]:
    ev(at(76, st), nm, KEY[ch] - 12, h, prev, 0.9, "toot", toot=(nm in VOWELS))
    prev = KEY[ch] - 12

CHOIR_PLAN = [(12, 2, "tea", "oo", 1, .6), (14, 2, "tea", "oo", 1, .7)]
CHOIR_PLAN += [(16 + 2 * j, 2, w, "ah", 1, 1) for j, w in enumerate(["tea", "taw", "tea", "daw", "tea", "taw"])]
CHOIR_PLAN += [(28 + 2 * j, 2, "tea", "oh", 2, .9) for j in range(4)]
CHOIR_PLAN += [(36 + 2 * j, 2, w, "ah", 1, .9) for j, w in enumerate(["tea", "daw", "rib", "ace"])]
CHOIR_PLAN += [(44 + j, 1, ["tea", "daw", "rib", "ace"][j % 4], "ee", 1, .8) for j in range(8)]
CHOIR_PLAN += [(52, 4, "tea", "oo", 2, .8)]
CHOIR_PLAN += [(56 + 2 * j, 2, w, "ah", 1, 1) for j, w in enumerate(["tea", "taw", "tea", "taw", "ace", "fad", "big", "tea"])]
CHOIR_PLAN += [(72, 4, "tea", "oo", 1, .7), (76, 4, "tea", "oo", 2, .6)]
for bar, nb, w, v, sl, g in CHOIR_PLAN: choir_block(bar, nb, w, v, sl, g)

# boot: the hook's letters whispered as they're typed (from bar 2), then again in the prepare
for bar in (2, 4, 6, 52):
    for i, st in enumerate(HOOK_STEPS):
        accent(at(bar, st), LETTER_NAME[HOOK[i]], "whisper", 0.5, [-0.4, 0.4][i % 2], "toot")
# stillness: a breath out between the vowels
for i in range(5): accent(at(29 + i, 12), "hah", "breathy", 0.45, 0.0, "toot")
# konnakol TOMs shouted on the last odd bar of each half
for bar in (43, 51):
    for st in (4, 15): accent(at(bar, st), "toh", "shout", 0.9, 0.0)
# B2: angry hits on each chord change, the chord word's first syllable
for j, bar in enumerate(range(44, 52)):
    accent(at(bar), {"tea": "tee", "daw": "dah", "rib": "ree", "ace": "ay"}[["tea", "daw", "rib", "ace"][j % 4]], "angry", 0.6, [-0.3, 0.3][j % 2])
# the drop: screams on its two downbeats, angry kah on every 4th bar's last beat
# extended screams: the scream bank through the aesthetivox stretch, sliding up into the note
ev(at(56), "yah", 57, BAR * 1.6, 50, 1.1, "toot", **SCREAM)
ev(at(64), "ay", 64, BAR * 1.6, 57, 1.1, "toot", **SCREAM)
ev(at(70, 8), "yah", 69, BAR * 1.4, 57, 1.0, "toot", **SCREAM)
# over the tuplet ladder: one scream rising a fifth across two bars into the drop
ev(at(54), "ah", 52, BAR * 2, None, 0.9, "toot", path=tuple((j * BEAT, 52 + [0, 2, 4, 5, 7, 9, 11, 12][j]) for j in range(8)), **SCREAM)
for bar in range(59, 72, 4): accent(at(bar, 12), "kah", "angry", 0.7, 0.25)
# square-dance calls: yee-haw into the big sections
for bar in (16, 36, 56, 72):
    accent(at(bar - 1, 12), "yee", "shout", 0.8, -0.2, "toot")
    accent(at(bar - 1, 14), "haw", "shout", 0.9, 0.2, "toot")
# outro: the last word whispered after the music
accent(at(79, 4), "yoo", "whisper", 0.6, 0.0, "toot")
