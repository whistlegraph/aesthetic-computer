# voices/callresponse.py — the caller (152 bars). Pairs with arrangements/callresponse.mjs:
# CALLS, CALL_STEPS and the word → chord map are copied from there and must match.
#
# Each call word is sung as library syllables on the word's own notepat pitches
# (one octave down); inside each syllable the pitch walks through its letters.
# Shout/angry banks carry the calls through WORLD, so they stay hollered but in tune.

CHORD_OF = dict(swing="tea", dosido="daw", allemande="ace", promenade="big", circle="fad", home="tea", yeehaw="tea")
SYLS = {
    "swing": [("swih", 0, 3), ("ihng", 3, 5)],
    "dosido": [("doh", 0, 2), ("see", 2, 4), ("doh", 4, 6)],
    "allemande": [("ah", 0, 1), ("leh", 1, 4), ("mah", 4, 6), ("ahnd", 6, 9)],
    "promenade": [("proh", 0, 3), ("meh", 3, 5), ("nay", 5, 7), ("ayd", 7, 9)],
    "circle": [("ser", 0, 3), ("kuh", 3, 5), ("uhl", 5, 6)],
    "home": [("hoh", 0, 2), ("ohm", 2, 4)],
    "yeehaw": [("yee", 0, 3), ("haw", 3, 6)],
}


def call_steps(n):
    return [2 * i if n <= 6 else int(i * 14 / (n - 1) + 0.5) for i in range(n)]


def has(name, style):
    d = LIB if not style else LIB.parent / f"syllables-{style}"
    return (d / f"{name}.mp3").exists()


def call(bar, word, style="shout", gain=1.0, slow=1, bus="dry"):
    st = call_steps(len(word))
    T = lambda k: at(bar, st[k] * slow)
    for j, (syl, a, b) in enumerate(SYLS[word]):
        t0 = T(a)
        end = T(b) - 0.02 if b < len(word) else T(len(word) - 1) + 0.45 * slow
        path = tuple((T(k) - t0, KEY[word[k]] - 12) for k in range(a, b))
        s = style if style and has(syl, style) else None
        ev(t0, syl, path[0][1], max(0.08, end - t0), None, gain * (1.0 if s else 0.85), bus, path=path,
           style=s, toot=False, halo=0.15, vib=0.1, rel=0.3 if slow == 1 else 0.7)


# the calls (copied from arrangements/callresponse.mjs)
CALLS = [(6, "yeehaw"), (8, "swing"), (12, "dosido"), (16, "swing"), (20, "home"),
         (24, "allemande"), (28, "promenade"), (32, "circle"), (36, "dosido"), (40, "promenade"),
         (66, "home"),
         (72, "swing"), (73, "promenade"), (76, "allemande"), (77, "home"), (80, "dosido"), (81, "circle"),
         (84, "promenade"), (85, "home"), (88, "yeehaw"), (89, "swing"), (92, "allemande"), (93, "promenade"),
         (140, "home"), (144, "home")]
CANON_CYCLE = ["swing", "dosido", "allemande", "promenade", "circle", "home"]
CALLS += [(bar, CANON_CYCLE[(bar - 104) % 6]) for bar in range(104, 136)]


def section(bar):
    for lim, name in [(8, "tune"), (24, "set1"), (40, "set2"), (48, "bow"), (64, "fiddle"), (72, "home"),
                      (96, "set3"), (104, "hush"), (136, "canon")]:
        if bar < lim: return name
    return "out"


for bar, word in CALLS:
    s = section(bar)
    if word == "yeehaw": call(bar, word, "shout", 1.0)
    elif s == "set1": call(bar, word, "shout", 0.95)
    elif s == "set2": call(bar, word, ["angry", "shout"][(bar // 4) % 2], 1.0)
    elif s == "bow": call(bar, word, None, 0.9, bus="toot")
    elif s == "home": call(bar, word, None, 0.9, slow=2, bus="toot")
    elif s == "set3": call(bar, word, "shout" if bar % 2 == 0 else "angry", 1.0)
    elif s == "canon": call(bar, word, ["shout", "angry"][bar % 2], 0.9)
    else: call(bar, word, None, 0.8, bus="toot")

# the arpeggio choir under the bow, home, the canon and the way out
CHOIR = [(40, 2, "big", "oo", 1, .7), (42, 2, "big", "oo", 1, .7), (44, 2, "tea", "oo", 1, .7), (46, 2, "tea", "oo", 1, .7),
         (64, 4, "tea", "oh", 2, .8), (68, 4, "tea", "oh", 2, .8)]
CHOIR += [(bar, 2, CHORD_OF[CANON_CYCLE[(bar - 104) % 6]], "ah", 1, .9) for bar in range(104, 136, 2)]
CHOIR += [(136, 4, "tea", "oo", 1, .6), (140, 4, "tea", "oo", 2, .5), (144, 4, "tea", "oo", 2, .5)]
for bar, nb, w, v, sl, g in CHOIR: choir_block(bar, nb, w, v, sl, g)

# screams: the hush climbs into the canon; the canon peaks every 8 bars
ev(at(102), "ah", 52, BAR * 2, None, 0.9, "toot", path=tuple((j * BEAT, 52 + [0, 2, 4, 5, 7, 9, 11, 12][j]) for j in range(8)), **SCREAM)
ev(at(104), "yah", 57, BAR * 1.6, 50, 1.1, "toot", **SCREAM)
ev(at(112), "ay", 64, BAR * 1.6, 57, 1.1, "toot", **SCREAM)
ev(at(120), "yah", 69, BAR * 1.6, 57, 1.1, "toot", **SCREAM)
ev(at(128), "ah", 57, BAR * 2, None, 1.0, "toot", path=tuple((j * BEAT, 57 + [0, 4, 7, 12, 7, 4, 0, -5][j]) for j in range(8)), **SCREAM)

# yee-haw into the big sections; angry kah on the fiddle's offbeat bars
for bar in (24, 48, 72, 104, 136):
    accent(at(bar - 1, 12), "yee", "shout", 0.8, -0.2, "toot")
    accent(at(bar - 1, 14), "haw", "shout", 0.9, 0.2, "toot")
for bar in (48, 52, 56, 60): accent(at(bar), "swih", "angry", 0.8, -0.25)
for bar in range(49, 64, 2): accent(at(bar, 12), "kah", "angry", 0.6, 0.3)
# home: a breath between the slow calls; the way out ends whispered
for bar in (65, 69): accent(at(bar, 8), "hah", "breathy", 0.5, 0.0, "toot")
accent(at(148), "yee", "whisper", 0.6, -0.2, "toot")
accent(at(148, 4), "haw", "whisper", 0.6, 0.2, "toot")
