# voices/generative.py — the seeded version's vocal score.
# Reads arrangements/generative.plan.json, the one plan both halves follow
# (written by `node arrangements/generative.mjs`; this file runs that if the plan
# is missing or its seed differs from generative.json). Placement rules:
#   toots     every lead statement after the intro, one vowel per letter (the
#             vowel of the letter's name), an octave under the typed notes;
#             in the peaks every other statement sings the letter names instead
#   choir     choir_block on each chord of the peaks, builds and breakdown
#   whisper   the title's letter names as the intro types them
#   shout     yee-haw on the last bar before each peak
#   scream    extended screams opening both peaks, one rising over the rise
#   angry     the chord word's first letter name on the peaks' chord changes
#   breathy   a breath out every four bars of the breakdown

_ARR_DIR = HERE.parent / "arrangements"
_plan_path = _ARR_DIR / "generative.plan.json"
_meta = json.load(open(_ARR_DIR / "generative.json"))
if not _plan_path.exists() or json.load(open(_plan_path))["seed"] != _meta["seed"]:
    subprocess.run(["node", str(_ARR_DIR / "generative.mjs")], check=True)
PLAN = json.load(open(_plan_path))
if PLAN["bars"] != BARS:
    print(f"generative: plan has {PLAN['bars']} bars, voice.py sized for {BARS}; rerun after the plan settles")

# the vowel in each letter's name, and the name itself as a library syllable
LETTER_VOWEL = dict(a="ay", b="ee", c="ee", d="ee", e="ee", f="eh", g="ee", h="ay", i="ai", j="ay", k="ay", l="eh",
                    m="eh", n="eh", o="oh", p="ee", q="oo", r="ah", s="eh", t="ee", u="oo", v="ee", w="oo", x="eh",
                    y="ai", z="ee")
NAME = dict(a="ay", b="bee", c="see", d="dee", e="ee", f="ehf", g="jee", h="aych", i="ai", j="jay", k="kay", l="ehl",
            m="ehm", n="ehn", o="oh", p="pee", q="kyoo", r="ahr", s="ehs", t="tee", u="yoo", v="vee", w="wuh",
            x="ehks", y="wai", z="zee")
has = lambda name, style=None: (LIB.parent / (f"syllables-{style}" if style else "syllables") / f"{name}.mp3").exists()


def word_line(bar, word, names, gain=1.0):
    r = PLAN["rhythms"][word]
    steps = r["steps"] + [r["n"] - 2]
    prev = None
    for i, c in enumerate(word):
        m = KEY[c] - 12
        gap = (steps[i + 1] - steps[i]) * S16
        hold = gap + 0.4 if i == len(word) - 1 else gap * 0.95
        nm = names(c)
        if not has(nm): nm = LETTER_VOWEL[c]
        ev(at(bar, steps[i]), nm, m, hold, prev, gain, "toot", decay=0.35, floor=0.2)
        prev = m


tonic_midi = 45 + (PLAN["key"]["root"] - 9) % 12 + 12  # the tonic, A3-ish
scale = [0, 2, 4, 5, 7, 9, 11] if PLAN["key"]["quality"] == "maj" else [0, 2, 3, 5, 7, 8, 10]
secs = PLAN["sections"]

for k, sec in enumerate(secs):
    kind, s0, n = sec["kind"], sec["start"], sec["len"]
    end = s0 + n
    peak = kind in ("peak", "peak2")
    span = PLAN["rhythms"][sec["lead"]]["n"] // 16
    L = sec["layers"]

    # intro: whisper the title's letter names as they're typed
    if kind == "intro":
        for rel in range(0, n, span):
            r = PLAN["rhythms"][sec["lead"]]
            for i, c in enumerate(sec["lead"]):
                if has(NAME[c], "whisper"): accent(at(s0 + rel, r["steps"][i]), NAME[c], "whisper", 0.5, [-0.4, 0.4][i % 2], "toot")
        continue

    # toots (and letter names in the peaks) on the lead statements
    if L["toots"]:
        slow = kind == "breakdown"
        j = 0
        for rel in range(0, n - (0 if kind == "outro" else 1), span * (2 if slow else 1)):
            if kind == "build" and rel < n // 2: continue
            names = (lambda c: NAME[c]) if peak and j % 2 else (lambda c: LETTER_VOWEL[c])
            if not slow: word_line(s0 + rel, sec["lead"], names, 0.9 if peak else 0.8)
            j += 1

    # choir on the chords
    if L["choir"]:
        cb = sec["chordBars"] if not kind == "breakdown" else 2
        for rel in range(0, n - cb + 1, cb):
            word = sec["prog"][(rel // sec["chordBars"]) % 4]
            choir_block(s0 + rel, cb, word, sec["vowel"], 2 if kind == "breakdown" else 1, 0.9 if peak else 0.75)

    # yee-haw into each peak
    if k + 1 < len(secs) and secs[k + 1]["kind"] in ("peak", "peak2"):
        accent(at(end - 1, 12), "yee", "shout", 0.8, -0.2, "toot")
        accent(at(end - 1, 14), "haw", "shout", 0.9, 0.2, "toot")

    # screams
    if kind == "peak":
        ev(at(s0), "yah", tonic_midi, BAR * 1.6, tonic_midi - 7, 1.1, "toot", **SCREAM)
        ev(at(s0 + n // 2), "ay", tonic_midi + 7, BAR * 1.4, tonic_midi, 1.0, "toot", **SCREAM)
    if kind == "peak2":
        ev(at(s0), "ay", tonic_midi + 7, BAR * 1.8, tonic_midi, 1.15, "toot", **SCREAM)
        ev(at(s0 + n // 2), "yah", tonic_midi + 12, BAR * 1.6, tonic_midi + 5, 1.1, "toot", **SCREAM)
        ev(at(end - 2, 8), "yah", tonic_midi + 12, BAR * 1.4, tonic_midi + 7, 1.0, "toot", **SCREAM)
    if kind == "rise":
        ev(at(end - 2), "ah", tonic_midi - 5, BAR * 2, None, 0.9, "toot",
           path=tuple((j * BEAT, tonic_midi - 5 + ([*scale, 12] * 2)[j] + (0 if j < 8 else 12)) for j in range(8)), **SCREAM)

    # angry letter names on the chord changes
    if peak:
        every = 1 if kind == "peak2" else 2
        for ci, rel in enumerate(range(0, n, sec["chordBars"])):
            if ci % every: continue
            word = sec["prog"][ci % 4]
            if has(NAME[word[0]], "angry"): accent(at(s0 + rel), NAME[word[0]], "angry", 0.6, [-0.3, 0.3][ci % 2])

    # breaths in the breakdown
    if kind == "breakdown":
        for rel in range(2, n, 4): accent(at(s0 + rel, 12), "hah", "breathy", 0.45, 0.0, "toot")

    # outro: the last word, whispered after the music
    if kind == "outro":
        last = sec["lead"][-1]
        if has(NAME[last], "whisper"): accent(at(end - 1, 4), NAME[last], "whisper", 0.6, 0.0, "toot")
