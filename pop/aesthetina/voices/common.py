# voices/common.py — helpers every vocal score shares (exec'd into voice.py)
HOOK = "aesthetina"; HOOK_STEPS = [0, 3, 6, 8, 10, 12, 14, 16, 19, 22]
TOOT_V = ["ah", "eh", "eh", "eh", "eh", "eh", "ee", "ee", "ah", "ah"]
CHOIR = ["ay", "ee", "ehs", "tee", "aych", "ee", "tee", "ee", "ehn", "ay"]  # the letters' own names


def hook_line(bar, names, gain, bus, emph=None, stacc=(), oct=-1, end_chord=None, **o):
    """emph: {syllable index: gain multiplier} — those notes also start clean, no slide in.
    stacc: syllable indexes sung short and detached (a gap before the next).
    oct: octaves from the typed pitch. end_chord: the last held syllable climbs that chord in 8ths."""
    steps = HOOK_STEPS + [30]; prev = None; emph = emph or {}
    for i, nm in enumerate(names):
        mid = KEY[HOOK[i]] + 12 * oct
        gap = (steps[i + 1] - steps[i]) * S16
        hold = gap + 0.4 if i == 9 else gap * (0.55 if i in stacc else 0.95)  # legato unless detached
        extra = {"rel": 0.06} if i in stacc else {}
        if i == 9 and end_chord:  # arpeggiate inside the held last vowel: up the chord and back
            up = [m for m in tones(end_chord, mid + 1, mid + 14)][:3]
            seq = [mid] + up + up[-2::-1]
            extra["path"] = tuple((j * 2 * S16, m) for j, m in enumerate(seq))
        ev(at(bar, steps[i]), nm, mid, hold, None if i in emph else prev, gain * emph.get(i, 1), bus, decay=0.35, floor=0.2,
           **extra, **o)
        prev = mid


KONNA = [("tah", 0), ("dee", 1), ("gee", 2), ("nah", 3), ("toh", 4), ("tah", 5), ("kee", 6), ("tah", 7),
         ("tah", 8), ("kee", 9), ("tah", 10), ("tah", 11), ("dee", 12), ("gee", 13), ("nah", 14), ("toh", 15)]


# chord words sung as one syllable on their triad, three voices 0/28/56 ms
SUNG = {"tea": "tee", "daw": "dah", "rib": "ree", "ace": "ays", "fad": "fah", "big": "bee", "taw": "tah"}


def chord_word(bar, word, hold):
    for i, ch in enumerate(word):
        ev(at(bar) + i * 0.028, SUNG[word], KEY[ch] - 12, hold, None, 0.4, "chord", halo=0.1, vib=0.08,
           cents=[6, -7, 0][i])


# ── the aesthetivox choir: three stretched vowels arpeggiating inside ───────
# Each voice holds one vowel for the whole block while its pitch walks the
# chord: low in quarters, mid in 8ths, high in dotted 8ths (3 against 4).
def tones(word, lo, hi):
    pcs = {KEY[c] % 12 for c in word}
    return [m for m in range(lo, hi + 1) if m % 12 in pcs]


def choir_block(bar, nbars, word, vowel, slow=1.0, gain=1.0):
    dur = nbars * BAR
    voices = [((43, 52), BEAT * 1.0, "updown", -0.55, 0.36), ((52, 60), BEAT * 0.5, "up", 0.0, 0.3),
              ((57, 67), BEAT * 0.75, "down", 0.55, 0.26)]
    for (lo, hi), rate, shape, pan, g in voices:
        tn = tones(word, lo, hi)
        cyc = {"up": tn, "down": tn[::-1], "updown": tn + tn[-2:0:-1]}[shape]
        r = rate * slow; steps = int(dur / r)
        path = tuple((j * r, cyc[j % len(cyc)]) for j in range(steps))
        ev(at(bar), vowel, path[0][1], dur, None, g * gain, "choir", path=path, toot=False, halo=0.1, vib=0.06,
           rel=0.45, pan=pan, decay=rate * slow * 1.2, floor=0.35)



# ── accents: the inflection banks, raw, at their own pitch ──────────────────
# whisper / breathy / shout / angry / scream (pop/bin/phoneme-library.mjs --style)
accents = []  # (time, name, style, gain, pan, bus)


def accent(t, name, style, gain=1.0, pan=0.0, bus="dry"):
    accents.append((t, name, style, gain, pan, bus))


def raw_clip(name, style):
    p = LIB.parent / f"syllables-{style}" / f"{name}.mp3"
    if not p.exists(): return None
    x = np.frombuffer(subprocess.run(["ffmpeg", "-loglevel", "error", "-i", str(p), "-f", "f32le", "-ac", "1", "-ar", str(FS), "-"],
                                     capture_output=True, check=True).stdout, dtype=np.float32).astype(np.float64)
    return x / (np.abs(x).max() + 1e-9)


LETTER_NAME = dict(a="ay", e="ee", s="ehs", t="tee", h="aych", i="ai", n="ehn", d="dee", u="yoo", o="oh")
SCREAM = dict(style="scream", toot=False, halo=0.25, vib=0.3, rel=0.6)
