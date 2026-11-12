#!/usr/bin/env python3
"""aesthetina — jeffrey's whole vocal part, from the phoneme library.

Every event is one library syllable on one note (or a slide into it). Bare
vowels skip to the steady part; syllables with a consonant keep it raw and
unpitched as a pickup, so the vowel lands on the grid. The vowel stretches to
hold the note (frame interpolation) and WORLD replaces f0 with the note,
sliding from the previous one. See ../ARRANGEMENT.md § Voice.

The score itself lives in voices/<arrangement>.py (helpers in voices/common.py);
meta (bars, bpm, swing) in arrangements/<arrangement>.json.
Writes out/vocal-<arrangement>.wav (country: vocal-stem.wav) on render.mjs's timeline.
usage: pop/.venv/bin/python pop/aesthetina/bin/voice.py [--arr=country]
"""
import json, subprocess, sys
from pathlib import Path
import numpy as np, soundfile as sf, pyworld as pw
from scipy.signal import resample_poly

HERE = Path(__file__).resolve().parent
OUT = HERE.parent / "out"
LIB = Path(__file__).resolve().parents[3] / "aesthetic-computer-vault/voice/jeffrey-phonemes/syllables"
FS, FP = 22050, 5.0
ARR = next((a[6:] for a in sys.argv if a.startswith("--arr=")), "country")
META = json.load(open(HERE.parent / "arrangements" / f"{ARR}.json"))
BPM, BARS, SWING = META.get("bpm", 128), META["bars"], META.get("swing", 0.62)
BEAT = 60 / BPM; BAR = BEAT * 4; S16 = BEAT / 4
LEN_S = BARS * BAR + 3
VOWELS = {"ah", "eh", "ee", "oh", "oo", "ay", "uh", "aw", "ow", "ai", "oy", "ih", "er"}

# notepat's map, as MIDI (disks/notepat.mjs NOTE_TO_KEYBOARD_KEY)
KEY = dict(z=58, x=59, c=60, v=61, d=62, s=63, e=64, f=65, w=66, g=67, r=68, a=69, q=70, b=71,
           h=72, t=73, i=74, y=75, j=76, k=77, u=78, l=79, o=80, m=81, p=82, n=83)
FR = lambda sec: int(round(sec * 1000 / FP))
# lyric syllables whose closing consonant should stay crisp (seconds kept at spoken speed)
CODA = {"will": 0.14, "please": 0.24, "hand": 0.14, "that": 0.1, "kum": 0.1, "want": 0.14, "need": 0.12, "get": 0.1, "there": 0.12,
        "pass": 0.2, "bul": 0.12, "char": 0.12, "jing": 0.14, "chub": 0.07}

_clips = {}


def clip(name, style=None, voice=None):
    key = (name, style, voice)
    if key in _clips: return _clips[key]
    src = LIB if not style else LIB.parent / f"syllables-{style}"
    if not style and not (src / f"{name}.mp3").exists(): src = LIB.parent / "words"  # lyric syllables
    if voice: src = LIB.parents[1] / f"{voice}-phonemes" / "words"  # another singer (prutti: draft-only until he approves)
    raw = subprocess.run(["ffmpeg", "-loglevel", "error", "-i", str(src / f"{name}.mp3"), "-f", "f32le", "-ac", "1", "-ar", str(FS), "-"],
                         capture_output=True, check=True).stdout
    x = np.frombuffer(raw, dtype=np.float32).astype(np.float64)
    f0, t = pw.harvest(x, FS, f0_floor=60, f0_ceil=300 if not style else 600, frame_period=FP)
    f0 = pw.stonemask(x, f0, t, FS)
    sp = pw.cheaptrick(x, f0, t, FS); ap = pw.d4c(x, f0, t, FS)
    v = np.where(f0 > 0)[0]
    first, last = (int(v[0]), int(v[-1])) if len(v) else (0, len(f0) - 1)
    e = np.log(sp.sum(1) + 1e-12)
    if name in CODA: last = len(f0) - 1  # a word keeps its whole ending (the z of please)
    else:
        while last > first + 10 and e[last] < e[first:last].max() - 5: last -= 1
    _clips[key] = dict(sp=sp, ap=ap, voiced=f0 > 0, first=first, last=last)
    return _clips[key]


def interp(arr, a, b, n):
    src = np.linspace(a, b - 1, n)
    lo = np.floor(src).astype(int); hi = np.minimum(lo + 1, b - 1); w = (src - lo)[:, None]
    return arr[lo] * (1 - w) + arr[hi] * w


def sing(name, midi, hold, slide_from=None, toot=True, halo=0.2, vib=0.12, rel=0.22, path=None, decay=None, floor=0.2, style=None, voice=None):
    """returns (pickup seconds before the vowel, samples)."""
    c = clip(name, style, voice)
    if name in VOWELS:
        a = min(c["first"] + 14, c["last"] - 8)
        sp0, ap0, voiced0, v0 = c["sp"][a:c["last"] + 1], c["ap"][a:c["last"] + 1], None, 0
        keep = 0
    else:
        sp0, ap0 = c["sp"][:c["last"] + 1], c["ap"][:c["last"] + 1]
        v0 = c["first"]; keep = min(len(sp0) - 2, v0 + 8)
    # a word's closing consonant (CODA seconds) keeps its spoken speed and ends the note;
    # only the vowel between onset and coda stretches
    nc = min(FR(CODA.get(name, 0)), max(0, len(sp0) - keep - 4))
    end = len(sp0) - nc
    n_nuc = max(4, FR(hold) - nc if nc else FR(hold + rel))  # an open vowel keeps sounding through its release
    nuc_sp = np.exp(interp(np.log(sp0 + 1e-16), keep, end, n_nuc))
    nuc_ap = np.clip(interp(ap0, keep, end, n_nuc) + 0.08, 0, 1)
    sp = np.maximum(np.vstack([sp0[:keep], nuc_sp, sp0[end:]]), 1e-12)
    ap = np.vstack([ap0[:keep], nuc_ap, ap0[end:]])
    n = len(sp)
    voiced = np.arange(n) >= v0
    if name not in VOWELS: voiced[:keep] |= c["voiced"][:keep]
    if nc: voiced[n - nc:] = c["voiced"][end:end + nc]
    k = (np.arange(n) - v0) * FP / 1000
    start = slide_from if slide_from is not None else midi - 0.5
    g = 0.07 * (1 + abs(midi - start) / 12)
    x = np.clip(k / g, 0, 1); m = start + (midi - start) * (1 - (1 - x) ** 3)
    if path:  # arpeggiate inside the held vowel: glide 45 ms into each (t, midi)
        ts = np.array([p[0] for p in path]); ms = np.array([p[1] for p in path], float)
        idx = np.clip(np.searchsorted(ts, k, side="right") - 1, 0, len(ts) - 1)
        prev = np.where(idx > 0, ms[np.maximum(idx - 1, 0)], ms[0] - 0.5)
        x = np.clip((k - ts[idx]) / 0.045, 0, 1)
        m = prev + (ms[idx] - prev) * (1 - (1 - x) ** 3)
    m = m + vib * np.clip((k - 0.15) / 0.2, 0, 1) * np.sin(2 * np.pi * 5.2 * k)
    f0 = np.where(voiced, 440 * 2 ** ((m - 69) / 12), 0.0)
    sp, ap = np.ascontiguousarray(sp), np.ascontiguousarray(ap)
    y = pw.synthesize(f0, sp, ap, FS, FP)
    if halo:
        sp_h = np.where(voiced[:, None], sp, sp * 1e-4)
        y = y + halo * pw.synthesize(f0 * 2, np.ascontiguousarray(sp_h), np.ascontiguousarray(np.clip(ap + 0.15, 0, 1)), FS, FP)[: len(y)]
    s = np.arange(len(y)) / FS; tail = (keep + n_nuc + nc) * FP / 1000 if nc else (keep + FR(hold)) * FP / 1000
    env = np.where(s < tail, 1, np.exp(-(s - tail) / (rel / 3)))
    if decay:  # pluck: decay from the vowel onset, re-struck at each arpeggio note
        sv = np.maximum(0, s - v0 * FP / 1000)
        if path:
            ts = np.array([p[0] for p in path]); sv = sv - ts[np.clip(np.searchsorted(ts, sv, side="right") - 1, 0, len(ts) - 1)]
        env = env * (floor + (1 - floor) * np.exp(-sv / decay))
    if name in VOWELS:
        env *= np.minimum(1, s / 0.018) ** 1.5
        if toot: env *= 1 + 0.25 * np.exp(-s / 0.05)
    y *= env
    assert np.isfinite(y).all(), name
    return v0 * FP / 1000, y / (np.abs(y).max() + 1e-9)



# ── sung phrases: a whole spoken line stretched onto its notes (the aesthetivox run law) ──
# phrases.mjs renders the line with character timings → syllable spans. Each syllable's
# note lands on its vowel onset (first voiced frame). Between two vowel onsets the
# source holds the syllable's voiced run, then the next syllable's unvoiced onset:
# unvoiced runs keep their spoken length, voiced runs share one stretch ratio so the
# span fills the note's slot. One continuous WORLD synthesis per line, f0 replaced by
# the notes (monotone 40 ms glides), octave halo on voiced frames only.
_phr = {}


def phrase_analysis(voice, slug):
    key = (voice, slug)
    if key in _phr: return _phr[key]
    d = LIB.parents[1] / f"{voice}-phonemes" / "phrases"
    meta = json.load(open(d / f"{slug}.json"))
    raw = subprocess.run(["ffmpeg", "-loglevel", "error", "-i", str(d / f"{slug}.mp3"), "-f", "f32le", "-ac", "1", "-ar", str(FS), "-"],
                         capture_output=True, check=True).stdout
    x = np.frombuffer(raw, dtype=np.float32).astype(np.float64)
    f0, t = pw.harvest(x, FS, f0_floor=60, f0_ceil=320, frame_period=FP)
    f0 = pw.stonemask(x, f0, t, FS)
    sp = pw.cheaptrick(x, f0, t, FS); ap = pw.d4c(x, f0, t, FS)
    v = f0 > 0
    i = 0  # close voicing holes shorter than 30 ms
    while i < len(v):
        if not v[i]:
            j = i
            while j < len(v) and not v[j]: j += 1
            if 0 < i and j < len(v) and (j - i) * FP < 30: v[i:j] = True
            i = j
        else: i += 1
    _phr[key] = dict(sp=sp, ap=ap, v=v, syl=[(FR(a), FR(b)) for a, b in meta["syllables"]], n=len(f0))
    return _phr[key]


def sing_phrase(voice, slug, starts, midis, end_hold, halo=0.15, vib=0.1):
    """starts: note times (s, relative to the first); returns (pre-roll s, samples)."""
    A = phrase_analysis(voice, slug); v = A["v"]; n = A["n"]; syl = A["syl"]
    on = []
    for a, b in syl:  # vowel onset = first voiced frame in the syllable
        k = next((f for f in range(a, min(b + 1, n)) if v[f]), a)
        on.append(min(k, n - 1))
    last_end = min(n, syl[-1][1] + 6)
    while last_end > on[-1] + 4 and not v[last_end - 1] and last_end - syl[-1][1] > 0: last_end -= 1
    last_end = min(n, max(last_end, syl[-1][1]) + 4)  # keep the final coda (the s of "please")
    pos, f0 = [], []
    pre = list(range(max(0, syl[0][0] - 2), on[0]))  # first syllable's unvoiced onset, raw
    pos += pre; f0 += [0.0] * len(pre)
    slots = [starts[i + 1] - starts[i] for i in range(len(starts) - 1)] + [end_hold]
    prev_m = midis[0] - 0.4
    for i, L in enumerate(slots):
        a, b = on[i], (on[i + 1] if i + 1 < len(on) else last_end)
        frames = list(range(a, max(a + 1, b)))
        T = max(2, FR(L))
        U = sum(1 for f in frames if not v[f]); V = len(frames) - U
        r = (T - U) / V if V else 1.0
        if V == 0 or r < 0.35:  # too tight: scale the whole span uniformly
            seg = list(np.linspace(a, b - 1, T))
        else:
            seg = []
            j = 0
            while j < len(frames):
                k = j
                while k < len(frames) and v[frames[k]] == v[frames[j]]: k += 1
                run = frames[j:k]
                if v[run[0]]: seg += list(np.linspace(run[0], run[-1] + 0.999, max(1, int(round(len(run) * r)))))
                else: seg += run
                j = k
        if len(seg) != T:  # every span lands exactly on its slot, so no drift accumulates
            seg = list(np.interp(np.linspace(0, len(seg) - 1, T), np.arange(len(seg)), seg))
        m = midis[i]
        for q, src in enumerate(seg):
            tq = q * FP / 1000
            x = min(1, tq / 0.04); mm = prev_m + (m - prev_m) * (1 - (1 - x) ** 3)
            mm += vib * min(1, max(0, (tq - 0.2) / 0.2)) * np.sin(2 * np.pi * 5.2 * tq)
            pos.append(src); f0.append(440 * 2 ** ((mm - 69) / 12) if v[min(int(src), n - 1)] else 0.0)
        prev_m = m
    pos = np.clip(np.array(pos, float), 0, n - 1.001)
    lo = np.floor(pos).astype(int); hi = np.minimum(lo + 1, n - 1); w = (pos - lo)[:, None]
    sp = np.exp(np.log(A["sp"][lo] + 1e-16) * (1 - w) + np.log(A["sp"][hi] + 1e-16) * w)
    ap = A["ap"][lo] * (1 - w) + A["ap"][hi] * w
    f0 = np.array(f0); sp = np.ascontiguousarray(np.maximum(sp, 1e-12)); ap = np.ascontiguousarray(np.clip(ap, 0, 1))
    y = pw.synthesize(f0, sp, ap, FS, FP)
    if halo:
        sph = np.where((f0 > 0)[:, None], sp, sp * 1e-4)
        y = y + halo * pw.synthesize(f0 * 2, np.ascontiguousarray(sph), np.ascontiguousarray(np.clip(ap + 0.15, 0, 1)), FS, FP)[: len(y)]
    tail = int(0.08 * FS); y[-tail:] *= np.linspace(1, 0, tail)
    assert np.isfinite(y).all(), slug
    return len(pre) * FP / 1000, y / (np.abs(y).max() + 1e-9)


phrases = []  # (t of first note, voice, slug, starts (abs), midis, end_hold, gain, bus, pan)


def phrase_ev(starts, voice, slug, midis, end_hold, gain=1.0, bus="lyric", pan=0.0):
    phrases.append((starts, voice, slug, midis, end_hold, gain, bus, pan))


# ── the score ──────────────────────────────────────────────────────────────


def sw(s):
    beat = s // 4; f = s - beat * 4; split = 2 + (2 / 3) * SWING
    return beat * 4 + (f * split / 2 if f < 2 else split + (f - 2) * (4 - split) / 2)


at = lambda bar, s16=0: bar * BAR + sw(s16) * S16
events = []  # (vowel time, name, midi, hold, slide_from, gain, bus, opts)


def ev(t, name, midi, hold, slide=None, gain=1.0, bus="dry", **o):
    events.append((t, name, midi, hold, slide, gain, bus, o))



# the score: shared helpers, then this arrangement's placements
for _f in ("common", ARR):
    exec(open(HERE.parent / "voices" / f"{_f}.py").read(), globals())


def delay(x, d_sec, fb, taps, lp=0.35):
    out = x.copy(); d = int(d_sec * FS); y = x
    for _ in range(taps):
        y = np.concatenate([np.zeros(d), y[:-d]]) * fb
        y = np.convolve(y, np.ones(int(1 / lp * 3)) / int(1 / lp * 3), mode="same")  # darker each repeat
        out += y
    return out


def main():
    n = int(LEN_S * FS) + FS
    bus = {k: np.zeros((n, 2)) for k in ("dry", "toot", "chord", "choir", "lyric")}
    cache = {}
    for t, nm, m, h, sl, g, b, o in events:
        o = dict(o); cents = o.pop("cents", 0); pan = o.pop("pan", 0.0)
        key = (nm, m, round(h, 3), sl, cents, tuple(sorted(o.items())))
        if key not in cache: cache[key] = sing(nm, m + cents / 100, h, sl, **o)
        pick, y = cache[key]
        i = int(round((t - pick) * FS)); y = y[: n - i]
        lr = np.array([np.cos((pan + 1) * np.pi / 4), np.sin((pan + 1) * np.pi / 4)]) * np.sqrt(2)
        bus[b][i: i + len(y)] += 0.4 * g * y[:, None] * lr
    for starts, voice, slug, midis, end_hold, g, b, pan in phrases:
        rel = [t - starts[0] for t in starts]
        key = ("phrase", voice, slug, tuple(np.round(rel, 4)), tuple(midis), round(end_hold, 3))
        if key not in cache: cache[key] = sing_phrase(voice, slug, rel, midis, end_hold)
        pick, y = cache[key]
        i = int(round((starts[0] - pick) * FS)); y = y[: n - i]
        lr = np.array([np.cos((pan + 1) * np.pi / 4), np.sin((pan + 1) * np.pi / 4)]) * np.sqrt(2)
        bus[b][i: i + len(y)] += 0.4 * g * y[:, None] * lr
    missing = set()
    for t, nm, style, g, pan, b in accents:
        y = raw_clip(nm, style)
        if y is None: missing.add(f"{style}/{nm}"); continue
        lr = np.array([np.cos((pan + 1) * np.pi / 4), np.sin((pan + 1) * np.pi / 4)]) * np.sqrt(2)
        i = int(round(t * FS)); y = y[: n - i]
        bus[b][i: i + len(y)] += 0.4 * g * y[:, None] * lr
    if missing: print("accents not rendered yet:", " ".join(sorted(missing)))
    st = lambda f, x: np.stack([f(x[:, 0]), f(x[:, 1])], 1)
    small = META.get("room") == "bedroom"  # a small room: one or two short echoes, not a dub tail
    toot = st(lambda c: delay(c, 0.93 * BEAT, 0.14 if small else 0.28, 1 if small else 3), bus["toot"])
    chord = st(lambda c: delay(c, 0.75 * BEAT, 0.1 if small else 0.2, 1 if small else 2), bus["chord"])
    choir = st(lambda c: delay(c, 1.5 * BEAT, 0.15 if small else 0.3, 2 if small else 3), bus["choir"])
    total = bus["dry"] + toot + chord + 0.55 * choir  # the lyric bus is its own stem (…-lyric.wav), dry
    out = resample_poly(total, 2, 1, axis=0)
    OUT.mkdir(exist_ok=True)
    STEM = OUT / ("vocal-stem.wav" if ARR == "country" else f"vocal-{ARR}.wav")
    sf.write(STEM, out.astype(np.float32), 44100)
    sf.write(STEM.with_name(STEM.stem + "-lyric.wav"), resample_poly(bus["lyric"], 2, 1, axis=0).astype(np.float32), 44100)
    json.dump({"events": len(events), "unique": len(cache)}, open(STEM.with_suffix(".json"), "w"))
    print(STEM, len(events), "events,", len(phrases), "sung phrases,", len(cache), "unique renders")


if __name__ == "__main__":
    sys.exit(main())

