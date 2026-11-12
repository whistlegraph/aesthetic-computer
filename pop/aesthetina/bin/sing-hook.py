#!/usr/bin/env python3
"""aesthetina — jeffrey sings the hook from the phoneme library, aesthetivox style.

seh-teh-tee-nah on the typed hook's own notes (one octave down). Per syllable:
the consonant keeps its spoken length as a pickup before the beat, the vowel
nucleus is stretched (frame interpolation, never repeated frames) to hold its
notes, and WORLD replaces f0 with the same slidey glide the ocarina uses. Each
hook is one continuous WORLD synthesis; a quiet octave-up halo rides the
vowels only. Writes out/vocal-stem.wav, aligned to render.mjs's timeline.

usage: pop/.venv/bin/python pop/aesthetina/bin/sing-hook.py
"""
import json, subprocess, sys
from pathlib import Path
import numpy as np, soundfile as sf, pyworld as pw
from scipy.signal import resample_poly

HERE = Path(__file__).resolve().parent
OUT = HERE.parent / "out"
LIB = Path("/Users/jas/aesthetic-computer/aesthetic-computer-vault/voice/jeffrey-phonemes/syllables")
FS, FP = 22050, 5.0  # library rate, WORLD frame period (ms)
BPM, BARS = 128, 32
BEAT = 60 / BPM; BAR = BEAT * 4; S16 = BEAT / 4
LEN_S = BARS * BAR + 3

# hook: steps (16ths) and typed MIDI from notepat's map, then sung -12
HOOK_STEPS = [0, 3, 6, 8, 10, 12, 14, 16, 19, 22]
HOOK_MIDI = [69, 64, 63, 73, 72, 64, 73, 74, 83, 69]  # a e s t h e t i n a
END_STEP = 30
SYLS = [("seh", 0, 3), ("teh", 3, 6), ("tee", 6, 8), ("nah", 8, 10)]  # (clip, first note idx, end idx)
SING_BARS = [12, 14, 20, 22, 24, 26, 28, 30]
TRANSPOSE = -12


def load(name):
    p = LIB / f"{name}.mp3"
    raw = subprocess.run(["ffmpeg", "-loglevel", "error", "-i", str(p), "-f", "f32le", "-ac", "1", "-ar", str(FS), "-"],
                         capture_output=True, check=True).stdout
    x = np.frombuffer(raw, dtype=np.float32).astype(np.float64)
    f0, t = pw.harvest(x, FS, f0_floor=60, f0_ceil=300, frame_period=FP)
    f0 = pw.stonemask(x, f0, t, FS)
    sp = pw.cheaptrick(x, f0, t, FS)
    ap = pw.d4c(x, f0, t, FS)
    v = np.where(f0 > 0)[0]
    first, last = (v[0], v[-1]) if len(v) else (0, len(f0) - 1)
    # energy-trim the fry tail: last voiced frame with real energy
    e = np.log(sp.sum(1) + 1e-12)
    while last > first + 10 and e[last] < e[first:last].max() - 5: last -= 1
    onset_end = min(last - 4, first + 10)  # keep 50 ms of CV transition unstretched
    return dict(sp=sp, ap=ap, voiced=f0 > 0, first=first, onset_end=onset_end, last=last)


CLIPS = {n: load(n) for n, *_ in SYLS}
FR = lambda sec: int(round(sec * 1000 / FP))


def interp_frames(arr, a, b, n):
    """n frames resampled across arr[a:b] (log domain for sp)."""
    src = np.linspace(a, b - 1, n)
    lo = np.floor(src).astype(int); hi = np.minimum(lo + 1, b - 1); w = (src - lo)[:, None]
    return arr[lo] * (1 - w) + arr[hi] * w


def note_curve(times, notes, n_frames, t0):
    """slidey f0 (midi) per frame: cubic ease into each note, small overshoot, delayed vibrato."""
    out = np.zeros(n_frames)
    for k in range(n_frames):
        T = t0 + k * FP / 1000
        i = max(0, np.searchsorted(times, T, side="right") - 1)
        since = T - times[i]
        m = notes[i]
        if i > 0:
            j = notes[i] - notes[i - 1]; g = 0.11 * (1 + abs(j) / 10)
            x = min(1, since / g); ease = 1 - (1 - x) ** 3
            over = 0.15 * np.sign(j) * np.sin(np.pi * min(1, since / (g * 1.8))) * x
            m = notes[i - 1] + j * ease + over
        else:
            m -= 0.4 * np.exp(-max(0, since) / 0.06)
        vib = 0.15 * min(1, max(0, (since - 0.16) / 0.3)) * np.sin(2 * np.pi * 5.2 * T)
        out[k] = m + vib
    return out


def sing_hook(t_bar):
    times = np.array([t_bar + s * S16 for s in HOOK_STEPS])
    notes = np.array(HOOK_MIDI) + TRANSPOSE
    end_t = t_bar + END_STEP * S16
    # vowel-onset times per syllable; consonant pickup precedes it
    vt = [times[a] for _, a, _ in SYLS] + [end_t]
    cons = [CLIPS[n]["onset_end"] for n, *_ in SYLS]
    start_t = vt[0] - cons[0] * FP / 1000
    sp, ap, voiced, vowel = [], [], [], []
    for i, (name, a, b) in enumerate(SYLS):
        c = CLIPS[name]
        # onset (consonant + CV transition) raw
        sp.append(c["sp"][: c["onset_end"]]); ap.append(c["ap"][: c["onset_end"]])
        voiced.append(c["voiced"][: c["onset_end"]]); vowel.append(np.zeros(c["onset_end"], bool))
        # nucleus fills until the next syllable's consonant pickup (or the end + tail)
        next_cons = cons[i + 1] if i + 1 < len(SYLS) else 0
        n_nuc = max(4, FR(vt[i + 1] - vt[i]) - next_cons)
        nuc_sp = np.exp(interp_frames(np.log(c["sp"] + 1e-16), c["onset_end"], c["last"] + 1, n_nuc))
        sp.append(nuc_sp); ap.append(interp_frames(c["ap"], c["onset_end"], c["last"] + 1, n_nuc))
        voiced.append(np.ones(n_nuc, bool)); vowel.append(np.ones(n_nuc, bool))
    sp = np.maximum(np.vstack(sp), 1e-12); ap = np.vstack(ap); voiced = np.concatenate(voiced); vowel = np.concatenate(vowel)
    # release: 120 ms fade on the last vowel
    n = len(sp)
    midi = note_curve(times, notes, n, start_t)
    f0 = np.where(voiced, 440 * 2 ** ((midi - 69) / 12), 0.0)
    fade = np.ones(n); r = FR(0.12); fade[-r:] = np.linspace(1, 0, r) ** 2
    # airy holds: lift aperiodicity a touch on long vowels
    ap = np.where(vowel[:, None], np.clip(ap + 0.08, 0, 1), ap)
    y = pw.synthesize(f0, np.ascontiguousarray(sp), np.ascontiguousarray(ap), FS, FP)
    # halo: octave up, vowels only
    sp_h = np.where(vowel[:, None], sp, sp * 1e-4)
    f0_h = np.where(vowel, f0 * 2, 0.0)
    h = pw.synthesize(f0_h, np.ascontiguousarray(sp_h), np.ascontiguousarray(np.clip(ap + 0.15, 0, 1)), FS, FP)
    m = min(len(y), len(h))
    env = np.interp(np.arange(m) / FS, np.arange(n) * FP / 1000, fade)  # release on the output, never on sp (log 0 → NaN)
    out = (y[:m] + 0.2 * h[:m]) * env
    assert np.isfinite(out).all(), "WORLD produced non-finite samples"
    return start_t, out


# ── toot mode: vowels only, one toot per keystroke ─────────────────────────
# Each typed note gets a bare vowel (no consonant), a soft breathy onset and a
# short hold, sliding up from the previous note's pitch. The vowel follows the
# sound of the word: aes-the-ti-na → ah eh eh eh eh eh ee ee ah ah.
TOOT_VOWELS = ["ah", "eh", "eh", "eh", "eh", "eh", "ee", "ee", "ah", "ah"]
VOWEL_CLIP = {"ah": "mah", "eh": "meh", "ee": "mee", "oh": "moh", "oo": "moo"}
VCLIPS = {v: load(c) for v, c in VOWEL_CLIP.items()}


def toot_hook(t_bar):
    times = np.array([t_bar + s * S16 for s in HOOK_STEPS])
    notes = np.array(HOOK_MIDI, float) + TRANSPOSE
    end_t = t_bar + END_STEP * S16
    out_len = int((end_t - t_bar + 0.6) * FS)
    y_all = np.zeros(out_len)
    for i, (t, m) in enumerate(zip(times, notes)):
        gap = (times[i + 1] if i + 1 < len(times) else end_t) - t
        dur = gap * 0.9 if i == len(times) - 1 else min(gap * 0.72, 0.3)
        c = VCLIPS[TOOT_VOWELS[i]]
        n = max(8, FR(dur + 0.06))
        # steady part of the vowel only: skip the m and its transition
        a = min(c["onset_end"] + 4, c["last"] - 8)
        sp = np.maximum(np.exp(interp_frames(np.log(c["sp"] + 1e-16), a, c["last"] + 1, n)), 1e-12)
        ap = np.clip(interp_frames(c["ap"], a, c["last"] + 1, n) + 0.1, 0, 1)
        # slide up from the previous toot's pitch (or a small scoop on the first)
        prev = notes[i - 1] if i else m - 0.6
        k = np.arange(n) * FP / 1000
        g = 0.07 * (1 + abs(m - prev) / 12)
        x = np.minimum(1, k / g); midi = prev + (m - prev) * (1 - (1 - x) ** 3)
        midi += 0.12 * np.minimum(1, np.maximum(0, (k - 0.15) / 0.2)) * np.sin(2 * np.pi * 5.2 * k)
        f0 = 440 * 2 ** ((midi - 69) / 12)
        y = pw.synthesize(f0, np.ascontiguousarray(sp), np.ascontiguousarray(ap), FS, FP)
        h = pw.synthesize(f0 * 2, np.ascontiguousarray(sp), np.ascontiguousarray(np.clip(ap + 0.15, 0, 1)), FS, FP)
        L = min(len(y), len(h), int((dur + 0.06) * FS)); s = np.arange(L) / FS
        env = np.minimum(1, s / 0.018) ** 1.5 * np.where(s < dur, 1, np.exp(-(s - dur) / 0.03))
        env *= 1 + 0.25 * np.exp(-s / 0.05)  # the toot: a little push of breath at the front
        o = int(round((t - t_bar) * FS))
        seg = ((y[:L] + 0.2 * h[:L]) * env)[: out_len - o]
        y_all[o: o + len(seg)] += seg
    assert np.isfinite(y_all).all()
    return t_bar, y_all


# ── scat: ba / ka / tra / a as vocal percussion ───────────────────────────
# Consonant onsets stay raw and unpitched; only the voiced part takes the note.
# The consonant is a pickup, so the vowel lands on the grid.
SCAT_CLIPS = {"ba": load("bah"), "ka": load("kah"), "ra": load("rah"), "ta": load("tah"), "a": VCLIPS["ah"]}


def scat_frames(syl):
    if syl == "tra":  # t burst from "tah" spliced onto "rah"
        t, r = SCAT_CLIPS["ta"], SCAT_CLIPS["ra"]
        cut = t["first"]
        return (np.vstack([t["sp"][:cut], r["sp"][: r["last"] + 1]]), np.vstack([t["ap"][:cut], r["ap"][: r["last"] + 1]]),
                np.concatenate([np.zeros(cut, bool), r["voiced"][: r["last"] + 1] | (np.arange(r["last"] + 1) >= r["first"])]),
                cut + r["first"])
    c = SCAT_CLIPS[syl]
    if syl == "a":
        a = min(c["onset_end"] + 4, c["last"] - 8)
        sl = slice(a, c["last"] + 1)
        return c["sp"][sl], c["ap"][sl], np.ones(c["last"] + 1 - a, bool), 0
    sl = slice(0, c["last"] + 1)
    return c["sp"][sl], c["ap"][sl], c["voiced"][sl] | (np.arange(c["last"] + 1) >= c["first"]), c["first"]


def scat(syl, midi, hold, slide_from=None):
    """one scat hit: returns (pickup seconds, samples)."""
    sp, ap, voiced, v0 = scat_frames(syl)
    # vowel: keep 40 ms of transition, then stretch/trim the nucleus to `hold`
    keep = min(len(sp), v0 + 8)
    n_nuc = max(4, FR(hold))
    nuc_sp = np.exp(interp_frames(np.log(sp + 1e-16), keep, len(sp), n_nuc)) if len(sp) > keep + 1 else sp[-1:].repeat(n_nuc, 0)
    nuc_ap = interp_frames(ap, keep, len(ap), n_nuc) if len(ap) > keep + 1 else ap[-1:].repeat(n_nuc, 0)
    sp = np.maximum(np.vstack([sp[:keep], nuc_sp]), 1e-12)
    ap = np.vstack([ap[:keep], nuc_ap])
    voiced = np.concatenate([voiced[:keep], np.ones(n_nuc, bool)])
    n = len(sp); k = (np.arange(n) - v0) * FP / 1000
    start = slide_from if slide_from is not None else midi - 0.5
    x = np.clip(k / 0.06, 0, 1); m = start + (midi - start) * (1 - (1 - x) ** 3)
    f0 = np.where(voiced, 440 * 2 ** ((m - 69) / 12), 0.0)
    y = pw.synthesize(f0, np.ascontiguousarray(sp), np.ascontiguousarray(ap), FS, FP)
    s = np.arange(len(y)) / FS; tail = (n - 6) * FP / 1000
    y *= np.where(s < tail, 1, np.exp(-(s - tail) / 0.025))
    return v0 * FP / 1000, y


# two-bar scat cycle: (16th step, syllable, midi, hold seconds, slide-from)
SCAT_PATTERN = [
    (0, "ba", 57, 0.10, None), (3, "ka", 57, 0.06, None), (6, "tra", 57, 0.10, 52), (8, "a", 60, 0.18, None),
    (11, "ka", 57, 0.06, None), (14, "tra", 64, 0.08, 57),
    (16, "ba", 57, 0.10, None), (18, "a", 64, 0.10, None), (19, "ka", 57, 0.06, None), (22, "tra", 57, 0.10, 52),
    (24, "ba", 60, 0.08, None), (26, "ka", 57, 0.06, None), (27, "a", 64, 0.12, None), (30, "tra", 69, 0.08, 57),
]
SCAT_BARS = [4, 6, 8, 10, 16, 18]


def main():
    toot = "--words" not in sys.argv
    total = np.zeros(int(LEN_S * FS) + FS)
    for bar in SING_BARS:
        t0, y = (toot_hook if toot else sing_hook)(bar * BAR)
        y = y / (np.sqrt(np.mean(y ** 2)) + 1e-9) * 0.08
        i = int(round(t0 * FS)); total[i: i + len(y)] += y[: len(total) - i]
    cache = {}
    for bar in SCAT_BARS:
        for step, syl, midi, hold, sf_ in SCAT_PATTERN:
            key = (syl, midi, hold, sf_)
            if key not in cache:
                pick, y = scat(syl, midi, hold, sf_)
                cache[key] = (pick, y / (np.abs(y).max() + 1e-9) * 0.16)
            pick, y = cache[key]
            vel = 1.0 if step % 8 == 0 else 0.8
            i = int(round((bar * BAR + step * S16 - pick) * FS)); total[i: i + len(y)] += vel * y[: len(total) - i]
    out = resample_poly(total, 2, 1)  # 22050 → 44100
    OUT.mkdir(exist_ok=True)
    sf.write(OUT / "vocal-stem.wav", out.astype(np.float32), 44100)
    json.dump({"bars": SING_BARS, "mode": "toot" if toot else "words", "vowels": TOOT_VOWELS, "transpose": TRANSPOSE},
              open(OUT / "vocal-stem.json", "w"))
    print(OUT / "vocal-stem.wav")


if __name__ == "__main__":
    sys.exit(main())
