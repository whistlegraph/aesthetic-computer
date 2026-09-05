#!/usr/bin/env python3
"""
autotune.py — WORLD scale-snap autotune for a free-sung vocal.
Shared across the pop lanes (jungle/dance/big-pictures/…).

Two modes:

  --mode note  (default, "Melodyne-style"): segment the take into NOTES
      (pitch-stable voiced runs), correct each note's CENTRE to the
      nearest scale degree, and let the singer's intra-note movement
      (vibrato / scoop / drift) ride on top, scaled by --preserve. Sounds
      musically tuned, not robot-tuned.

  --mode frame ("hard"): snap every voiced frame to the scale. The old
      aggressive autotune.

Spectral envelope (cheaptrick) + aperiodicity (d4c) are untouched, so
voice identity, words and breath are preserved — only f0 changes.

A third mode arrives with --targets (imab, 2026-09-03): a per-syllable
NOTE-LOCK — the melody is written, each window [{label,t,dur,note}] is
corrected to ITS note (not just the nearest scale degree). Since
2026-09-05 each window measures the base freq of its own incoming
sample and pitch-matches to the written note at the nearest octave,
and windows own every voiced frame through to the next onset — the
original take's pitch never survives between or after words.
(--register-fit is accepted for compatibility; per-window base-freq
fitting is now always on in targets mode.) The fitted map is written
to <targets>.fitted.json so notecheck and the lyric videos share the
same register.

Usage:
  autotune.py <in.wav> <out.wav> [--key A] [--scale minorpent]
      [--mode note|frame] [--strength 0.9] [--preserve 0.6]
      [--shift 0] [--glide-ms 35]
      [--targets targets.json] [--register-fit]
"""
import argparse
import numpy as np
import soundfile as sf
import pyworld as pw

PC = {"c": 0, "d": 2, "e": 4, "f": 5, "g": 7, "a": 9, "b": 11}
SCALES = {
    "minorpent": [0, 3, 5, 7, 10], "majorpent": [0, 2, 4, 7, 9],
    "minor": [0, 2, 3, 5, 7, 8, 10], "major": [0, 2, 4, 5, 7, 9, 11],
    "chromatic": list(range(12)),
}


def key_to_pc(k):
    k = k.strip().lower(); pc = PC[k[0]]
    if len(k) > 1 and k[1] == "#": pc += 1
    if len(k) > 1 and k[1] == "b": pc -= 1
    return pc % 12


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inp"); ap.add_argument("out")
    ap.add_argument("--key", default="A")
    ap.add_argument("--scale", default="minorpent")
    ap.add_argument("--mode", default="note", choices=["note", "frame"])
    ap.add_argument("--strength", type=float, default=0.9)   # 1 = dead-on
    ap.add_argument("--preserve", type=float, default=0.6)    # note mode: keep vibrato/scoop
    ap.add_argument("--shift", type=float, default=0.0)       # semitones up
    ap.add_argument("--glide-ms", type=float, default=35.0)
    ap.add_argument("--targets", default=None)                # per-syllable note-lock map
    ap.add_argument("--register-fit", action="store_true")    # global octave to the singer
    a = ap.parse_args()

    x, fs = sf.read(a.inp)
    if x.ndim > 1: x = x.mean(axis=1)
    x = np.ascontiguousarray(x, dtype=np.float64)

    f0, t = pw.harvest(x, fs)
    f0 = pw.stonemask(x, f0, t, fs)
    sp = pw.cheaptrick(x, f0, t, fs)
    ap_ = pw.d4c(x, f0, t, fs)

    root = key_to_pc(a.key)
    allowed = sorted({(root + d) % 12 for d in SCALES.get(a.scale, SCALES["minorpent"])})

    def snap(m):
        b = int(np.floor(m)); best, bd = m, 99.0
        for c in range(b - 7, b + 8):
            if c % 12 in allowed:
                d = abs(c - m)
                if d < bd: bd, best = d, float(c)
        return best

    voiced = f0 > 0
    midi = np.zeros_like(f0)
    midi[voiced] = 69.0 + 12.0 * np.log2(np.maximum(f0[voiced], 1e-6) / 440.0)
    tgt = midi.copy()
    n = len(f0)
    fp = (t[1] - t[0]) if len(t) > 1 else 0.005

    if a.targets:
        import json, re
        NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
        def name_to_midi(nm):
            m = re.match(r"^([A-G]#?)(-?\d)$", nm)
            return (int(m.group(2)) + 1) * 12 + NAMES.index(m.group(1))
        def midi_to_name(m):
            return NAMES[int(round(m)) % 12] + str(int(round(m)) // 12 - 1)
        tmap = sorted(json.load(open(a.targets)), key=lambda w_: w_["t"])
        # Each window measures the BASE FREQ of its own incoming sample
        # and pitch-matches to the written note at the nearest octave.
        # Windows own every voiced frame up to the next window's onset
        # (the last owns to EOF), so no melisma tail or gap can inherit
        # the original take's pitch.
        fitted = []
        for i_, w_ in enumerate(tmap):
            t0_ = w_["t"]
            t1_ = t0_ + max(w_["dur"], 0.08)
            nxt = tmap[i_ + 1]["t"] if i_ + 1 < len(tmap) else t[-1] + fp
            own = voiced & (t >= t0_) & (t < max(nxt, t1_))
            core = own & (t < t1_)
            sel = core if core.sum() >= 3 else own
            if not sel.sum():
                fitted.append(dict(w_))
                continue
            base = float(np.median(midi[sel]))
            note_ = name_to_midi(w_["note"])
            eff = note_ + int(round((base - note_) / 12.0)) * 12
            tgt[own] = (midi[own] - base) * a.preserve + base + a.strength * (eff - base)
            fitted.append({**w_, "note": midi_to_name(eff)})
            print(f"  {w_.get('label', w_['note']):>10}: base {midi_to_name(base)} "
                  f"→ lock {midi_to_name(eff)}")
        json.dump(fitted, open(a.targets.replace(".json", "") + ".fitted.json", "w"), indent=1)
    elif a.mode == "frame":
        for i in np.where(voiced)[0]:
            tgt[i] = midi[i] + a.strength * (snap(midi[i]) - midi[i])
    else:
        # group voiced frames into notes via a smoothed "intended pitch"
        # trajectory (≈125 ms window) snapped to scale — same snapped
        # value over consecutive voiced frames = one note.
        w = max(1, int(round(0.125 / fp)))
        sm = midi.copy()
        if w > 1:
            ker = np.ones(w) / w
            sm = np.convolve(midi, ker, mode="same")
        i = 0
        while i < n:
            if not voiced[i]:
                i += 1
                continue
            note_t = round(snap(sm[i]))
            j = i
            while j < n and voiced[j] and round(snap(sm[j])) == note_t:
                j += 1
            seg = slice(i, j)
            centre = float(np.median(midi[seg]))          # her sung centre
            target = snap(centre)                          # the correct pitch
            corr = a.strength * (target - centre)
            # keep the expressive deviation around the centre, ×preserve
            tgt[seg] = (midi[seg] - centre) * a.preserve + centre + corr
            i = j

    tgt[voiced] += a.shift
    f0c = f0.copy()
    f0c[voiced] = 440.0 * (2.0 ** ((tgt[voiced] - 69.0) / 12.0))

    # short glide so note-to-note jumps aren't stair-steppy
    win = max(1, int(round(a.glide_ms / max(1e-6, fp * 1000.0))))
    if win > 1:
        sm2 = np.convolve(f0c, np.ones(win) / win, mode="same")
        f0c[voiced] = sm2[voiced]

    y = pw.synthesize(f0c, sp, ap_, fs)
    peak = np.max(np.abs(y)) or 1.0
    y = (y / peak) * 0.97
    sf.write(a.out, y.astype(np.float32), fs)
    print(f"autotune[{a.mode}]: {a.key} {a.scale} str={a.strength} "
          f"prsv={a.preserve} shift={a.shift} "
          f"voiced={int(voiced.sum())}/{n} → {a.out}")


if __name__ == "__main__":
    main()
