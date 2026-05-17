#!/usr/bin/env python3
"""
autotune.py — WORLD scale-snap autotune for a free-sung vocal.

Unlike pitchsnap_world.py (which replaces f0 with a fixed target
melody), this keeps her actual sung contour but quantizes every voiced
frame toward the nearest note of a musical scale — classic autotune.
Spectral envelope + aperiodicity are untouched, so her voice identity
and words are preserved; only the pitch is corrected.

Usage:
  autotune.py <in.wav> <out.wav> --key A --scale minorpent \
      [--strength 0.92] [--glide-ms 35]
"""
import argparse
import numpy as np
import soundfile as sf
import pyworld as pw

PC = {"c": 0, "d": 2, "e": 4, "f": 5, "g": 7, "a": 9, "b": 11}
SCALES = {
    "minorpent": [0, 3, 5, 7, 10],
    "majorpent": [0, 2, 4, 7, 9],
    "minor":     [0, 2, 3, 5, 7, 8, 10],
    "major":     [0, 2, 4, 5, 7, 9, 11],
    "chromatic": list(range(12)),
}

def key_to_pc(k: str) -> int:
    k = k.strip().lower()
    pc = PC[k[0]]
    if len(k) > 1 and k[1] == "#": pc += 1
    if len(k) > 1 and k[1] == "b": pc -= 1
    return pc % 12

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inp"); ap.add_argument("out")
    ap.add_argument("--key", default="A")
    ap.add_argument("--scale", default="minorpent")
    ap.add_argument("--strength", type=float, default=0.92)  # 1 = hard
    ap.add_argument("--shift", type=float, default=0.0)       # semitones up
    ap.add_argument("--glide-ms", type=float, default=35.0)
    a = ap.parse_args()

    x, fs = sf.read(a.inp)
    if x.ndim > 1: x = x.mean(axis=1)
    x = np.ascontiguousarray(x, dtype=np.float64)

    f0, t = pw.harvest(x, fs)
    f0 = pw.stonemask(x, f0, t, fs)
    sp = pw.cheaptrick(x, f0, t, fs)
    ap_ = pw.d4c(x, f0, t, fs)

    root = key_to_pc(a.key)
    degrees = SCALES.get(a.scale, SCALES["minorpent"])
    allowed = sorted(((root + d) % 12) for d in degrees)

    voiced = f0 > 0
    midi = np.zeros_like(f0)
    midi[voiced] = 69.0 + 12.0 * np.log2(f0[voiced] / 440.0)
    tgt = midi.copy()
    for i in np.where(voiced)[0]:
        m = midi[i]
        base = np.floor(m)
        # search nearest allowed pitch-class within ±a few semitones
        best, bestd = m, 99.0
        for cand in range(int(base) - 7, int(base) + 8):
            if (cand % 12) in allowed:
                d = abs(cand - m)
                if d < bestd: bestd, best = d, float(cand)
        tgt[i] = m + a.strength * (best - m)
    tgt[voiced] += a.shift                               # octave/semitone lift

    f0c = f0.copy()
    f0c[voiced] = 440.0 * (2.0 ** ((tgt[voiced] - 69.0) / 12.0))

    # short glide so corrections aren't stair-steppy (frame period ~5ms)
    fp = (t[1] - t[0]) * 1000.0 if len(t) > 1 else 5.0
    win = max(1, int(round(a.glide_ms / max(1e-6, fp))))
    if win > 1:
        k = np.ones(win) / win
        sm = np.convolve(f0c, k, mode="same")
        f0c[voiced] = sm[voiced]

    y = pw.synthesize(f0c, sp, ap_, fs)
    peak = np.max(np.abs(y)) or 1.0
    y = (y / peak) * 0.97
    sf.write(a.out, y.astype(np.float32), fs)
    print(f"autotune: {a.key} {a.scale} str={a.strength} "
          f"voiced={int(voiced.sum())}/{len(f0)} → {a.out}")

if __name__ == "__main__":
    main()
