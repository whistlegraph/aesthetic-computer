#!/usr/bin/env python3
"""wordgate.py — pitch measurement for the hitbaker word score.

  wordgate.py src <stem.wav> <fromMs> <toMs>   median sung midi in the
                                               window (cached .f0.npz
                                               beside the stem when
                                               present, else pyin)
  wordgate.py wav <slice.wav>                  pyin median midi of a
                                               rendered slice

Prints one float (fractional midi) or "nan". The hitbaker uses this to
hit chord-tone targets exactly and to verify every rendered layer —
measured, never guessed.
"""
import sys
from pathlib import Path

import numpy as np


def pyin_midi(path, offset=0.0, duration=None):
    import librosa
    y, sr = librosa.load(path, sr=22050, mono=True,
                         offset=offset, duration=duration)
    if len(y) < 512:
        return float("nan")
    f0, voiced, _ = librosa.pyin(
        y, fmin=float(librosa.note_to_hz("C2")),
        fmax=float(librosa.note_to_hz("C6")), sr=sr)
    v = f0[voiced.astype(bool)] if voiced is not None else np.array([])
    if v.size < 5:
        return float("nan")
    return float(librosa.hz_to_midi(np.median(v)))


def main():
    mode = sys.argv[1]
    if mode == "src":
        stem, from_ms, to_ms = sys.argv[2], float(sys.argv[3]), float(sys.argv[4])
        npz = Path(stem + ".f0.npz")
        if npz.exists():
            d = np.load(npz)
            t, m = d["t"], d["m"]
            win = m[(t >= from_ms / 1000) & (t < to_ms / 1000)]
            win = win[~np.isnan(win)]
            if win.size >= 3:
                print(round(float(np.median(win)), 2))
                return
        print(round(pyin_midi(stem, from_ms / 1000, (to_ms - from_ms) / 1000), 2))
    elif mode == "wav":
        print(round(pyin_midi(sys.argv[2]), 2))


if __name__ == "__main__":
    main()
