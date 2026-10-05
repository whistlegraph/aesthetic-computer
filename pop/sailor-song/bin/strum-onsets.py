#!/usr/bin/env python
# strum-onsets.py — her strums, one by one, from the guitar stem (v120, jeffrey: "the twang effects … need to be attached
# to every twang"). The receipt's gtr events are one per bar; this finds every strum onset in src/vox/cut/guitar-48k.wav
# (take clock): spectral flux in the 1–6 kHz band (the pick's attack lives there, not the body's boom), half-wave
# rectified, against a local-median threshold, peaks at least 60 ms apart; each strum's strength normalised 0–1 by the
# 95th percentile of the flux peaks, so one loud strum does not flatten the rest.
#
#   pop/.venv/bin/python pop/sailor-song/bin/strum-onsets.py [--in src/vox/cut/guitar-48k.wav] [--out src/strums.json] [--check 22.2 26.5]
#     → src/strums.json  [{"t": <take s>, "gain": <0–1>}, …]
import argparse, json, os, sys
import numpy as np, soundfile as sf
from scipy.signal import stft

HERE = os.path.dirname(os.path.abspath(__file__)); LANE = os.path.dirname(HERE)
ap = argparse.ArgumentParser()
ap.add_argument("--in", dest="inp", default=os.path.join(LANE, "src/vox/cut/guitar-48k.wav"))
ap.add_argument("--out", default=os.path.join(LANE, "src/strums.json"))
ap.add_argument("--check", nargs=2, type=float, default=[22.2, 26.5], help="print the strums in this take window")
ap.add_argument("--gap", type=float, default=0.06, help="minimum s between strums")
ap.add_argument("--delta", type=float, default=0.12, help="how far above the local median a peak must rise (of the 95th pct)")
A = ap.parse_args()

y, sr = sf.read(A.inp, dtype="float32", always_2d=True); y = y.mean(axis=1)
NFFT, HOP = 2048, 240                                                     # 5 ms hops at 48 kHz
f, t, Z = stft(y, fs=sr, nperseg=NFFT, noverlap=NFFT - HOP, padded=False, boundary=None)
band = (f >= 1000) & (f <= 6000)
S = np.log1p(50 * np.abs(Z[band]))                                        # log magnitude: the quiet strums count too
flux = np.maximum(S[:, 1:] - S[:, :-1], 0).sum(axis=0); flux = np.concatenate([[0], flux])
t = t[: len(flux)]
# the local median over ±150 ms, and the threshold a share of the 95th percentile above it
from scipy.ndimage import median_filter, maximum_filter1d
w = int(round(0.3 / (HOP / sr))) | 1
med = median_filter(flux, size=w, mode="nearest")
ref = np.percentile(flux, 95); thr = med + A.delta * ref
gapn = max(1, int(round(A.gap / (HOP / sr))))
loc = maximum_filter1d(flux, size=2 * gapn + 1, mode="nearest")
peaks = np.where((flux >= loc) & (flux > thr) & (flux > 0))[0]
# keep the gap: a later peak inside `gap` of a kept one is dropped (the kept one is the earlier — the attack)
kept = []
for i in peaks:
    if kept and t[i] - t[kept[-1]] < A.gap:
        if flux[i] > flux[kept[-1]] * 1.5: kept[-1] = i                    # unless it is plainly the real attack
        continue
    kept.append(i)
kept = np.array(kept, dtype=int)
strength = flux[kept] - med[kept]
norm = np.percentile(strength, 95) if len(strength) else 1
# the onset lands a hop or two before the flux peak (the attack rises through the frame): back it up by half a frame's rise
out = [{"t": round(float(t[i]) - 0.004, 4), "gain": round(float(min(1, max(0.05, s / norm))), 3)} for i, s in zip(kept, strength)]
with open(A.out, "w") as fh: json.dump(out, fh)
print(f"✓ {A.out}: {len(out)} strums over {t[-1]:.1f} s (median gap {np.median(np.diff([o['t'] for o in out])) if len(out) > 1 else 0:.3f} s)")
a, b = A.check; win = [o for o in out if a <= o["t"] <= b]
print(f"  {a}–{b} s: {len(win)} strums")
prev = None
for o in win:
    print(f"    {o['t']:8.3f}  gain {o['gain']:.2f}" + (f"  (+{o['t'] - prev:.3f})" if prev is not None else "")); prev = o["t"]
