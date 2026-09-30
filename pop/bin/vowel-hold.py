#!/usr/bin/env python3
"""vowel-hold.py — continue a sung vowel with WORLD (the aesthetivox way), not grains.

Analyzes the voice just before a cut (harvest → cheaptrick → d4c), takes the last
`--steady` seconds of voiced frames, and synthesizes `--len` seconds that continue
them: the spectral envelope and aperiodicity ping-pong through those frames (no
loop seam), f0 continues at the vowel's own median with a slow, growing vibrato
and a breath of drift, so it stays her voice holding the note. Level matches the
steady part; a 30 ms equal-power entry and a shaped release (`--curve`) are baked in.

  pop/.venv/bin/python pop/bin/vowel-hold.py <lead.wav> --at 59.08 --len 1.9 --out hold.wav
      --steady 0.2   seconds of vowel before --at to continue (voiced frames only)
      --curve 0.8    release shape: out = (1 - t/len)^curve
      --vibrato 0.2  depth in semitones (5.2 Hz, fades in over the first 0.4 s)

The output starts AT the cut (mix it there); it is mono if the input is mono.
"""
import argparse
import numpy as np, soundfile as sf, pyworld as pw

ap = argparse.ArgumentParser()
ap.add_argument("wav"); ap.add_argument("--at", type=float, required=True); ap.add_argument("--len", type=float, required=True)
ap.add_argument("--out", required=True); ap.add_argument("--steady", type=float, default=0.2)
ap.add_argument("--curve", type=float, default=0.8); ap.add_argument("--vibrato", type=float, default=0.15)
ap.add_argument("--fade-in", type=float, default=0.03)
a = ap.parse_args()
FRAME_MS = 5.0
x, fs = sf.read(a.wav, always_2d=True)
mono = x.mean(1).astype(np.float64)
w0, w1 = int(max(0, a.at - 1.0) * fs), int(a.at * fs)
seg = mono[w0:w1]
f0, t = pw.harvest(seg, fs, f0_floor=150.0, f0_ceil=900.0, frame_period=FRAME_MS)
f0 = pw.stonemask(seg, f0, t, fs)
sp = pw.cheaptrick(seg, f0, t, fs); apr = pw.d4c(seg, f0, t, fs)
n_steady = int(a.steady * 1000 / FRAME_MS)
voiced = np.where(f0 > 0)[0]
last = voiced[voiced >= len(f0) - n_steady - 2]
if len(last) < 4: raise SystemExit(f"no voiced frames in the last {a.steady}s before {a.at}s")
idx = last[-n_steady:] if len(last) >= n_steady else last
# ONE envelope for the whole hold: the log-mean spectrum of the steady frames (frame-to-frame
# jumps and WORLD's per-frame buzz are what screeched), the median aperiodicity with a floor of
# 0.35 above 4 kHz (a too-periodic top band whistles), and a gentle tilt above 5 kHz
N = int(a.len * 1000 / FRAME_MS)
sp_mean = np.exp(np.mean(np.log(sp[idx] + 1e-12), axis=0))
ap_med = np.median(apr[idx], axis=0)
freqs = np.linspace(0, fs / 2, sp_mean.shape[0])
ap_med = np.where(freqs > 4000, np.maximum(ap_med, 0.35), ap_med)
tilt = np.where(freqs > 5000, 10 ** (-(freqs - 5000) / 4000 * 6 / 20), 1.0)     # −6 dB per 4 kHz above 5 kHz
sp_hold = np.tile(sp_mean * tilt ** 2, (N, 1)); ap_hold = np.tile(ap_med, (N, 1))
f0_med = float(np.median(f0[idx]))
tt = np.arange(N) * FRAME_MS / 1000
vib_env = np.clip(tt / 0.4, 0, 1)
rng = np.random.default_rng(3)
drift = np.cumsum(rng.normal(0, 0.004, N)); drift -= np.linspace(0, drift[-1], N)   # a breath of wander, zero net
cents = a.vibrato * vib_env * np.sin(2 * np.pi * 5.2 * tt) + drift
f0_hold = f0_med * 2 ** (cents / 12)
y = pw.synthesize(np.ascontiguousarray(f0_hold), np.ascontiguousarray(sp_hold), np.ascontiguousarray(ap_hold), fs, frame_period=FRAME_MS)
y = y[: int(a.len * fs)]
# level: the steady vowel's RMS
ref = seg[int(t[idx[0]] * fs): int(t[idx[-1]] * fs) + 1]
g = np.sqrt(np.mean(ref ** 2)) / (np.sqrt(np.mean(y[: len(ref)] ** 2)) + 1e-9)
y *= g
n = len(y); env = (1 - np.arange(n) / n) ** a.curve
fi = int(a.fade_in * fs); env[:fi] *= np.sin(0.5 * np.pi * np.arange(fi) / fi)
y *= env
out = np.repeat(y[:, None], x.shape[1], axis=1) if x.shape[1] > 1 else y
sf.write(a.out, out.astype(np.float32), fs, subtype="FLOAT")
print(f"hold {a.len:.2f}s from {len(idx)} frames ({a.steady:.2f}s) before {a.at:.3f}s · f0 {f0_med:.1f} Hz · gain {g:.2f} → {a.out}")
