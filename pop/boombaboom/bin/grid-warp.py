#!/usr/bin/env python3
# grid-warp.py — beat-align a free-sung take onto a tempo grid WITHOUT losing
# the performance. Detects syllable onsets, snaps each to the nearest 16th-note
# at the target BPM, and time-warps the audio between onsets with a single
# rubberband --timemap pass (pitch preserved). The result keeps his melody,
# words and phrasing but now LOCKS to the beat.
#
# Usage: grid-warp.py <in.wav> <out.wav> --bpm 140 [--grid 4]   (grid = subdivisions/beat: 4 = 16ths)

import argparse, subprocess, os, tempfile
import numpy as np
import soundfile as sf

ap = argparse.ArgumentParser()
ap.add_argument("inp"); ap.add_argument("out")
ap.add_argument("--bpm", type=float, default=140.0)
ap.add_argument("--grid", type=int, default=2)        # subdivisions per beat (2 = 8th notes)
ap.add_argument("--gate", type=float, default=0.10)   # onset energy gate (frac of peak)
ap.add_argument("--anchor", type=float, default=0.42)    # flux frac → a syllable-attack anchor (high = few, gentle)
ap.add_argument("--boom-flux", type=float, default=0.55) # flux ≥ this = sharp 'b' (beat); below = 'ma' (bar)
ap.add_argument("--max-ratio", type=float, default=1.5)  # clamp local stretch so it never skips
a = ap.parse_args()

x, sr = sf.read(a.inp)
if x.ndim > 1: x = x.mean(axis=1)
x = np.ascontiguousarray(x, dtype=np.float64)
n = len(x)

beat = 60.0 / a.bpm
step = beat / a.grid                                  # grid quantum (s)

# ── onset detection (energy flux + silence→active), 10ms hops ──────────
hop = int(sr * 0.010)
frames = max(1, n // hop)
env = np.array([np.sqrt(np.mean(x[i*hop:(i+1)*hop]**2)) + 1e-9 for i in range(frames)])
env = np.convolve(env, np.ones(3)/3, mode="same")
peak = env.max()
GATE = a.gate * peak
active = env > GATE
flux = np.maximum(0, np.diff(env, prepend=env[0]))
othr = 0.16 * flux.max()

# Collect onsets with their flux strength; the STRONG ones are the "boom"
# b-plosives and "ma" attacks — those are the rhythmic anchors @jeffrey wants
# locked to the beat. Weaker onsets ride inside the stretch between anchors.
onsets, strengths = [], []
last = -999
MIN = 9   # frames (~90ms) between onsets
amax = flux.max()
for i in range(1, frames):
    rising = flux[i] > othr and env[i] > GATE
    s2a = (not active[i-1]) and active[i]
    if (rising or s2a) and (i - last) >= MIN:
        onsets.append(i * hop)            # sample position
        strengths.append(flux[i] / amax)  # 0..1 transient strength
        last = i

# Anchors = onsets strong enough to be a syllable attack. Classify each:
#   • SHARP transient (high flux) = a 'b' plosive → "boom" → snaps to a BEAT
#     (lands on a kick).
#   • SOFTER onset (lower flux, the nasal 'm') = "ma" → snaps to a BAR
#     boundary (a downbeat), which means stretching the boom-ma material out
#     to reach it — a tighter, more deliberate fit.
anchors = [(s, st) for s, st in zip(onsets, strengths) if st >= a.anchor]
if not anchors or anchors[0][0] != onsets[0]:
    anchors = [(onsets[0], strengths[0] if strengths else 1.0)] + anchors
anchors = sorted(set(anchors))

beat = 60.0 / a.bpm
barlen = beat * 4.0

# ── TRIM to the first attack (sample 0), then snap each anchor ─────────
# Output STARTS at the first attack; the C engine drops sample 0 on a bar
# downbeat, so anchor[0] = a kick and every later anchor lands on its grid.
start = anchors[0][0]
in_rel = [s - start for s, _ in anchors] + [n - start]
targets = []                                           # output seconds, from 0
kinds = []
prev_t = -1.0
for s, st in anchors:
    rel = (s - start) / sr
    if st >= a.boom_flux:                              # sharp → boom → beat
        t = round(rel / beat) * beat; kinds.append("boom")
    else:                                              # soft → ma → bar (lengthen)
        t = round(rel / barlen) * barlen; kinds.append("ma")
    if t <= prev_t + 1e-4:                             # strictly increasing
        # bump to the next quantum of this anchor's grid (prefer stretching)
        grid = beat if kinds[-1] == "boom" else barlen
        t = (np.floor(prev_t / grid) + 1) * grid
    prev_t = t
    targets.append(t)

out_kf = [int(round(t * sr)) for t in targets]
out_kf.append(out_kf[-1] + (n - anchors[-1][0]))       # tail keeps its length
for i in range(1, len(out_kf)):
    if out_kf[i] <= out_kf[i-1]:
        out_kf[i] = out_kf[i-1] + 1

# CLAMP local stretch so no single segment warps hard enough to chop. A
# segment ratio outside [1/max, max] gets pulled back to the bound (and the
# shift cascades to later keyframes), trading a little grid-exactness for a
# smooth, glitch-free stretch.
hi, lo = a.max_ratio, 1.0 / a.max_ratio
for i in range(1, len(out_kf)):
    din = in_rel[i] - in_rel[i-1]
    if din <= 0:
        continue
    seg = out_kf[i] - out_kf[i-1]
    r = seg / din
    if r > hi:   out_kf[i] = out_kf[i-1] + int(round(hi * din))
    elif r < lo: out_kf[i] = out_kf[i-1] + int(round(lo * din))

total_in, total_out = in_rel[-1], out_kf[-1]
ratio = total_out / total_in

# trimmed input (from the first attack) → temp wav for rubberband
with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tf:
    tin = tf.name
sf.write(tin, x[start:].astype(np.float32), sr)

with tempfile.NamedTemporaryFile("w", suffix=".txt", delete=False) as tf:
    for ci, co in zip(in_rel, out_kf):
        tf.write(f"{ci} {co}\n")
    tmap = tf.name

# R3 ("finer") engine + smoothing → the smoothest vocal stretch rubberband
# offers (no R2 crisp transient-stutter that made the warp choppy).
subprocess.run(["rubberband", "-3", "--time", f"{ratio:.6f}", "--timemap", tmap,
                "--formant", "--smoothing", tin, a.out], check=True,
               stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
os.unlink(tmap); os.unlink(tin)

nboom = kinds.count("boom"); nma = kinds.count("ma")
print(f"grid-warp: {len(anchors)} anchors → {nboom} booms on beats, {nma} 'ma' on bars; "
      f"trim {start/sr:.2f}s, {total_in/sr:.2f}s → {total_out/sr:.2f}s (ratio {ratio:.3f}) @ {a.bpm} BPM "
      f"({'stretch' if ratio>1 else 'compress'})")
