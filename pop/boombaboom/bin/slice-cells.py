#!/usr/bin/env python3
# slice-cells.py — segment the boombaboom source vocal into syllable "cells".
#
# The source (boombaboom-raw.wav) is a ~32.5s melodic chant sitting around
# C#4 in a D-minor space, with a recurring "boom-ba-boom" hook motif
# (E-E-E-F-E-F). We don't need a word transcript — we slice on amplitude
# onsets (syllable attacks) and tag each cell with its WORLD-detected median
# pitch + nearest note. The bake script then keeps the hook cells as-sung
# and re-maps the verse cells onto a designed D-minor techno line.
#
# Output: boombaboom/cells.json — [{i, from_ms, to_ms, dur_ms, hz, note,
#          midi, rms}] and clean per-cell WAV slices in boombaboom/snapped/.
#
# Usage: pop/.venv/bin/python boombaboom/bin/slice-cells.py

import json, os, wave, subprocess, sys
import numpy as np
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)                       # pop/boombaboom
SRC  = os.path.join(ROOT, "sources", "boombaboom-raw.wav")
SNAP = os.path.join(ROOT, "snapped")
os.makedirs(SNAP, exist_ok=True)

NAMES = ['C','C#','D','D#','E','F','F#','G','G#','A','A#','B']
def note_name(m): return NAMES[m % 12] + str(m // 12 - 1)
def hz_to_midi(f): return int(round(69 + 12 * np.log2(f / 440.0))) if f > 0 else 0

# ── load ──────────────────────────────────────────────────────────────
w = wave.open(SRC); sr = w.getframerate(); n = w.getnframes()
a = np.frombuffer(w.readframes(n), dtype=np.int16).astype(np.float64) / 32768.0
w.close()
dur = len(a) / sr

# ── energy envelope (10ms hop) + onset detection ───────────────────────
hop = int(sr * 0.010)
frames = max(1, len(a) // hop)
env = np.array([np.sqrt(np.mean(a[i*hop:(i+1)*hop]**2)) + 1e-9 for i in range(frames)])
# smooth
k = 3
env_s = np.convolve(env, np.ones(k)/k, mode='same')
peak = env_s.max()

# voiced gate: frames above a fraction of peak are "active"
GATE = 0.10 * peak
active = env_s > GATE

# onset = rising edge of a positive spectral-flux-ish difference while active.
# We detect local energy minima between activity as cell boundaries, plus
# split long active runs at internal energy dips (re-articulations).
flux = np.maximum(0, np.diff(env_s, prepend=env_s[0]))
onset_thresh = 0.18 * flux.max()

boundaries = [0]
last_onset = -999
MIN_GAP = 12  # frames = 120ms minimum cell
for i in range(1, frames):
    rising = flux[i] > onset_thresh and env_s[i] > GATE
    # also break on transition from silence to active
    silence_to_active = (not active[i-1]) and active[i]
    if (rising or silence_to_active) and (i - last_onset) >= MIN_GAP:
        boundaries.append(i)
        last_onset = i
boundaries.append(frames)
boundaries = sorted(set(boundaries))

# ── f0 for pitch tagging ────────────────────────────────────────────────
f0, t = pw.harvest(a, sr, f0_floor=80, f0_ceil=520, frame_period=10)

cells = []
for bi in range(len(boundaries) - 1):
    s_f, e_f = boundaries[bi], boundaries[bi+1]
    seg = env_s[s_f:e_f]
    if len(seg) == 0 or seg.max() < GATE:
        continue  # silent gap
    from_ms = s_f * 10
    to_ms   = e_f * 10
    # trim trailing silence within the cell
    voiced_idx = np.where(seg > GATE)[0]
    if len(voiced_idx) == 0:
        continue
    to_ms = (s_f + voiced_idx[-1] + 1) * 10
    if to_ms - from_ms < 100:
        continue
    # median voiced f0 in the cell
    f0_seg = f0[(s_f if s_f < len(f0) else len(f0)-1):min(e_f, len(f0))]
    f0_v = f0_seg[f0_seg > 0]
    if len(f0_v) < 3:
        continue
    hz = float(np.median(f0_v))
    midi = hz_to_midi(hz)
    rms = float(np.sqrt(np.mean(a[int(from_ms/1000*sr):int(to_ms/1000*sr)]**2)))
    cells.append({
        "i": len(cells),
        "from_ms": int(from_ms),
        "to_ms": int(to_ms),
        "dur_ms": int(to_ms - from_ms),
        "hz": round(hz, 1),
        "note": note_name(midi),
        "midi": midi,
        "rms": round(rms, 4),
    })

# ── slice clean WAVs (15ms pre-roll, 40ms tail, micro-fades) ───────────
PRE, TAIL = 0.015, 0.040
for c in cells:
    start = max(0.0, c["from_ms"]/1000 - PRE)
    length = (c["to_ms"] - c["from_ms"])/1000 + PRE + TAIL
    out = os.path.join(SNAP, f"cell{c['i']:02d}_{c['note']}.wav")
    fade_out_st = max(0.0, length - 0.03)
    subprocess.run([
        "ffmpeg", "-y", "-loglevel", "error",
        "-ss", f"{start:.3f}", "-t", f"{length:.3f}", "-i", SRC,
        "-af", f"afade=t=in:st=0:d=0.006,afade=t=out:st={fade_out_st:.3f}:d=0.03",
        "-ar", "48000", "-ac", "1", "-c:a", "pcm_s16le", out,
    ], check=True)
    c["file"] = os.path.relpath(out, ROOT)

with open(os.path.join(ROOT, "cells.json"), "w") as f:
    json.dump({"src": os.path.relpath(SRC, ROOT), "dur_s": round(dur,2),
               "sr": sr, "n_cells": len(cells), "cells": cells}, f, indent=2)

print(f"{len(cells)} cells, {dur:.1f}s source")
for c in cells:
    print(f"  cell{c['i']:02d}  {c['from_ms']/1000:5.2f}s +{c['dur_ms']:4d}ms  "
          f"{c['note']:>3} ({c['hz']:.0f}Hz)  rms={c['rms']:.3f}")
