#!/usr/bin/env python3
# place-vocal.py — sample-accurate vocal-stem assembly.
#
# Reads a placements JSON ({sr, total_s, placements:[{file, at_s, gain}]}) and
# sums each (already WORLD-locked + stretched) sung stem onto a silent stereo
# canvas at its exact sample offset. Replaces the fragile 90-input ffmpeg
# asplit/adelay/amix filtergraph (which leaked a stem to t=0). Output is a
# raw float32 stereo WAV; bake.mjs then runs the forward-bus EQ over it.
#
# Usage: place-vocal.py <placements.json> <out.wav>

import json, sys
import numpy as np
import soundfile as sf

spec = json.load(open(sys.argv[1]))
out_path = sys.argv[2]
sr = spec["sr"]
n = int(spec["total_s"] * sr)
buf = np.zeros((n, 2), dtype=np.float64)

placed = 0
for p in spec["placements"]:
    x, xsr = sf.read(p["file"], dtype="float64")
    if x.ndim == 1:
        x = np.stack([x, x], axis=1)         # mono → stereo
    if xsr != sr:                            # nearest-neighbour-ish guard (stems are 48k)
        idx = (np.arange(int(len(x) * sr / xsr)) * xsr / sr).astype(int)
        idx = idx[idx < len(x)]
        x = x[idx]
    g = float(p.get("gain", 1.0))
    s0 = int(round(p["at_s"] * sr))
    s1 = min(n, s0 + len(x))
    if s0 >= n:
        continue
    buf[s0:s1] += x[: s1 - s0] * g
    placed += 1

peak = float(np.max(np.abs(buf)))
sf.write(out_path, buf.astype(np.float32), sr, subtype="FLOAT")
print(f"[place-vocal] {placed} stems → {out_path}  ({n/sr:.1f}s, peak {peak:.3f})")
