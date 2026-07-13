#!/usr/bin/env python3
"""floor.py — what f0_floor does Puett actually need?

pitchsnap_world.py hardcodes f0_floor=90.0, tuned for jeffrey-pvc's baritone.
If a speaker's real f0 dips below the floor, harvest calls those frames
UNVOICED — they fall through to unmodified speech and never sing.

Measure the true f0 distribution with a permissive floor, then sweep floors
and report the voiced-frame ratio at each.
"""
import sys
import numpy as np
import pyworld as pw
import soundfile as sf

src = sys.argv[1]
x, fs = sf.read(src)
if x.ndim > 1:
    x = x.mean(axis=1)
x = x.astype(np.float64)

# permissive floor to see the TRUE distribution
f0_raw, t = pw.harvest(x, fs, f0_floor=55.0, f0_ceil=700.0, frame_period=5.0)
f0 = pw.stonemask(x, f0_raw, t, fs)
v = f0[f0 > 0]

print(f"source: {src}  ({len(x)/fs:.1f}s)")
print(f"\ntrue f0 distribution (floor=55Hz, permissive):")
print(f"  voiced frames : {len(v)}/{len(f0)}  ({100*len(v)/len(f0):.0f}%)")
if len(v):
    for p in (1, 5, 10, 25, 50, 75, 95):
        print(f"  p{p:<2}           : {np.percentile(v, p):6.1f} Hz")
    print(f"  min / max     : {v.min():.1f} / {v.max():.1f} Hz")
    below90 = (v < 90).sum()
    print(f"\n  frames below the hardcoded 90Hz floor: {below90} "
          f"({100*below90/len(v):.1f}% of voiced)  ← these are being thrown away")

print(f"\nvoiced-ratio vs f0_floor:")
print(f"  {'floor':>7}  {'voiced':>7}  {'ratio':>6}")
for floor in (55, 60, 65, 70, 75, 80, 90, 100):
    fr, tt = pw.harvest(x, fs, f0_floor=float(floor), f0_ceil=700.0, frame_period=5.0)
    ff = pw.stonemask(x, fr, tt, fs)
    ratio = 100 * (ff > 0).sum() / len(ff)
    tag = "  ← current default" if floor == 90 else ""
    print(f"  {floor:>5} Hz  {(ff>0).sum():>7}  {ratio:>5.0f}%{tag}")
