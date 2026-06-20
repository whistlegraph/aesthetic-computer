#!/usr/bin/env python3
# accel.py — apply a gradual tempo ACCELERANDO to a finished stereo track via a
# rubberband time-map. Pitch-preserving and applied to the whole mix at once, so
# every layer (bed + vocal) speeds up together in sync — no drift. The
# instantaneous tempo ramps from 1.0× at the start to (1+ramp)× by the end.
#
# Usage: accel.py <in.wav> <out.wav> [ramp]   (ramp default 0.10 = +10% by end)

import sys, subprocess, tempfile, os, math
import soundfile as sf

inp, out = sys.argv[1], sys.argv[2]
ramp = float(sys.argv[3]) if len(sys.argv) > 3 else 0.10

info = sf.info(inp); sr = info.samplerate; nin = info.frames; T = nin / sr
R = ramp

# output time as a function of input time for a linear tempo ramp f(u)=1+R*u/T
# (pitch-preserving speed-up): v(u) = ∫₀ᵘ du'/f(u') = (T/R)·ln(1+R·u/T).
def v(u): return (T / R) * math.log(1 + R * u / T) if R > 1e-6 else u
Tout = v(T)

kf = []
steps = 600
for k in range(steps + 1):
    u = T * k / steps
    kf.append([int(round(u * sr)), int(round(v(u) * sr))])
for i in range(1, len(kf)):
    if kf[i][1] <= kf[i - 1][1]:
        kf[i][1] = kf[i - 1][1] + 1

ratio = Tout / T
with tempfile.NamedTemporaryFile("w", suffix=".txt", delete=False) as f:
    for a, b in kf:
        f.write(f"{a} {b}\n")
    tmap = f.name

subprocess.run(["rubberband", "-3", "--time", f"{ratio:.6f}", "--timemap", tmap, inp, out],
               check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
os.unlink(tmap)
print(f"accel: tempo 1.0×→{1+R:.2f}× ramp, {T:.1f}s → {Tout:.1f}s")
