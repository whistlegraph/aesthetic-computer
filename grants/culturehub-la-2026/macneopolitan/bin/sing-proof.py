#!/usr/bin/env python
# sing-proof.py — proof of the aesthetivox machine-voice route: take a line
# spoken by a member's cast voice and lift it onto a melody with WORLD f0
# replacement (envelope untouched, so it stays that machine's voice).
# Crude next to spinging's phoneme-aware engine — equal time slices, no
# vowel anchoring — this only proves the timbre identity survives the lift.
# Run with pop/.venv python. Usage: sing-proof.py in.wav out.wav n1 n2 ...
import sys
import numpy as np
import pyworld
import soundfile as sf

inp, outp = sys.argv[1], sys.argv[2]
midi = [float(m) for m in sys.argv[3:]] or [50, 50, 53, 55, 57, 55, 53, 50]

x, fs = sf.read(inp)
if x.ndim > 1:
    x = x.mean(axis=1)
x = np.ascontiguousarray(x, dtype=np.float64)

f0, t = pyworld.harvest(x, fs, f0_floor=60.0, f0_ceil=300.0)
sp = pyworld.cheaptrick(x, f0, t, fs)
ap = pyworld.d4c(x, f0, t, fs)

hz = 440.0 * 2 ** ((np.array(midi) - 69) / 12)
n = len(f0)
notes = np.repeat(hz, int(np.ceil(n / len(hz))))[:n]

# vibrato rate = 4x the member's battery cycles/day (neo 1.6 -> 6.4 Hz,
# blueberry 1.0 -> 4.0 Hz), 18 cents deep; override with VIB_HZ env.
import os
vib_hz = float(os.environ.get("VIB_HZ", "6.4"))
frame_s = t[1] - t[0] if len(t) > 1 else 0.005
vib = 2 ** (18 / 1200 * np.sin(2 * np.pi * vib_hz * np.arange(n) * frame_s))
new_f0 = np.where(f0 > 0, notes * vib, 0.0)

y = pyworld.synthesize(new_f0, sp, ap, fs)
y = y / max(1e-9, np.abs(y).max()) * 0.89
sf.write(outp, y, fs)
print(outp)
