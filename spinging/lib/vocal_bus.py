#!/usr/bin/env python3
"""
vocal_bus.py — spinging vocal-bus finishing (round 3).

  reverb IN.wav OUT.wav [wet_db=-16] [decay_s=1.1]
      Soft Schroeder reverb halo on the vocal bus (angelic): 20ms pre-delay,
      4 combs + 2 allpasses, wet lowpassed ~5kHz, mixed quietly under the dry.

  scan IN.wav
      Click/glitch scan (waveform discontinuity + spectral-flux spikes) —
      prints the vocal_shapes.click_scan JSON. Hard QA gate.
"""
import json
import os
import sys

import numpy as np
import soundfile as sf
from scipy.signal import lfilter

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from vocal_shapes import click_scan


def comb(x, delay, fb):
    # y[n] = x[n-d] + fb·y[n-d]
    b = np.zeros(delay + 1)
    b[delay] = 1.0
    a = np.zeros(delay + 1)
    a[0] = 1.0
    a[delay] = -fb
    return lfilter(b, a, x)


def allpass(x, delay, g=0.5):
    # y[n] = -g·x[n] + x[n-d] + g·y[n-d]
    b = np.zeros(delay + 1)
    b[0] = -g
    b[delay] = 1.0
    a = np.zeros(delay + 1)
    a[0] = 1.0
    a[delay] = -g
    return lfilter(b, a, x)


def onepole_lp(x, fs, fc):
    aa = np.exp(-2.0 * np.pi * fc / fs)
    return lfilter([1 - aa], [1, -aa], x)


def reverb(x, fs, wet_db=-16.0, decay_s=1.1):
    pre = int(0.020 * fs)
    dx = np.concatenate([np.zeros(pre), x])
    combs = [int(fs * d) for d in (0.0297, 0.0371, 0.0411, 0.0437)]
    wet = np.zeros(len(dx))
    for d in combs:
        fb = 10 ** (-3.0 * d / fs / decay_s)   # RT60 = decay_s
        wet += comb(dx, d, fb)
    wet /= len(combs)
    for d in (int(fs * 0.005), int(fs * 0.0017)):
        wet = allpass(wet, d)
    wet = onepole_lp(wet, fs, 5000.0)
    g = 10 ** (wet_db / 20.0)
    n = len(x)
    tail = int(min(decay_s, 1.5) * fs)
    out = np.zeros(n + tail)
    out[:n] += x
    L = min(len(out), len(wet))
    out[:L] += g * wet[:L]
    out = out[:n]                              # keep exact input length
    peak = np.abs(out).max()
    if peak > 0.98:
        out *= 0.98 / peak
    return out


def main():
    cmd = sys.argv[1]
    if cmd == "reverb":
        src, dst = sys.argv[2], sys.argv[3]
        wet_db = float(sys.argv[4]) if len(sys.argv) > 4 else -16.0
        decay = float(sys.argv[5]) if len(sys.argv) > 5 else 1.1
        x, fs = sf.read(src, dtype="float64")
        if x.ndim > 1:
            x = x.mean(axis=1)
        y = reverb(x, fs, wet_db, decay)
        sf.write(dst, y.astype(np.float32), fs)
        print(json.dumps({"ok": True, "wet_db": wet_db, "decay_s": decay,
                          "len_s": round(len(y) / fs, 3)}))
    elif cmd == "scan":
        x, fs = sf.read(sys.argv[2], dtype="float64")
        if x.ndim > 1:
            x = x.mean(axis=1)
        print(json.dumps(click_scan(x, fs)))
    else:
        print("usage: vocal_bus.py reverb IN OUT [wet_db] [decay_s] | scan IN",
              file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
