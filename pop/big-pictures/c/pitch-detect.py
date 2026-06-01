#!/usr/bin/env python3
# pitch-detect.py — robust per-file base-pitch estimation via the YIN
# algorithm (cumulative mean normalized difference function + absolute
# threshold). YIN is far more octave-stable than plain autocorrelation,
# which kept locking onto 2nd harmonics. Prints one line per file:
#     <path>\t<f0_hz>\t<midi>      (or  <path>\tnan\tnan  if unvoiced)
# numpy-only.
#
# Flags:  --track <file>   print a per-frame f0 track (time f0 midi) and exit
import sys, wave, math
import numpy as np

FMIN, FMAX = 70.0, 500.0          # search range (YIN won't octave-jump within it)
WIN, HOP = 2048, 512
YIN_THRESH = 0.15                 # absolute threshold for the first dip

def load(path):
    w = wave.open(path, "rb")
    sr = w.getframerate(); ch = w.getnchannels(); n = w.getnframes()
    a = np.frombuffer(w.readframes(n), dtype=np.int16).astype(np.float64)
    w.close()
    if ch > 1:
        a = a.reshape(-1, ch).mean(axis=1)
    return a / 32768.0, sr

def yin_f0(frame, sr):
    """YIN fundamental for one frame, or None if unvoiced."""
    if math.sqrt(float(np.mean(frame ** 2)) + 1e-12) < 3e-3:
        return None
    n = len(frame)
    tau_max = min(int(sr / FMIN), n - 1)
    tau_min = max(2, int(sr / FMAX))
    x = frame - frame.mean()
    # difference function via FFT autocorrelation:
    #   d(tau) = r(0,n-tau) + r(tau,n) - 2*acf(tau)
    acf = np.fft.irfft(np.abs(np.fft.rfft(x, 2 * n)) ** 2)[:tau_max + 1]
    power = np.concatenate(([0.0], np.cumsum(x ** 2)))         # prefix sums
    d = np.empty(tau_max + 1)
    for tau in range(tau_max + 1):
        e0 = power[n - tau]                 # energy of x[0 : n-tau]
        et = power[n] - power[tau]          # energy of x[tau : n]
        d[tau] = e0 + et - 2 * acf[tau]
    # cumulative mean normalized difference
    d[0] = 1.0
    cms = np.empty(tau_max + 1); cms[0] = 1.0
    run = 0.0
    for tau in range(1, tau_max + 1):
        run += d[tau]
        cms[tau] = d[tau] * tau / run if run > 0 else 1.0
    # absolute threshold: first tau (>=tau_min) dipping below YIN_THRESH at a
    # local minimum; else the global min in range.
    tau = -1
    t = tau_min
    while t < tau_max:
        if cms[t] < YIN_THRESH:
            while t + 1 < tau_max and cms[t + 1] < cms[t]:
                t += 1
            tau = t
            break
        t += 1
    if tau == -1:
        tau = tau_min + int(np.argmin(cms[tau_min:tau_max]))
        if cms[tau] > 0.6:                  # nothing convincingly periodic
            return None
    # parabolic interpolation around tau
    if 1 <= tau < tau_max:
        a0, b0, c0 = cms[tau - 1], cms[tau], cms[tau + 1]
        denom = a0 - 2 * b0 + c0
        if denom != 0:
            tau = tau + 0.5 * (a0 - c0) / denom
    f0 = sr / tau
    return f0 if FMIN <= f0 <= FMAX else None

def track(path):
    a, sr = load(path)
    out = []
    for i in range(0, max(1, len(a) - WIN), HOP):
        f = yin_f0(a[i:i + WIN], sr)
        out.append((i / sr, f))
    return out

# Jeffrey's voice lives in one octave band. FOLD every per-frame estimate
# into [FOLD_LO, 2*FOLD_LO) BEFORE taking the median — this collapses the
# frame-level octave errors (spurious jumps to 400+ Hz or 70-90 Hz dips)
# onto the true fundamental, so the per-word base is stable and correct.
FOLD_LO = 100.0                   # band = [100, 200) Hz  (G2..G3)

def fold(f):
    while f < FOLD_LO:
        f *= 2.0
    while f >= 2.0 * FOLD_LO:
        f /= 2.0
    return f

def detect(path):
    f0s = [fold(f) for _, f in track(path) if f]
    if not f0s:
        return None
    return float(np.median(f0s))

if __name__ == "__main__":
    args = sys.argv[1:]
    if args and args[0] == "--track":
        for t, f in track(args[1]):
            if f:
                print(f"{t:5.2f}\t{f:7.2f}\t{69 + 12 * math.log2(f / 440.0):5.2f}")
            else:
                print(f"{t:5.2f}\t   -   \t  -")
        sys.exit(0)
    for path in args:
        f0 = detect(path)
        if f0 is None:
            print(f"{path}\tnan\tnan")
        else:
            print(f"{path}\t{f0:.2f}\t{69 + 12 * math.log2(f0 / 440.0):.2f}")
