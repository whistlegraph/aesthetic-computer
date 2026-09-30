#!/usr/bin/env python3
"""sister.py — a sister for s@ge: her line, another voice, other octaves.

The harmony stems (aesthetivox.py) keep her formants, so they are HER at
another pitch. A sister keeps the line but changes the singer: the WORLD
spectral envelope (cheaptrick) is warped along the frequency axis — a
smaller vocal tract for the high sister (formants up), a larger one for the
low sister (formants down) — the aperiodicity follows, the f0 moves an
octave, the vibrato is her own rate but her own depth is replaced with the
sister's, and the whole line is a few milliseconds behind her, like a second
person breathing with her. Vowels only where she is voiced; her consonants
stay in the lead.

  sister-high.wav   +12 st, formants x1.09, vibrato 5.6 Hz / 0.18 st, 14 ms late
  sister-low.wav    -12 st, formants x0.94, vibrato 4.4 Hz / 0.12 st, 22 ms late

  pop/.venv/bin/python pop/sailor-song/bin/sister.py      → src/vox/reg/sister-{high,low}.wav
"""
import os, warnings
import numpy as np, soundfile as sf
with warnings.catch_warnings():
    warnings.simplefilter("ignore"); import pyworld as pw

import sys
LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
STEMS = sys.argv[sys.argv.index("--stems") + 1] if "--stems" in sys.argv else "reg"
REG = os.path.join(LANE, "src/vox", STEMS)
FP = 5.0
SISTERS = {
    "high": dict(st=+12, formant=1.09, vib_hz=5.6, vib_st=0.18, late_ms=14, gain=0.9),
    "low":  dict(st=-12, formant=0.94, vib_hz=4.4, vib_st=0.12, late_ms=22, gain=0.9),
}

x, fs = sf.read(os.path.join(REG, "vocals-aesthetivox.wav"))
if x.ndim > 1: x = x.mean(1)
x = x.astype(np.float64)
print("WORLD on her lead …", flush=True)
f0, t = pw.harvest(x, fs, f0_floor=150, f0_ceil=900, frame_period=FP)
f0 = pw.stonemask(x, f0, t, fs)
fft = pw.get_cheaptrick_fft_size(fs, f0_floor=150.0)
sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft, f0_floor=150.0)
apr = pw.d4c(x, f0, t, fs, fft_size=fft)
voiced = f0 > 0
runs = np.diff(np.concatenate([[0], voiced.astype(int), [0]]))
for s0, s1 in zip(np.where(runs == 1)[0], np.where(runs == -1)[0]):
    if (s1 - s0) * FP < 60: voiced[s0:s1] = False
nf, nb = sp.shape
bins = np.arange(nb)

def warp(env, k):
    """Resample the envelope along frequency: bin i of the sister reads bin i/k of hers."""
    src = np.clip(bins / k, 0, nb - 1)
    lo = np.floor(src).astype(int); hi = np.minimum(lo + 1, nb - 1); fr = src - lo
    return env[:, lo] * (1 - fr) + env[:, hi] * fr

# her vibrato is smoothed away and the sister's put on: a 150 ms running mean of f0 in cents
m = np.where(voiced, 12 * np.log2(np.maximum(f0, 1) / 440) + 69, np.nan)
k = int(150 / FP)
ms = np.copy(m)
for i in range(nf):
    a0, a1 = max(0, i - k // 2), min(nf, i + k // 2 + 1); seg = m[a0:a1]; seg = seg[np.isfinite(seg)]
    ms[i] = seg.mean() if len(seg) else np.nan
tt = np.arange(nf) * FP / 1000
for name, S in SISTERS.items():
    vib = S["vib_st"] * np.sin(2 * np.pi * S["vib_hz"] * tt + 1.3)
    mm = np.where(np.isfinite(ms), ms, np.nan) + S["st"] + vib
    fz = np.where(voiced & np.isfinite(mm), 440 * 2 ** ((mm - 69) / 12), 0.0)
    # keep f0 continuous through gaps (WORLD pops on 0 → tone); silence comes from the gate below
    last = 0.0
    for i in range(nf):
        if fz[i] > 0: last = fz[i]
        elif last > 0: fz[i] = last
    fz = np.where(fz > 0, fz, 220.0)
    spw = warp(sp, S["formant"]); apw = warp(apr, S["formant"])
    print(f"  sister {name}: synthesizing …", flush=True)
    y = pw.synthesize(np.ascontiguousarray(fz), np.ascontiguousarray(spw), np.ascontiguousarray(apw), fs, frame_period=FP)[:len(x)]
    gate = np.repeat(voiced.astype(float), int(fs * FP / 1000))[:len(y)]
    if len(gate) < len(y): gate = np.pad(gate, (0, len(y) - len(gate)))
    w = int(0.08 * fs); gate = np.convolve(gate, np.ones(w) / w, mode="same")   # v12: 80 ms swells
    y = y * gate
    late = int(S["late_ms"] / 1000 * fs); y = np.concatenate([np.zeros(late), y])[:len(x)]
    y *= S["gain"] * 0.5 / (np.max(np.abs(y)) + 1e-9)
    sf.write(os.path.join(REG, f"sister-{name}.wav"), y.astype(np.float32), fs)
# ── the hum: long vowels held per bar from her own sustained frames ──────
# For each bar, the frame in her line with the most low-band energy (a wide
# open vowel) is FROZEN and held for the whole bar at the sister-low octave of
# the chord's root or fifth; in the verses the envelope is rolled off above
# 1.2 kHz so it reads as a closed-mouth hum, in the choruses it opens.
import json
M = json.load(open(os.path.join(LANE, "measures.reg.json" if STEMS == "reg" else "measures.json")))["bars"]
PCS = {"G#m": (8, 3), "Emaj7": (8, 3), "B": (11, 6)}          # root, fifth
def mtof(m): return 440.0 * 2 ** ((m + 0.13 - 69) / 12)
freqs = np.linspace(0, fs / 2, nb)
lowband = (freqs > 200) & (freqs < 900)
energy = sp[:, lowband].mean(1) * voiced
nfh = int((M[-1]["t"] + M[-1]["dur"]) * 1000 / FP) + 2
sph = np.zeros((nfh, nb)); aph = np.ones((nfh, nb)); fh = np.zeros(nfh)
roll = 1 / (1 + (freqs / 1200.0) ** 4)                               # the hum's closed mouth
vibh = 0.10 * np.sin(2 * np.pi * 4.2 * np.arange(nfh) * FP / 1000)
for i, b in enumerate(M):
    a0, a1 = int(b["t"] * 1000 / FP), min(nfh, int((b["t"] + b["dur"]) * 1000 / FP))
    seg = energy[a0:min(a1, nf)]
    j = a0 + int(np.argmax(seg)) if len(seg) and seg.max() > 0 else None
    if j is None:                                                    # she is silent here: hold the last vowel
        prev = [q for q in range(a0 - 1, 0, -1) if voiced[q]]
        j = prev[0] if prev else None
    if j is None: continue
    root, fifth = PCS[b["chord"]]
    midi = 44 + ((root if b["n"] % 4 < 2 else fifth) - 8) % 12         # G#2 / D#3 region, under her
    if midi > 51: midi -= 12
    open_mouth = 1.0 if b["n"] >= 52 else 0.0
    env_row = sp[j] * (roll * (1 - open_mouth) + open_mouth)
    for fr in range(a0, a1):
        sph[fr] = env_row; aph[fr] = apr[j]; fh[fr] = mtof(midi) * 2 ** (vibh[fr] / 12)
for i in range(1, nfh):                                              # 100 ms glide at bar lines
    if fh[i] > 0 and fh[i - 1] > 0: fh[i] = fh[i - 1] + (fh[i] - fh[i - 1]) / 20
fh = np.where(fh > 0, fh, 110.0)
print("  sister hum: synthesizing …", flush=True)
y = pw.synthesize(np.ascontiguousarray(fh), np.ascontiguousarray(sph), np.ascontiguousarray(aph), fs, frame_period=FP)
on = np.repeat((sph.sum(1) > 0).astype(float), int(fs * FP / 1000))[:len(y)]
if len(on) < len(y): on = np.pad(on, (0, len(y) - len(on)))
w = int(0.25 * fs); on = np.convolve(on, np.ones(w) / w, mode="same")   # slow swell in and out
y = y * on; y *= 0.5 / (np.max(np.abs(y)) + 1e-9)
sf.write(os.path.join(REG, "sister-hum.wav"), y.astype(np.float32), fs)
print("✓", REG)
