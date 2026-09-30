#!/usr/bin/env python3
"""choir.py — accompaniment voices behind her, two ways.

  SAGE CHOIR   her own frequency profile: WORLD (harvest → cheaptrick → d4c)
               on the regularized lead, then RESYNTHESIS with her spectral
               envelope and aperiodicity per frame but the f0 replaced by a
               chord tone held machine-steady (a little vibrato). Her vowels
               and consonant air follow her line; the pitch is the chord.
               Three voices, low / mid / high, below her register mostly,
               each only sounding where she is voiced. A vocoder choir of her.
  JEFFREY      pop/voice/lab/jeffrey-pvc/*-iso.mp3 — @jeffrey's isolated
               sustained vowels (ah / iy / uw). WORLD-analysed once, the frames
               looped, resynthesized on the bar's root (and fifth) two octaves
               under her: a computer drone that changes vowel with the chord
               (ah on G#m, uw on B, iy on Emaj7).

Outputs, in the regularized clock (src/vox/reg/):
  choir-low.wav choir-mid.wav choir-high.wav jeffrey-root.wav jeffrey-fifth.wav
  + choir.json (per-bar tones)

  pop/.venv/bin/python pop/sailor-song/bin/choir.py [--vib 0.10]
"""
import argparse, json, os, sys, warnings
import numpy as np, soundfile as sf
with warnings.catch_warnings():
    warnings.simplefilter("ignore"); import pyworld as pw
import librosa

ap = argparse.ArgumentParser()
ap.add_argument("--vib", type=float, default=0.10)     # semitones of vibrato depth
ap.add_argument("--vib-hz", type=float, default=5.2)
ap.add_argument("--stems", default="reg")          # reg (regularized clock) or raw (her take as played)
a = ap.parse_args()

LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ROOT = os.path.dirname(os.path.dirname(LANE))
REG = os.path.join(LANE, "src/vox", a.stems)
M = json.load(open(os.path.join(LANE, "measures.reg.json" if a.stems == "reg" else "measures.json")))["bars"]
A = json.load(open(os.path.join(LANE, "take.analysis.json")))
TUNE = A["tuningCents"] / 100.0
FP = 5.0  # ms

def mtof(m): return 440.0 * 2 ** ((m + TUNE - 69) / 12)

# chord tones per bar: pcs in the guitar frame (G#m = 8 11 3, B = 11 3 6, Emaj7 sounds G#m here)
PCS = {"G#m": [8, 11, 3], "Emaj7": [8, 11, 3, 4], "B": [11, 3, 6]}
ROOTPC = {"G#m": 8, "Emaj7": 8, "B": 11}
VOWEL = {"G#m": "ah", "Emaj7": "iy", "B": "uw"}

def nearest(pcs, target, lo, hi):
    c = [m for m in range(lo, hi + 1) if m % 12 in pcs]
    return min(c, key=lambda m: abs(m - target))

# ── SAGE CHOIR ───────────────────────────────────────────────────────────
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
# require ≥ 60 ms voiced runs (bleed blips out)
runs = np.diff(np.concatenate([[0], voiced.astype(int), [0]]))
for s0, s1 in zip(np.where(runs == 1)[0], np.where(runs == -1)[0]):
    if (s1 - s0) * FP < 60: voiced[s0:s1] = False
nf = len(t)
bar_of = np.zeros(nf, int) - 1
for i, b in enumerate(M):
    a0, a1 = int(b["t"] * 1000 / FP), int((b["t"] + b["dur"]) * 1000 / FP)
    bar_of[a0:a1] = i

# voice leading: three voices, nearest chord tone per bar, no doubling, kept below her (she sits 56–68)
voices = {"low": 44, "mid": 49, "high": 53}
ranges = {"low": (41, 51), "mid": (46, 56), "high": (51, 60)}
tones = {k: np.zeros(len(M), int) for k in voices}
cur = dict(voices)
for i, b in enumerate(M):
    used = set()
    for k in ("low", "mid", "high"):
        pcs = [p for p in PCS[b["chord"]] if p not in used] or PCS[b["chord"]]
        m = nearest(pcs, cur[k], *ranges[k]); cur[k] = m; tones[k][i] = m; used.add(m % 12)

def vibrato(n, depth_st, hz):
    tt = np.arange(n) * FP / 1000
    return 2 ** (depth_st * np.sin(2 * np.pi * hz * tt + 0.7) / 12)

vib = vibrato(nf, a.vib, a.vib_hz)
out_tones = {}
for k in voices:
    f = np.zeros(nf)
    for i in range(nf):
        if voiced[i] and bar_of[i] >= 0: f[i] = mtof(tones[k][bar_of[i]]) * vib[i]
    # glide between bars over 60 ms so the tone changes don't click
    fm = f.copy(); k60 = 12
    for i in range(1, nf):
        if f[i] > 0 and fm[i - 1] > 0: fm[i] = fm[i - 1] + (f[i] - fm[i - 1]) / k60
    print(f"  choir {k}: synthesizing …", flush=True)
    y = pw.synthesize(np.ascontiguousarray(fm), sp, apr, fs, frame_period=FP)
    y = y[:len(x)]
    # gate to her voiced regions with 20 ms fades (WORLD hums through gaps otherwise)
    g = np.repeat(voiced.astype(float), int(fs * FP / 1000))[:len(y)]
    g = np.convolve(g, np.ones(int(0.08 * fs)) / int(0.08 * fs), mode="same")   # v12: 80 ms swells, not 20 ms chops
    y = y * g
    y *= 0.5 / (np.max(np.abs(y)) + 1e-9)
    sf.write(os.path.join(REG, f"choir-{k}.wav"), y.astype(np.float32), fs)
    out_tones[k] = tones[k].tolist()

# ── JEFFREY vowel drone ──────────────────────────────────────────────────
LAB = os.path.join(ROOT, "pop/voice/lab/jeffrey-pvc")
vow = {}
for v in ("ah", "iy", "uw"):
    p = os.path.join(LAB, f"jeffrey-pvc-{v}-iso.mp3")
    if not os.path.exists(p): print(f"  ! missing {p}"); continue
    z, zfs = librosa.load(p, sr=fs, mono=True)
    z = z.astype(np.float64)
    # trim silence, keep the steady middle
    zi = np.where(np.abs(z) > 0.02)[0]
    if len(zi) < fs // 4: continue
    z = z[zi[0]:zi[-1]]; z = z[len(z) // 5: -len(z) // 5]
    zf0, zt = pw.harvest(z, fs, f0_floor=60, f0_ceil=400, frame_period=FP)
    zf0 = pw.stonemask(z, zf0, zt, fs)
    zfft = pw.get_cheaptrick_fft_size(fs, f0_floor=60.0)
    zsp = pw.cheaptrick(z, zf0, zt, fs, fft_size=zfft, f0_floor=60.0)
    zap = pw.d4c(z, zf0, zt, fs, fft_size=zfft)
    keep = zf0 > 0
    vow[v] = (zsp[keep], zap[keep])
    print(f"  jeffrey {v}: {keep.sum()} voiced frames at ~{np.median(zf0[keep]):.0f} Hz")
if vow:
    total = int(M[-1]["t"] + M[-1]["dur"]) + 2
    nfj = int(total * 1000 / FP)
    fftj = next(iter(vow.values()))[0].shape[1]
    for name, interval in (("root", 0), ("fifth", 7)):
        spj = np.zeros((nfj, fftj)); apj = np.ones((nfj, fftj)); fj = np.zeros(nfj)
        vibj = vibrato(nfj, 0.12, 4.0)
        for i, b in enumerate(M):
            a0, a1 = int(b["t"] * 1000 / FP), min(nfj, int((b["t"] + b["dur"]) * 1000 / FP))
            v = VOWEL[b["chord"]] if VOWEL[b["chord"]] in vow else next(iter(vow))
            vs, va = vow[v]; n = len(vs)
            midi = 32 + ((ROOTPC[b["chord"]] - 8) % 12) + interval          # G#1 / B1 root, fifth above
            if midi > 39: midi -= 12
            for k, fr in enumerate(range(a0, a1)):
                j = (k // 2) % n if n else 0                                  # half-speed loop of the vowel's frames
                spj[fr] = vs[j]; apj[fr] = va[j]; fj[fr] = mtof(midi) * vibj[fr]
        # 80 ms glide at bar lines
        for i in range(1, nfj):
            if fj[i] > 0 and fj[i - 1] > 0: fj[i] = fj[i - 1] + (fj[i] - fj[i - 1]) / 16
        print(f"  jeffrey {name}: synthesizing …", flush=True)
        y = pw.synthesize(np.ascontiguousarray(fj), np.ascontiguousarray(spj), np.ascontiguousarray(apj), fs, frame_period=FP)
        y *= 0.5 / (np.max(np.abs(y)) + 1e-9)
        sf.write(os.path.join(REG, f"jeffrey-{name}.wav"), y.astype(np.float32), fs)
json.dump({"frameMs": FP, "tones": out_tones, "vowels": VOWEL, "jeffrey": sorted(vow)}, open(os.path.join(LANE, "choir.json"), "w"), indent=1)
print("✓", REG)
