#!/usr/bin/env python3
# rebuild-drums.py — fully data-driven drum rebuild. For every onset in
# the demucs drums stem we measure pitch (spectral centroid), decay
# (peak → half-amplitude time), peak amplitude, and brightness
# (rolloff/centroid ratio). We then synthesize a pitched percussion hit
# matching those four features and place it at the original timing.
#
# No kick/snare/hat classification — the source's drum break is mid-
# band synthesized perc (centroids 700-2200 Hz, very low ZCR), so a
# class-based kit doesn't fit. We just rebuild what we measured.
#
# Outputs:
#   drums-rebuilt.wav  — synth-only re-creation
#   drums-ab.wav       — orig | rebuilt | orig | rebuilt (4 bars each)

from pathlib import Path
import warnings

import numpy as np
import librosa
import soundfile as sf

warnings.filterwarnings("ignore")

HERE = Path(__file__).parent
DRUMS_PATH = HERE / "stems/htdemucs/drums.wav"
OUT_REBUILT = HERE / "drums-rebuilt.wav"
OUT_AB = HERE / "drums-ab.wav"

SR = 44100
BPM = 101.33
BAR_S = 60 / BPM * 4
LOOP_START = 0.813
LOOP_S = BAR_S * 4

y, _ = librosa.load(str(DRUMS_PATH), sr=SR, mono=False)
if y.ndim == 1: y = np.stack([y, y])
mono = y.mean(axis=0).astype(np.float32)

onsets = librosa.onset.onset_detect(
    y=mono, sr=SR, backtrack=True, hop_length=256,
    units="samples", delta=0.04,
)
print(f"detected {len(onsets)} onsets")

SPEC_WIN = int(0.030 * SR)   # 30ms clean window for spectral features
ENV_WIN = int(0.200 * SR)    # 200ms window for envelope/decay (overlaps allowed)
hits = []
for k, onset in enumerate(onsets):
    a = int(onset)
    nxt = int(onsets[k + 1]) if k + 1 < len(onsets) else len(mono)
    # spectrum: short, cut at next onset to keep the snapshot clean
    sb = min(a + SPEC_WIN, nxt, len(mono))
    if sb - a < int(0.015 * SR): continue
    swin = mono[a:sb]
    if np.abs(swin).max() < 0.025: continue

    S = np.abs(np.fft.rfft(swin * np.hanning(sb - a)))
    freqs = np.fft.rfftfreq(sb - a, 1.0 / SR)
    energy = S.sum() + 1e-9
    centroid = float((freqs * S).sum() / energy)
    csum = np.cumsum(S)
    rolloff = float(freqs[min(int(np.searchsorted(csum, 0.85 * csum[-1])),
                              len(freqs) - 1)])

    # decay: take the LOCAL peak in the first 30ms, then walk the
    # smoothed envelope forward until it crosses below half. Bound the
    # search at the next onset minus a small overlap budget.
    eb = min(a + ENV_WIN, len(mono))
    ewin = mono[a:eb]
    smooth_n = max(1, int(0.005 * SR))
    env = np.convolve(np.abs(ewin), np.ones(smooth_n, dtype=np.float32) / smooth_n,
                      mode="same")
    local_end = min(int(0.030 * SR), len(env))
    pk_idx = int(np.argmax(env[:local_end])) if local_end > 0 else 0
    pk = float(env[pk_idx])
    # bound the decay search to ~10ms before the next onset
    search_end = min(len(env), max(pk_idx + smooth_n,
                                   (nxt - a) - int(0.010 * SR)))
    half = pk * 0.5
    decay_samples = search_end - pk_idx
    for i in range(pk_idx + 1, search_end):
        if env[i] < half:
            decay_samples = i - pk_idx; break
    decay_ms = max(5.0, min(180.0, decay_samples / SR * 1000))

    brightness = float(min(2.5, rolloff / (centroid + 1e-9)))  # 1.0+ → harmonic

    hits.append({
        "sample": a + pk_idx,            # align synth attack to measured peak
        "pitch_hz": max(60.0, min(centroid, 2500.0)),
        "decay_ms": decay_ms,
        "peak": pk,
        "brightness": brightness,
    })

print(f"kept {len(hits)} hits")
print("first few:")
for h in hits[:8]:
    print(f"  @{h['sample']/SR:6.3f}s  pitch={h['pitch_hz']:6.1f}Hz "
          f"decay={h['decay_ms']:5.1f}ms  peak={h['peak']:.2f}  "
          f"bright={h['brightness']:.2f}")

# ── synth voice: pitched perc with measured envelope ─────────────────
rng = np.random.default_rng(11)

def perc_voice(pitch_hz, decay_ms, peak_amp, brightness):
    # 4 half-lives covers the tail
    dur_s = max(0.04, (decay_ms / 1000.0) * 4)
    n = int(dur_s * SR); t = np.arange(n) / SR
    # exponential decay with rate so amp halves at decay_ms
    k = np.log(2.0) / max(0.001, decay_ms / 1000.0)
    env = np.exp(-t * k).astype(np.float32)

    y = np.sin(2 * np.pi * pitch_hz * t).astype(np.float32) * 0.6
    if brightness > 0:
        # add 2nd + inharmonic 2.76× (bell-like) scaled by brightness
        b2 = min(1.0, (brightness - 1.0) * 0.6)
        if b2 > 0:
            y += np.sin(2 * np.pi * pitch_hz * 2.0 * t).astype(np.float32) * 0.30 * b2
            y += np.sin(2 * np.pi * pitch_hz * 2.76 * t).astype(np.float32) * 0.15 * b2

    y *= env

    # tiny click at attack (~3 ms) — gives the transient its bite
    click_n = int(0.003 * SR)
    if click_n > 0:
        noise = rng.standard_normal(click_n).astype(np.float32)
        y[:click_n] += noise * 0.18 * (peak_amp * 0.7 + 0.3)

    return y * (peak_amp * 0.85 + 0.15)

# ── place synth hits at measured peak timing ─────────────────────────
total_n = len(mono) + int(0.5 * SR)
rebuilt = np.zeros(total_n, dtype=np.float32)
for h in hits:
    seg = perc_voice(h["pitch_hz"], h["decay_ms"], h["peak"], h["brightness"])
    a = h["sample"]; b = min(a + len(seg), total_n)
    rebuilt[a:b] += seg[:b - a]

peak = float(np.abs(rebuilt).max())
if peak > 0.95: rebuilt *= 0.95 / peak

haas = int(0.0025 * SR)
right = np.concatenate([np.zeros(haas, dtype=np.float32), rebuilt[:-haas]])
rebuilt_stereo = np.stack([rebuilt, right * 0.98])
sf.write(str(OUT_REBUILT), rebuilt_stereo.T, SR)
print(f"wrote {OUT_REBUILT.name}, {rebuilt_stereo.shape[1]/SR:.2f}s")

# ── A/B: orig | rebuilt | orig | rebuilt (4 bars each) ───────────────
loop_n = int(LOOP_S * SR)
start_n = int(LOOP_START * SR)
end_n = start_n + loop_n
orig_loop = y[:, start_n:end_n]
reb_loop = rebuilt_stereo[:, start_n:end_n]
ab = np.concatenate([orig_loop, reb_loop, orig_loop, reb_loop], axis=1)
peak2 = float(np.abs(ab).max())
if peak2 > 0.95: ab *= 0.95 / peak2
sf.write(str(OUT_AB), ab.T, SR)
print(f"wrote {OUT_AB.name}, {ab.shape[1]/SR:.2f}s (orig | rebuilt | orig | rebuilt)")
