#!/usr/bin/env python3
"""
worldsing.py — single-note WORLD-vocoder pitch-correction tuned for a
"sung instrument" tone: aggressive aperiodicity scaling (less breath),
unvoiced attenuation (no consonant air between vowels), and an optional
spectral tilt to push toward a hollow tube/vocoder character.

Local to pop/marimba/. The shared pop/bin/pitchsnap_world.py is built
for big-pictures vocals where natural breath/consonants matter; this
helper is for instrument-blend baas/woos where they don't.

Pipeline:
  audio → harvest/stonemask (f0)
        → cheaptrick (spectral envelope = formants)
        → d4c (aperiodicity = breath/noise)
        → SCALE ap down  (less air)
        → REPLACE f0 with constant target pitch
        → synthesize
        → ATTENUATE unvoiced regions (drop in/out breath)

Usage:
  worldsing.py in.wav out.wav --note A3 \\
        --ap-scale 0.10        # 1.0 = WORLD default airy, 0.0 = no breath
        --unvoiced-gain 0.05    # 1.0 = preserve consonants, 0.0 = mute them
"""
import argparse, sys
import numpy as np
import soundfile as sf
import pyworld as pw

NOTE_SEMI = {"c": 0, "d": 2, "e": 4, "f": 5, "g": 7, "a": 9, "b": 11}

def note_to_midi(s):
    s = s.strip().lower()
    semi = NOTE_SEMI[s[0]]
    i = 1
    if i < len(s) and s[i] in "#b":
        semi += 1 if s[i] == "#" else -1
        i += 1
    return 12 * (int(s[i:]) + 1) + semi

def midi_to_hz(m):
    return 440.0 * (2.0 ** ((m - 69.0) / 12.0))

def main():
    p = argparse.ArgumentParser()
    p.add_argument("in_wav")
    p.add_argument("out_wav")
    p.add_argument("--note", required=True,
                   help="Target note name, e.g. A3, Eb4, F#5")
    p.add_argument("--ap-scale", type=float, default=0.10,
                   help="Multiply aperiodicity by this. 1.0 = full breath "
                        "(default WORLD), 0.0 = pure tonal. 0.10 = hollow.")
    p.add_argument("--unvoiced-gain", type=float, default=0.05,
                   help="Gain applied to unvoiced/breath regions. "
                        "1.0 = preserve, 0.0 = mute completely.")
    args = p.parse_args()

    x, fs = sf.read(args.in_wav, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)

    f0_floor = 90.0
    f0_raw, t = pw.harvest(x, fs, f0_floor=f0_floor, f0_ceil=600.0, frame_period=5.0)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft_size = pw.get_cheaptrick_fft_size(fs, f0_floor=f0_floor)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft_size, f0_floor=f0_floor)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft_size)

    # Aperiodicity scale — the main "less air" knob. ap is in [0, 1]
    # per (freq_bin, frame); scaling it down makes WORLD synthesise a
    # more sinusoidal / tonal output instead of breathy speech.
    ap = np.clip(ap * args.ap_scale, 1e-6, 1.0)

    # Replace f0 with constant target across all frames (the source is
    # always a single short syllable, no need for a curve).
    target_hz = midi_to_hz(note_to_midi(args.note))
    n_frames = len(t)
    voiced = f0 > 0
    f0_synth = np.full(n_frames, target_hz, dtype=np.float64)

    y = pw.synthesize(f0_synth, sp, ap, fs, frame_period=5.0)

    # Build voiced/unvoiced amplitude mask in time domain with 5ms ramps
    # at boundaries so consonant edges crossfade smoothly.
    samples_per_frame = int(round(fs * 5.0 / 1000.0))
    voiced_mask = np.repeat(voiced.astype(np.float64), samples_per_frame)
    if len(voiced_mask) < len(y):
        voiced_mask = np.pad(voiced_mask, (0, len(y) - len(voiced_mask)),
                             mode="edge")
    voiced_mask = voiced_mask[:len(y)]
    ramp = int(0.005 * fs)
    if ramp > 1:
        edges = np.diff(voiced_mask.astype(np.int8))
        for idx in np.where(edges == 1)[0]:
            for k in range(ramp):
                pos = idx + 1 + k
                if pos < len(voiced_mask):
                    voiced_mask[pos] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
        for idx in np.where(edges == -1)[0]:
            for k in range(ramp):
                pos = idx - k
                if pos >= 0:
                    voiced_mask[pos] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)

    # Composite: WORLD-synth at full level in voiced regions; original
    # at `unvoiced-gain` in unvoiced. unvoiced-gain=0.0 leaves only the
    # sung vowels, no breath/consonants.
    n = min(len(y), len(x), len(voiced_mask))
    y_out = (voiced_mask[:n] * y[:n] +
             (1.0 - voiced_mask[:n]) * x[:n] * args.unvoiced_gain)
    sf.write(args.out_wav, y_out.astype(np.float32), fs)

    voiced_pct = 100.0 * np.mean(voiced)
    print(f"  worldsing · {args.note} ({target_hz:.1f}Hz) · "
          f"ap×{args.ap_scale} · unv×{args.unvoiced_gain} · "
          f"{voiced_pct:.0f}% voiced · {n} samples")
    return 0

if __name__ == "__main__":
    sys.exit(main())
