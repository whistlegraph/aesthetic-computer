#!/usr/bin/env python3
"""
sing_word_world.py — melodyne-style RELATIVE pitch mapping for one sung word.

The companion to pop/bin/pitchsnap_world.py, but built around jeffrey's rule
for sung TTS: never absolute-snap his voice into a scale — DETECT the word's
actual fundamental in the take and shift it BY THE INTERVAL to the target
note(s). The word's own micro-contour (onset glides, declination, consonant
transitions) survives at a reduced depth (--beta), so it sings without losing
the baritone character.

  f0_new(t) = target_hz(t) * (f0(t) / median_f0) ** beta

WORLD chain (harvest → stonemask → cheaptrick → d4c → synthesize) with the
same unvoiced-composite trick as pitchsnap_world.py: consonants and sibilants
pass through from the original take, only voiced frames are resynthesized.

Usage:
  sing_word_world.py in.wav out.wav --midis "48,52" --starts "0,0.4"
    [--beta 0.25] [--f0-floor 70] [--xfade-ms 60]
    [--vibrato-hz 5] [--vibrato-cents 0] [--vibrato-onset-ms 250]

Prints one JSON line: {"detected_midi": …, "shift_st": …, "voiced_pct": …}
so the caller (sing-jingle.mjs) can log the per-word interval and flag
words whose pitch detection resisted.
"""
import argparse
import json
import sys
import numpy as np
import soundfile as sf
import pyworld as pw


def midi_to_hz(m):
    return 440.0 * (2.0 ** ((m - 69.0) / 12.0))


def hz_to_midi(hz):
    return 69.0 + 12.0 * np.log2(hz / 440.0)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("in_wav")
    p.add_argument("out_wav")
    p.add_argument("--midis", required=True, help="comma-separated target MIDI notes")
    p.add_argument("--starts", required=True,
                   help="comma-separated start times (sec, first 0) per note within the word")
    p.add_argument("--beta", type=float, default=0.25,
                   help="how much of the word's own pitch contour survives (0=flat, 1=all)")
    p.add_argument("--f0-floor", type=float, default=70.0)
    p.add_argument("--f0-ceil", type=float, default=500.0)
    p.add_argument("--xfade-ms", type=float, default=60.0)
    p.add_argument("--vibrato-hz", type=float, default=0.0)
    p.add_argument("--vibrato-cents", type=float, default=0.0)
    p.add_argument("--vibrato-onset-ms", type=float, default=250.0)
    args = p.parse_args()

    midis = np.array([float(m) for m in args.midis.split(",")], dtype=np.float64)
    starts = np.array([float(s) for s in args.starts.split(",")], dtype=np.float64)
    if len(midis) != len(starts):
        print(json.dumps({"error": "midis/starts length mismatch"}))
        return 1
    target_hzs = midi_to_hz(midis)

    x, fs = sf.read(args.in_wav, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)

    f0_raw, t = pw.harvest(x, fs, f0_floor=args.f0_floor, f0_ceil=args.f0_ceil,
                           frame_period=5.0)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    voiced = f0 > 0
    n_frames = len(t)
    frame_period_s = (t[1] - t[0]) if n_frames > 1 else 0.005

    # Not enough voiced material to pitch (a pure consonant burst) — pass the
    # original through untouched and let the caller report it.
    if voiced.sum() < 5:
        sf.write(args.out_wav, x.astype(np.float32), fs)
        print(json.dumps({"detected_midi": None, "shift_st": 0.0,
                          "voiced_pct": float(100.0 * voiced.mean())}))
        return 0

    med_hz = float(np.median(f0[voiced]))
    detected = float(hz_to_midi(np.array(med_hz)))
    target_mean = float(midis.mean())

    # Per-frame target: hold each note from its start, crossfade in log space.
    seg_starts = np.clip(np.round(starts / frame_period_s).astype(np.int64), 0, n_frames)
    xfade_frames = max(1, int(args.xfade_ms / (frame_period_s * 1000.0)))
    target_log = np.zeros(n_frames)
    for i in range(n_frames):
        seg = int(np.searchsorted(seg_starts[1:], i, side="right"))
        seg = min(seg, len(midis) - 1)
        center = np.log(target_hzs[seg])
        if seg + 1 < len(midis):
            dist = seg_starts[seg + 1] - i
            if dist < xfade_frames:
                u = 1.0 - (dist / xfade_frames)
                center = (1 - u) * center + u * np.log(target_hzs[seg + 1])
        target_log[i] = center
    target_curve = np.exp(target_log)

    if args.vibrato_hz > 0 and args.vibrato_cents > 0:
        time_sec = np.arange(n_frames) * frame_period_s
        onset = args.vibrato_onset_ms / 1000.0
        fade = np.clip((time_sec - onset) / max(0.05, onset), 0.0, 1.0)
        depth = (args.vibrato_cents / 100.0) / 12.0
        target_curve = target_curve * (2.0 ** (np.sin(2 * np.pi * args.vibrato_hz * time_sec) * depth * fade))

    # Relative map: move the word's median onto the target, keep beta of the
    # residual contour AROUND ITS OWN RAW MEDIAN. Using the raw median makes
    # the output land on target even when harvest tracked a harmonic — frames
    # and median double together, so the residual stays clean either way.
    # Interpolate f0 through unvoiced gaps first so WORLD sees a continuous
    # curve (no 0→target phase pops).
    log_src = np.log(np.maximum(f0, 1e-6))
    vi = np.where(voiced)[0]
    log_src_i = np.interp(np.arange(n_frames), vi, log_src[vi])
    resid = log_src_i - np.log(med_hz)            # his contour around center
    f0_synth = np.exp(np.log(target_curve) + args.beta * resid)

    fft_size = pw.get_cheaptrick_fft_size(fs, f0_floor=args.f0_floor)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft_size, f0_floor=args.f0_floor)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft_size)
    y = pw.synthesize(f0_synth, sp, ap, fs, frame_period=5.0)

    # Re-impose voiced/unvoiced: WORLD audio on voiced frames, the ORIGINAL
    # take on unvoiced (consonants stay crisp), 5ms cosine ramps at edges.
    spf = int(round(fs * 0.005))
    mask = np.repeat(voiced.astype(np.float64), spf)
    if len(mask) < len(y):
        mask = np.pad(mask, (0, len(y) - len(mask)), mode="edge")
    mask = mask[:len(y)]
    ramp = int(0.005 * fs)
    if ramp > 1:
        edges = np.diff(mask.astype(np.int8))
        for idx in np.where(edges == 1)[0]:
            for k in range(ramp):
                pos = idx + 1 + k
                if pos < len(mask):
                    mask[pos] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
        for idx in np.where(edges == -1)[0]:
            for k in range(ramp):
                pos = idx - k
                if pos >= 0:
                    mask[pos] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
    n = min(len(y), len(x), len(mask))
    out = mask[:n] * y[:n] + (1.0 - mask[:n]) * x[:n]
    sf.write(args.out_wav, out.astype(np.float32), fs)

    print(json.dumps({
        "detected_midi": round(detected, 2),
        "shift_st": round(target_mean - detected, 2),
        "voiced_pct": round(float(100.0 * voiced.mean()), 1),
    }))
    return 0


if __name__ == "__main__":
    sys.exit(main())
