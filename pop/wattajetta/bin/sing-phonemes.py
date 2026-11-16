#!/usr/bin/env python3
"""
sing-phonemes.py — sung notes from the jeffrey phoneme library.

Takes one spoken syllable clip (jeffrey-pvc, eleven_v4) and returns it as
one sung note: WORLD analysis, the pitch replaced with a flat target plus a
late shallow vibrato, the vowel nucleus lengthened in the frame domain by
interpolation (never tiled), the consonant onset and the first 50 ms of the
vowel left at their spoken length. Formant envelope untouched, so it stays
jeffrey at any pitch.

  sing-phonemes.py <jobs.json>
  jobs.json: {"library": dir, "cache": dir, "sr": 48000,
              "jobs": [{"key", "syl", "midi", "dur", "style"?}]}
Writes <cache>/<key>.f32 (mono float32) for every job not already there.
"""
import json, os, sys, warnings
warnings.filterwarnings("ignore")
import numpy as np
import librosa
import pyworld as pw

FP = 5.0  # ms per WORLD frame


def sing(path, midi, dur, sr_out):
    x, fs = librosa.load(path, sr=None, mono=True)
    x = x.astype(np.float64)
    f0, t = pw.harvest(x, fs, f0_floor=60, f0_ceil=350, frame_period=FP)
    f0 = pw.stonemask(x, f0, t, fs)
    sp = pw.cheaptrick(x, f0, t, fs)
    ap = pw.d4c(x, f0, t, fs)
    voiced = f0 > 0
    if not voiced.any():
        raise ValueError("no voiced frames")
    # longest voiced run = the nucleus
    best, a, n = (0, 0), None, len(f0)
    for i in range(n + 1):
        v = i < n and voiced[i]
        if v and a is None:
            a = i
        if not v and a is not None:
            if i - a > best[1] - best[0]:
                best = (a, i)
            a = None
    v0, v1 = best
    keep = min(10, (v1 - v0) // 3)           # 50 ms of natural vowel onset
    rel = min(12, (v1 - v0) // 3)            # 60 ms of natural release
    s0, s1 = v0 + keep, max(v0 + keep + 2, v1 - rel)
    total = max(int(dur * 1000 / FP), s0 + (n - s1) + 4)
    mid = total - s0 - (n - s1)
    # fractional source index for every output frame
    src = np.concatenate([np.arange(s0), np.linspace(s0, s1 - 1, mid), np.arange(s1, n)])
    lo = np.floor(src).astype(int)
    hi = np.minimum(lo + 1, n - 1)
    w = (src - lo)[:, None]
    sp2 = sp[lo] * (1 - w) + sp[hi] * w
    ap2 = ap[lo] * (1 - w) + ap[hi] * w
    voiced2 = voiced[np.round(src).astype(int)]
    hz = 440.0 * 2 ** ((midi - 69) / 12)
    k = np.arange(len(src)) * FP / 1000
    since = np.maximum(0, k - s0 * FP / 1000)
    vib = 1 + 0.006 * np.clip((since - 0.3) / 0.4, 0, 1) * np.sin(2 * np.pi * 5.2 * since)
    rng = np.random.default_rng(int(midi * 1000 + dur * 100))
    drift = np.convolve(rng.normal(0, 1, len(src) + 40), np.hanning(41) / np.hanning(41).sum(), "valid")
    scoop = 1 - 0.05 * np.exp(-np.maximum(0, k - v0 * FP / 1000) / 0.03)   # rises into the note
    f02 = np.where(voiced2, hz * vib * scoop * 2 ** (drift[: len(src)] * 0.004), 0.0)
    y = pw.synthesize(f02, np.ascontiguousarray(sp2), np.ascontiguousarray(ap2), fs, FP)
    fade = int(0.04 * fs)
    y[-fade:] *= np.linspace(1, 0, fade)
    y = librosa.resample(y, orig_sr=fs, target_sr=sr_out)
    pk = np.abs(y).max()
    return (y / pk if pk > 0 else y).astype(np.float32)


def main():
    spec = json.load(open(sys.argv[1]))
    os.makedirs(spec["cache"], exist_ok=True)
    made = 0
    for j in spec["jobs"]:
        out = os.path.join(spec["cache"], j["key"] + ".f32")
        if os.path.exists(out):
            continue
        folder = "syllables" + ("-" + j["style"] if j.get("style") else "")
        src = os.path.join(spec["library"], folder, j["syl"] + ".mp3")
        try:
            sing(src, j["midi"], j["dur"], spec["sr"]).tofile(out)
            made += 1
        except Exception as e:  # a failed note is reported, never silently skipped
            print(f"FAILED {j['key']}: {e}", flush=True)
    print(f"sung {made} new notes, {len(spec['jobs'])} in the score", flush=True)


main()
