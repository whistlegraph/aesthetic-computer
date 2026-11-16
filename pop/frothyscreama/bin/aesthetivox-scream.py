#!/usr/bin/env python3
"""
aesthetivox-scream.py — a scream take, extended and aesthetivoxed.

The aesthetivox shape applied to a scream: WORLD analysis, the take split
into voiced and unvoiced runs, the consonant runs left at their own length
and every voiced run lengthened by one common ratio (frame interpolation,
never tiled) until the line is as long as asked, and the pitch REPLACED
with absolute notes. The notes are an arpeggio over the chord under the
scream, stepped hard with no glide — sixteenths, then thirty-seconds — which
is what makes it warble like a modem handshake while the formants stay his.

  aesthetivox-scream.py <jobs.json>
  {"sr": 48000, "jobs": [{"in": wav, "out": f32, "dur": seconds,
     "step": seconds, "transpose": semitones,
     "segs": [[start seconds, [midi, ...]], ...]}]}
"""
import json, os, sys, warnings
warnings.filterwarnings("ignore")
import numpy as np
import soundfile as sf
import pyworld as pw

FP = 5.0


def runs(mask):
    out, a = [], 0
    for i in range(1, len(mask) + 1):
        if i == len(mask) or mask[i] != mask[a]:
            out.append((a, i, bool(mask[a]))); a = i
    return out


def avox(j, sr):
    x, fs = sf.read(j["in"], dtype="float64")
    if x.ndim > 1:
        x = x.mean(1)
    f0, t = pw.harvest(x, fs, f0_floor=110, f0_ceil=800, frame_period=FP)
    f0 = pw.stonemask(x, f0, t, fs)
    sp = pw.cheaptrick(x, f0, t, fs, f0_floor=110)
    ap = pw.d4c(x, f0, t, fs)
    n = len(f0)
    hop = int(fs * FP / 1000)
    rms = np.array([np.sqrt(np.mean(x[i * hop:i * hop + hop * 2] ** 2) + 1e-12) for i in range(n)])
    loud = rms > rms.max() * 0.12
    voiced = f0 > 0
    forced = voiced.mean() < 0.35
    if forced:                       # a pure rasp: the loud frames carry the note instead
        voiced = loud
        ap = ap * 0.72
    else:                            # close holes under 30 ms, and keep loud rasp inside the line pitched
        for a, b, v in runs(voiced):
            if not v and (b - a) <= 6 and a > 0 and b < n:
                voiced[a:b] = True
        voiced |= loud & (np.convolve(voiced.astype(float), np.ones(41), "same") > 0)
    rs = runs(voiced)
    v_len = sum(b - a for a, b, v in rs if v)
    u_len = n - v_len
    want = int(j["dur"] * 1000 / FP)
    # The words keep close to their own pace (voiced runs at most 1.3x); the
    # rest of the asked length is one long hold on the last real vowel, so
    # the line is heard before it is held.
    ratio = min(1.3, max(1.0, (want - u_len) / max(1, v_len)))
    longs = [i for i, (a, b, v) in enumerate(rs) if v and b - a >= 16]
    last = longs[-1] if longs else max(range(len(rs)), key=lambda i: (rs[i][1] - rs[i][0]) * rs[i][2])
    body = sum(int(round((b - a) * ratio)) if v and b - a > 2 else b - a for a, b, v in rs)
    extra = max(0, want - body)
    src = []
    for i, (a, b, v) in enumerate(rs):
        if v and b - a > 2:
            n_out = int(round((b - a) * ratio))
            if i == last and extra:
                keep = min(8, (b - a) // 3)          # the vowel's own onset and release stay as spoken
                src.append(np.concatenate([np.linspace(a, a + keep, int(keep * ratio), endpoint=False),
                                           np.linspace(a + keep, b - 1 - keep, max(2, n_out + extra - 2 * int(keep * ratio))),
                                           np.linspace(b - 1 - keep, b - 1, int(keep * ratio))]))
            else:
                src.append(np.linspace(a, b - 1, n_out))
        else:
            src.append(np.arange(a, b, dtype=float))
    src = np.concatenate(src)
    lo = np.floor(src).astype(int); hi = np.minimum(lo + 1, n - 1); w = (src - lo)[:, None]
    sp2 = np.ascontiguousarray(sp[lo] * (1 - w) + sp[hi] * w)
    ap2 = np.ascontiguousarray(ap[lo] * (1 - w) + ap[hi] * w)
    v2 = voiced[np.round(src).astype(int)]
    k = np.arange(len(src)) * FP / 1000
    # the arpeggio: hard steps over the chord in force, sixteenths first, thirty-seconds after
    starts = np.array([s[0] for s in j["segs"]])
    step = j["step"]
    turn = j["dur"] * 0.4
    count = np.where(k < turn, np.floor(k / step), np.floor(turn / step) + np.floor((k - turn) / (step / 2)))
    f02 = np.zeros(len(src))
    for i in range(len(src)):
        notes = j["segs"][max(0, np.searchsorted(starts, k[i], side="right") - 1)][1]
        f02[i] = 440.0 * 2 ** ((notes[int(count[i]) % len(notes)] + j.get("transpose", 0) - 69) / 12)
    f02 = np.where(v2, f02, 0.0)
    y = pw.synthesize(f02, sp2, ap2, fs, FP)
    fade = int(0.06 * fs)
    y[-fade:] *= np.linspace(1, 0, fade)
    pk = np.abs(y).max()
    (y / pk if pk > 0 else y).astype(np.float32).tofile(j["out"])
    return dict(ratio=round(float(ratio), 2), seconds=round(len(y) / fs, 2), voiced=round(float(v2.mean()), 2), forced=bool(forced))


spec = json.load(open(sys.argv[1]))
for j in spec["jobs"]:
    if os.path.exists(j["out"]):
        continue
    try:
        print(os.path.basename(j["out"]), avox(j, spec["sr"]), flush=True)
    except Exception as e:
        print("FAILED", os.path.basename(j["out"]), e, flush=True)
