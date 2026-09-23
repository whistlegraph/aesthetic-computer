#!/usr/bin/env python
# nuclei.py — find the vowel nuclei (and so the consonant spans) inside each
# word window of a spoken stem. Audio-side, with WORLD's f0 tracker: a
# nucleus is a run of voiced, energetic frames; a word with k syllables gets
# its k longest runs in time order; the cut between two nuclei falls on the
# quietest frame between them, so each consonant goes to the side it leans.
#
#   pop/.venv/bin/python spinging/lib/nuclei.py <stem.wav|mp3> <windows.json>
#   windows.json: [ { "fromMs": 0, "toMs": 320, "nsyl": 1 }, … ]
#   → stdout JSON: { fs, words: [ { syllables: [ { fromMs, toMs,
#       nucleusFromMs, nucleusToMs, measured } ] } ] }
#
# `measured` is false when fewer runs than syllables were found and the
# window was split evenly instead — the caller counts those.
import json
import subprocess
import sys
import tempfile
import numpy as np
import pyworld
import soundfile as sf

stem, win_path = sys.argv[1], sys.argv[2]
FP = 5.0  # ms

def load(path):
    if not path.lower().endswith(".wav"):
        tmp = tempfile.NamedTemporaryFile(suffix=".wav", delete=False).name
        subprocess.run(["ffmpeg", "-v", "error", "-y", "-i", path, "-ac", "1", "-ar", "22050", tmp], check=True)
        path = tmp
    x, fs = sf.read(path)
    if x.ndim > 1:
        x = x.mean(axis=1)
    return np.ascontiguousarray(x.astype(np.float64)), fs

x, fs = load(stem)
f0, t = pyworld.harvest(x, fs, f0_floor=50.0, f0_ceil=500.0, frame_period=FP)
# frame energy (log RMS over the analysis hop)
hop = int(fs * FP / 1000.0)
n = len(f0)
en = np.array([np.log(np.sqrt(np.mean(x[i * hop:(i + 1) * hop] ** 2) + 1e-12)) for i in range(n)])

def runs(a, b):
    a, b = max(0, a), min(n, b)
    if b <= a:
        return []
    seg = en[a:b]
    thr = seg.min() + 0.55 * (seg.max() - seg.min())
    out, run = [], None
    for i in range(a, b + 1):
        on = i < b and f0[i] > 0 and en[i] > thr
        if on and run is None:
            run = i
        elif not on and run is not None:
            out.append((run, i)); run = None
    return out

def trim_tail(a, b):
    """End the word where its energy ends: the last frame above a fifth of the
    word's dynamic range, plus 60 ms for a plosive release — never the pause
    that follows it. That pause, composited as 'coda', is the midnightttt."""
    a, b = max(0, a), min(n, b)
    if b - a < 4:
        return a, b
    seg = en[a:b]
    thr = seg.min() + 0.2 * (seg.max() - seg.min())
    last = a + int(np.max(np.where(seg > thr)[0])) if np.any(seg > thr) else b - 1
    return a, min(b, last + int(60 / FP) + 1)

words = []
for w in json.load(open(win_path)):
    a, b = int(round(w["fromMs"] / FP)), int(round(w["toMs"] / FP))
    a, b = trim_tail(a, b)
    k = max(1, int(w.get("nsyl", 1)))
    rs = runs(a, b)
    sylls = []
    if len(rs) >= k:
        keep = sorted(sorted(rs, key=lambda r: r[1] - r[0], reverse=True)[:k])
        bounds = [a]
        for i in range(1, k):
            lo, hi = keep[i - 1][1], keep[i][0]
            cut = lo + int(np.argmin(en[lo:hi])) if hi > lo else lo
            bounds.append(cut)
        bounds.append(b)
        for i in range(k):
            sylls.append({"fromMs": bounds[i] * FP, "toMs": bounds[i + 1] * FP,
                          "nucleusFromMs": keep[i][0] * FP, "nucleusToMs": keep[i][1] * FP, "measured": True})
    else:
        for i in range(k):
            sa, sb = a + (b - a) * i // k, a + (b - a) * (i + 1) // k
            r = runs(sa, sb)
            if r:
                best = max(r, key=lambda q: q[1] - q[0])
                sylls.append({"fromMs": sa * FP, "toMs": sb * FP, "nucleusFromMs": best[0] * FP, "nucleusToMs": best[1] * FP, "measured": len(rs) >= 1})
            else:
                sylls.append({"fromMs": sa * FP, "toMs": sb * FP, "nucleusFromMs": sa * FP, "nucleusToMs": sb * FP, "measured": False})
    words.append({"syllables": sylls})

print(json.dumps({"fs": fs, "words": words}))
