#!/usr/bin/env python3
"""syllsplit.py — split a word window into syllables where the SINGING
actually changes note.

The nuclei in a take's syllnote doc live in the raw timebase. Carrying
them into a stretched, hand-dictated window by proportion drifts, and a
drifted split puts the flap/PING boundary on the wrong side of the
flare — the octave collapses and the choir sings the hook flat. So
measure the stretched audio instead: pyin inside each window, smooth to
a semitone track, and cut at the largest pitch moves that are far enough
apart to be separate syllables.

  pop/.venv/bin/python syllsplit.py <stretched.wav> <req.json>
  req.json  [{i, fromMs, toMs, k}]        k = syllables wanted
  → stdout  {"<i>": [onsetMs, …]}         k onsets, first == fromMs

A window whose pitch never moves has no honest split to report; it is
omitted and the caller falls back.
"""
import json, sys
import numpy as np
import librosa

wav, reqf = sys.argv[1], sys.argv[2]
req = json.load(open(reqf))
y, sr = librosa.load(wav, sr=22050, mono=True)
HOP = 256
MIN_SYL_MS = 90          # a syllable shorter than this is a transient

f0, voiced, _ = librosa.pyin(y, sr=sr, fmin=70, fmax=700,
                             frame_length=2048, hop_length=HOP)
times = librosa.times_like(f0, sr=sr, hop_length=HOP) * 1000.0
semi = np.full(f0.shape, np.nan)
ok = voiced & np.isfinite(f0)
semi[ok] = 69 + 12 * np.log2(f0[ok] / 440.0)

def flare_run(a, b):
    """The highest sustained voiced run in [a,b] — the hook's flare. Returns
    (fromMs, toMs, semitones) for the best >=120ms run near the top of the
    region's pitch range, or None when the region never lifts."""
    sel = np.flatnonzero((times >= a) & (times <= b) & ok)
    if sel.size < 8:
        return None
    s, t = semi[sel], times[sel]
    top = np.percentile(s, 97)
    if top - np.median(s) < 2.0:            # the region never lifts
        return None
    hot = s >= top - 1.5
    best, run = None, None
    for i, h in enumerate(list(hot) + [False]):
        if h and run is None:
            run = i
        elif not h and run is not None:
            if t[i - 1] - t[run] >= 120:
                cand = (t[run], t[i - 1], float(np.median(s[run:i])))
                if best is None or cand[2] > best[2]:
                    best = cand
            run = None
    return best


out = {}
for w in req:
    # flare repair: a window that claims the flapping/PING word but does
    # not contain the line's high note is mislabeled — the flare landed in
    # the neighbour. Report where it really is; the caller re-cuts.
    if w.get("flareScanToMs"):
        fr = flare_run(w["fromMs"], w["flareScanToMs"])
        if fr:
            out["flare" + str(w["i"])] = [float(fr[0]), float(fr[1]), fr[2]]
    k = int(w["k"])
    if k < 2:
        continue
    sel = (times >= w["fromMs"]) & (times <= w["toMs"])
    idx = np.flatnonzero(sel & ok)
    if idx.size < 6:
        continue
    t = times[idx]
    s = semi[idx]
    # median-smooth so vibrato and octave-hopping pyin frames do not
    # read as note changes
    win = max(3, int(round(0.05 * sr / HOP)) | 1)
    pad = win // 2
    sm = np.median(np.stack([np.pad(s, (pad, pad), mode="edge")[i:i + s.size]
                             for i in range(win)]), axis=0)
    # candidate cuts: frame-to-frame movement in the smoothed track
    jump = np.abs(np.diff(sm))
    order = np.argsort(jump)[::-1]
    cuts = []
    for j in order:
        if jump[j] < 0.6:                     # < ~a semitone is not a note change
            break
        ms = t[j + 1]
        if ms - w["fromMs"] < MIN_SYL_MS or w["toMs"] - ms < MIN_SYL_MS:
            continue
        if any(abs(ms - c) < MIN_SYL_MS for c in cuts):
            continue
        cuts.append(ms)
        if len(cuts) == k - 1:
            break
    if len(cuts) < k - 1:
        continue
    out[str(w["i"])] = [float(w["fromMs"])] + [float(c) for c in sorted(cuts)]

json.dump(out, sys.stdout)
