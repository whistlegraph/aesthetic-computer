#!/usr/bin/env python3
"""notealign.py — align a KNOWN sung melody to a recording, and read the
syllable boundaries off the alignment.

Whisper cannot judge these takes: it guesses words, and sung melisma
defeats it (it has snapped "guys" onto "just"'s audio and shrunk "room"
to 15ms). But we are not missing the words — we know exactly what is
sung and on which notes. What is unknown is WHERE each syllable sits.
That makes this a forced alignment, and for singing the honest feature
is pitch: DTW the measured f0 track against the written note sequence,
monotonically, and every syllable gets a contiguous span by
construction. A window can then never sit on a note the singer did not
sing there — which is the failure that flattens the flare and makes
cos→tume read as a rise.

  pop/.venv/bin/python notealign.py <wav> <notes.json> [--offset auto|N]
  notes.json  [{label, note}]  in sung order (e.g. C4, C5 …)
  → stdout    [{label, note, fromMs, toMs}]

The written octave need not match the singer's: the transposition is
searched, which is how a hook written at C4 aligns to a take sung at C3.
"""
import json, sys
import numpy as np
import librosa

NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]


def to_midi(name):
    import re
    m = re.match(r"^([A-G]#?)(-?\d)$", name)
    return (int(m.group(2)) + 1) * 12 + NAMES.index(m.group(1))


wav, notesf = sys.argv[1], sys.argv[2]
notes = json.load(open(notesf))
targets = np.array([to_midi(n["note"]) for n in notes], dtype=float)
K = len(targets)

y, sr = librosa.load(wav, sr=22050, mono=True)
HOP = 256
f0, voiced, _ = librosa.pyin(y, sr=sr, fmin=70, fmax=700,
                             frame_length=2048, hop_length=HOP)
times = librosa.times_like(f0, sr=sr, hop_length=HOP) * 1000.0
ok = voiced & np.isfinite(f0)
semi = np.full(f0.shape, np.nan)
semi[ok] = 69 + 12 * np.log2(f0[ok] / 440.0)

# trim to the sung span so leading/trailing silence cannot absorb notes
vidx = np.flatnonzero(ok)
if vidx.size < K * 2:
    print("[]")
    sys.exit(0)
lo, hi = vidx[0], vidx[-1]
sl = slice(lo, hi + 1)
s, t, v = semi[sl], times[sl], ok[sl]
T = s.size

UNVOICED = 0.9      # a rest costs a little, so notes are not parked in silence
CAP = 5.0           # a wildly wrong frame should not dominate the path
MIN_FR = max(2, int(round(0.06 * sr / HOP)))   # 60ms floor per syllable


def align(offset):
    tg = targets + offset
    cost = np.empty((T, K))
    d = np.abs(s[:, None] - tg[None, :])
    np.minimum(d, CAP, out=d)
    cost[:] = d
    cost[~v, :] = UNVOICED
    D = np.full((T, K), np.inf)
    P = np.zeros((T, K), dtype=np.int8)
    D[0, 0] = cost[0, 0]
    for i in range(1, T):
        stay = D[i - 1]
        adv = np.concatenate(([np.inf], D[i - 1, :-1]))
        take_adv = adv < stay
        D[i] = np.where(take_adv, adv, stay) + cost[i]
        P[i] = take_adv
    return D[T - 1, K - 1], D, P


best = None
for off in range(-24, 25):
    total, D, P = align(off)
    if best is None or total < best[0]:
        best = (total, off, D, P)
_, off, D, P = best

# backtrack
k = K - 1
bounds = [T]
for i in range(T - 1, 0, -1):
    if P[i, k]:
        bounds.append(i)
        k -= 1
        if k < 0:
            break
bounds.append(0)
bounds = sorted(set(bounds))
while len(bounds) < K + 1:
    bounds.append(T)
bounds = bounds[:K + 1]

# Pitch cannot separate syllables written on the SAME note — but, ter,
# fly and flap are four C4s in a row, so the path is free to park three
# of them in slivers and let the fourth eat four seconds. Within each run
# of equal targets there is no pitch evidence, so fall back to the
# evidence that does exist there: onsets. Re-cut each run at its
# strongest attacks.
onsets = librosa.onset.onset_strength(y=y, sr=sr, hop_length=HOP)[sl]
i = 0
while i < K:
    j = i
    while j + 1 < K and targets[j + 1] == targets[i]:
        j += 1
    n = j - i + 1
    if n > 1:
        a, b = bounds[i], bounds[j + 1]
        if b - a >= n * MIN_FR:
            seg = onsets[a:b].copy()
            picks = []
            for _ in range(n - 1):
                order = np.argsort(seg)[::-1]
                for p in order:
                    ms = a + int(p)
                    if ms - a < MIN_FR or b - ms < MIN_FR:
                        continue
                    if any(abs(ms - q) < MIN_FR for q in picks):
                        continue
                    picks.append(ms)
                    seg[max(0, int(p) - MIN_FR):int(p) + MIN_FR] = -1
                    break
                else:
                    break
            if len(picks) == n - 1:
                for m, ms in enumerate(sorted(picks)):
                    bounds[i + 1 + m] = ms
    i = j + 1

# enforce a minimum syllable length so a note cannot collapse to nothing
for i in range(1, K):
    if bounds[i] - bounds[i - 1] < MIN_FR:
        bounds[i] = min(bounds[i - 1] + MIN_FR, T)
for i in range(K - 1, 0, -1):
    if bounds[i + 1] - bounds[i] < MIN_FR:
        bounds[i] = max(bounds[i + 1] - MIN_FR, 0)

out = []
for i, n in enumerate(notes):
    a, b = bounds[i], min(bounds[i + 1], T - 1)
    seg = s[a:b + 1][v[a:b + 1]]
    out.append({"label": n["label"], "note": n["note"],
                "fromMs": round(float(t[a]), 1), "toMs": round(float(t[b]), 1),
                "sungMidi": round(float(np.median(seg)), 2) if seg.size else None})
json.dump({"offset": int(off), "syllables": out}, sys.stdout)
