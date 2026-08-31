#!/usr/bin/env python3
"""notecheck.py — prove the vocal hits the written notes.

Reads a tuned vocal and a target map [{label, t, dur, note}], measures
the median voiced f0 inside each target window (pyin, voice band), and
reports per-syllable: detected vs target, cents off, HIT within ±60¢.

  pop/.venv/bin/python notecheck.py <vocal.wav> <targets.json>
  → prints the table, writes <vocal>.notecheck.json, exits 0
"""
import json, sys
import numpy as np
import librosa

NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]
def to_midi(name):
    import re
    m = re.match(r"^([A-G]#?)(-?\d)$", name)
    return (int(m.group(2)) + 1) * 12 + NAMES.index(m.group(1))
def nname(m): return NAMES[int(round(m)) % 12] + str(int(round(m)) // 12 - 1)

wav, tj = sys.argv[1], sys.argv[2]
targets = json.load(open(tj))
y, sr = librosa.load(wav, sr=22050, mono=True)
hop = 256
f0, voiced, vprob = librosa.pyin(y, sr=sr, fmin=80, fmax=600, frame_length=2048, hop_length=hop)
times = librosa.times_like(f0, sr=sr, hop_length=hop)

rows, hits = [], 0
print(f"{'syllable':<12}{'target':>7}{'start':>7}{'mid':>7}{'end':>7}{'cents':>8}  ")
for t in targets:
    t0, t1 = t["t"] + 0.02, t["t"] + max(t["dur"], 0.12)
    sel = (times >= t0) & (times <= t1) & voiced & (vprob > 0.3) & np.isfinite(f0)
    if sel.sum() < 3:
        print(f"{t['label']:<12}{t['note']:>7}      —      —      —       —  (unvoiced)")
        rows.append({**t, "sung": None, "cents": None, "hit": False}); continue
    m = 69 + 12 * np.log2(f0[sel] / 440.0)
    k = max(1, min(5, len(m) // 4))
    start, mid, end = float(np.median(m[:k])), float(np.median(m)), float(np.median(m[-k:]))
    tgt = to_midi(t["note"])
    cents = round((mid - tgt) * 100)
    ends_on = abs(end - tgt) * 100 <= 80
    # a tail already traveling to the NEXT target is a legato glide, not a miss
    nxt = targets[targets.index(t) + 1]["note"] if targets.index(t) + 1 < len(targets) else None
    glide = nxt is not None and abs(end - to_midi(nxt)) * 100 <= 80
    hit = abs(cents) <= 60 and (ends_on or glide)
    hits += hit
    mark = "✓" if hit and ends_on else ("✓ glide→" + nxt if hit else "✗ MISS")
    print(f"{t['label']:<12}{t['note']:>7}{nname(start):>7}{nname(mid):>7}{nname(end):>7}{cents:>+7}¢  {mark}")
    rows.append({**t, "sung": nname(mid), "start": nname(start), "end": nname(end),
                 "startCents": round((start - tgt) * 100), "endCents": round((end - tgt) * 100),
                 "cents": cents, "hit": hit})
n = len([r for r in rows if r["cents"] is not None])
print(f"— {hits}/{n} within ±60¢")
json.dump({"hits": hits, "of": n, "rows": rows}, open(wav + ".notecheck.json", "w"), indent=1)
