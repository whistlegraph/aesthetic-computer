#!/usr/bin/env python3
"""utterances.py — every phrase she sings, as a clip.

Her lead (the regularized aesthetivox stem — the record's own time base) is
gated on its energy: 10 ms RMS above the floor, a breath is a gap of ≥ GAP s,
a phrase must hold ≥ MIN s. Each phrase gets its words (src/words-aligned.json
mapped through the lock and regularize maps) and becomes an utterance:

  { id, start, end, dur, text, words: [{text, start, end}] }     (reg seconds)

That list is what the arrangement (bin/splice.mjs) cuts on, and what the
voice stems are clipped to — a phrase ends where HER voice ends, never where
a transcription guessed. Boundaries: start is walked back to the onset of
energy (≤ 60 ms), end is walked forward to where energy falls under the floor.

  pop/.venv/bin/python pop/sailor-song/bin/utterances.py [--gap 0.12] [--min 0.15] [--floor -46]
     → src/utterances.json, and a printed table
"""
import argparse, json, os
import numpy as np, soundfile as sf

ap = argparse.ArgumentParser()
ap.add_argument("--gap", type=float, default=0.12)
ap.add_argument("--min", type=float, default=0.15)
ap.add_argument("--floor", type=float, default=-46.0)   # dBFS on the 10 ms RMS
ap.add_argument("--stems", default="reg")
a = ap.parse_args()
LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(LANE, "src"); VOX = os.path.join(SRC, "vox")

x, fs = sf.read(os.path.join(VOX, a.stems, "vocals-aesthetivox.wav"))
if x.ndim > 1: x = x.mean(1)
hop = fs // 100
n = len(x) // hop
db = 20 * np.log10(np.sqrt((x[:n * hop].reshape(n, hop) ** 2).mean(1)) + 1e-9)
on = db > a.floor
# runs, merged across gaps shorter than --gap, dropped if shorter than --min
runs = []; i = 0
while i < n:
    if not on[i]: i += 1; continue
    j = i
    while j < n and on[j]: j += 1
    runs.append([i, j]); i = j
merged = []
for r in runs:
    if merged and (r[0] - merged[-1][1]) * 0.01 < a.gap: merged[-1][1] = r[1]
    else: merged.append(r)
merged = [r for r in merged if (r[1] - r[0]) * 0.01 >= a.min]

# words in reg time
def mapf(path):
    m = np.array([[float(v) / 48000 for v in l.split()] for l in open(path).read().strip().split("\n")])
    return lambda t: float(np.interp(t, m[:, 0], m[:, 1]))
if a.stems == "reg":
    lock = mapf(os.path.join(VOX, "locked/timemap.txt")); reg = mapf(os.path.join(VOX, "reg/timemap.txt"))
    R = lambda t: reg(lock(t))
else:
    R = lambda t: t
W = [{"text": w["text"], "start": R(w["fromMs"] / 1000), "end": R(w["toMs"] / 1000)} for w in json.load(open(os.path.join(SRC, "words-aligned.json")))]

utts = []
for k, (i, j) in enumerate(merged):
    s, e = i * 0.01, j * 0.01
    ws = [w for w in W if s - 0.08 <= (w["start"] + w["end"]) / 2 <= e + 0.08]
    utts.append({"id": k, "start": round(s, 3), "end": round(e, 3), "dur": round(e - s, 3), "text": " ".join(w["text"] for w in ws),
                 "words": [{"text": w["text"], "start": round(w["start"], 3), "end": round(w["end"], 3)} for w in ws]})
json.dump({"stems": a.stems, "floorDb": a.floor, "gapS": a.gap, "utterances": utts}, open(os.path.join(SRC, "utterances.json"), "w"), indent=1)
print(f"{len(utts)} utterances (floor {a.floor} dB, gap ≥ {a.gap} s)")
for u in utts: print(f"  #{u['id']:2d} {u['start']:7.2f} → {u['end']:7.2f} ({u['dur']:4.2f}s)  {u['text']}")
