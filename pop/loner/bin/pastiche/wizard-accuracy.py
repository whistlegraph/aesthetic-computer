"""Score the performed gestures against the footage's ground truth.

For each word, the reference is the ink Camille actually laid during
that word's window (appear-map stamps); the candidate is the chosen
wizard take. Three measures per word:

  trace   how close the performed line stays to the reference ink
          (mean nearest-ink distance over the performed points, px)
  cover   how much of the reference ink the performed line passes
          near (fraction within 14px)
  ends    start/end placement — performed endpoints vs the reference
          mark's earliest/latest ink centroids (px)

Prints the table and writes viz/wg-accuracy.json so retakes can chase
the weak words.
"""
import json
import os

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
LONER = os.path.dirname(os.path.dirname(HERE))

z = np.load(f"{LONER}/viz/wg-appear.npz")
appear = z["appear"]
mask = z["final_mask"].astype(bool)
clock = json.load(open(f"{LONER}/viz/wordclock.json"))[:20]
rec = json.load(open(f"{LONER}/viz/wg-perform.json"))

ys, xs = np.where(mask)
stamps = appear[ys, xs].astype(int)
tv = stamps / 12.0
bounds = np.array([e["v0"] for e in clock] + [clock[-1]["v1"]])
owner = np.searchsorted(bounds, tv, "right") - 1
owner[owner >= len(clock)] = len(clock) - 1
owner[stamps <= 27] = 0

report = []
for k, (e, w) in enumerate(zip(clock, rec["words"])):
    if w.get("chosen", -1) < 0 or not w["takes"]:
        continue
    m = owner == k
    ref = np.stack([xs[m], ys[m]], 1).astype(float)
    rt = tv[m]
    take = w["takes"][w["chosen"]]
    pts = np.array([[p["x"], p["y"]]
                    for seg in take["segments"] for p in seg], float)
    if not len(ref) or not len(pts):
        continue
    d = np.sqrt(((pts[:, None, :] - ref[None, :, :]) ** 2).sum(-1))
    trace = float(d.min(1).mean())
    cover = float((d.min(0) < 14).mean())
    early = ref[rt <= np.quantile(rt, 0.12)].mean(0)
    late = ref[rt >= np.quantile(rt, 0.88)].mean(0)
    ends = float(np.hypot(*(pts[0] - early)) + np.hypot(*(pts[-1] - late))) / 2
    score = max(0.0, 100 - trace * 2.2 - (1 - cover) * 30 - ends * 0.35)
    report.append({"word": e["word"], "mark": e["mark"],
                   "trace": round(trace, 1), "cover": round(cover, 2),
                   "ends": round(ends, 1), "score": round(score)})
    print(f'{e["word"]:10s} {e["mark"]:16s} trace {trace:5.1f}px  '
          f'cover {cover * 100:3.0f}%  ends {ends:5.1f}px  '
          f'score {score:3.0f}')

report.sort(key=lambda r: r["score"])
json.dump(report, open(f"{LONER}/viz/wg-accuracy.json", "w"), indent=1)
weak = [r["word"] for r in report[:5]]
print(f"\nweakest words to retake: {', '.join(weak)}")
print("-> viz/wg-accuracy.json")
