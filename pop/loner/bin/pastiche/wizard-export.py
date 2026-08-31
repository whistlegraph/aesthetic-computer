"""Export a Whistlegraph Wizard recording to the reel's stroke schema.

viz/wg-perform.json (chosen take per word) -> viz/wg-strokes.json:
Catmull-Rom-adjacent smoothing (moving average preserving endpoints)
plus even ~5px resampling makes the performed lines super smooth; the
chrome renderer then lerps along them per syllable, unchanged.
"""
import json
import os

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
LONER = os.path.dirname(os.path.dirname(HERE))

rec = json.load(open(f"{LONER}/viz/wg-perform.json"))
W, H = rec["canvasW"], rec["canvasH"]

strokes = []
for w in rec["words"]:
    if w.get("chosen", -1) < 0 or not w["takes"]:
        continue
    take = w["takes"][w["chosen"]]
    subs = []
    for seg in take["segments"]:
        P = np.array([[p["x"], p["y"]] for p in seg], float)
        if len(P) >= 7:
            P[3:-3] = sum(P[i:len(P) - 6 + i] for i in range(7)) / 7
        d = np.concatenate([[0],
                            np.cumsum(np.hypot(*np.diff(P, axis=0).T))]) \
            if len(P) > 1 else np.array([0.0])
        if d[-1] > 5:
            u = np.arange(0, d[-1] + 2.5, 5.0)
            P = np.stack([np.interp(u, d, P[:, 0]),
                          np.interp(u, d, P[:, 1])], 1)
        subs.append([[round(float(x), 1), round(float(y), 1)]
                     for x, y in P])
    if not subs:
        continue
    strokes.append({
        "name": w.get("mark") or w["word"], "word": w["word"],
        "v0": w["v0"], "v1": w["v1"], "sub": subs,
    })
    print(f'{w["word"]:10s} {len(subs)} line, '
          f'{sum(len(s) for s in subs):3d} pts')

json.dump({"w": W, "h": H, "strokes": strokes},
          open(f"{LONER}/viz/wg-strokes.json", "w"))
print(f"-> viz/wg-strokes.json ({len(strokes)} performed strokes)")
