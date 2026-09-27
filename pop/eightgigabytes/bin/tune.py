#!/usr/bin/env python
# tune.py — are the three voices on their written notes?
# For every rendered line (out/stems/<member>/manifest.json) lay the WORLD
# f0 track against the line's notes; each note's pitch = median voiced f0
# over its middle 60 %. Reports cents error per member, notes within 50
# cents, octave slips. Run after bin/render.mjs:
#   pop/.venv/bin/python pop/eightgigabytes/bin/tune.py
import json, math, sys, os, warnings
from pathlib import Path
import numpy as np, soundfile as sf
warnings.filterwarnings("ignore")
import pyworld as pw
LANE = Path(__file__).resolve().parent.parent
OUT = LANE / "out"
BPM = json.load(open(OUT / "timeline.json"))["bpm"]; beat = 60 / BPM
hz = lambda m: 440 * 2 ** ((m - 69) / 12)
report = {}
for m in ["neo", "blueberry", "frisbee"]:
    man = json.load(open(OUT / "stems" / m / "manifest.json"))
    rows = []; errs = []; slips = 0; within = 0; total = 0
    for ln in man["lines"]:
        if not ln.get("wav") or not os.path.exists(ln["wav"]): continue
        y, sr = sf.read(ln["wav"], dtype="float64")
        if y.ndim > 1: y = y.mean(1)
        f0, t = pw.harvest(y, sr, f0_floor=60, f0_ceil=1200, frame_period=5.0)
        # the wav starts spanOffset seconds after beat 0 of the member's score
        pos = 0.0; line_err = []
        for tok in ln["notes"].split(","):
            k, _, d = tok.partition(":"); d = float(d or 1)
            if k != "r":
                a = pos * beat - ln["spanOffset"]; b = a + d * beat
                lo, hi = a + 0.2 * (b - a), a + 0.8 * (b - a)
                sel = f0[(t >= lo) & (t <= hi) & (f0 > 0)]
                total += 1
                if len(sel):
                    c = 1200 * math.log2(np.median(sel) / hz(int(k)))
                    if abs(abs(c) - 1200) < 100: slips += 1
                    else:
                        errs.append(abs(c)); line_err.append(c)
                        if abs(c) <= 50: within += 1
            pos += d
        rows.append({"text": ln["lyrics"], "mean_abs_cents": round(float(np.mean(np.abs(line_err))), 1) if line_err else None, "n": len(line_err)})
    report[m] = {"notes": total, "measured": len(errs), "within_50c": within, "octave_slips": slips,
                 "mean_abs_cents": round(float(np.mean(errs)), 1) if errs else None, "lines": rows}
    print(f"{m:10s} notes {total:3d}  measured {len(errs):3d}  within 50c {within:3d}  slips {slips}  mean |cents| {report[m]['mean_abs_cents']}")
    worst = sorted([r for r in rows if r["mean_abs_cents"] is not None], key=lambda r: -r["mean_abs_cents"])[:2]
    for r in worst: print(f"           worst: {r['mean_abs_cents']:5.1f}c  {r['text']}")
json.dump(report, open(OUT / "tune.json", "w"), indent=1)
