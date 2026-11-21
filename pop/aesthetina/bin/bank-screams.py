#!/usr/bin/env python3
"""
bank-screams.py — aesthetina's two rising screams, re-made from real screams.

(@jeffrey 2026-11-20: "screams could be re-recorded with our more advanced knowledge now".) The rise at
bars 38 and 74 was a synthesized "ah" climbing a major scale. Here it is a wordless scream from the
frothyscreama chaos bank (jeffrey-pvc, eleven_v4, the chaos delivery), given frothyscreama's scream
aesthetivox in its wild mode: lengthened to the two bars, and its pitch replaced with the same rise —
the scale from the root to the octave, slow at first and twice as fast after — an octave over the old
one, where a scream lives. The tool's own verdict decides full, light or raw. Each is written at the
level the old rise had in the vox stem (out/stems-<arr>/vox.wav must still hold the old one).

  pop/.venv/bin/python pop/aesthetina/bin/bank-screams.py
  → out/screams/scream-38.wav, scream-74.wav, report.json
"""
import json, subprocess, sys
from pathlib import Path
import numpy as np, soundfile as sf
from scipy.signal import butter, sosfilt, resample_poly

HERE = Path(__file__).resolve().parent; OUT = HERE.parent / "out"; BANK = HERE.parents[1] / "frothyscreama/out/chaos"
META = json.load(open(HERE.parent / "arrangements/reveal.json")); BAR = 240 / META.get("bpm", 128)
PLAN = [(38, 52, "x39"), (74, 57, "x20")]   # bar, the old root, the bank take
kept = {r["id"]: r for r in json.load(open(BANK / "manifest.json"))["kept"]}
(OUT / "screams").mkdir(exist_ok=True)
dur = 2 * BAR; jobs = []
for bar, root, tid in PLAN:
    notes = [root + 12 + iv for iv in (0, 2, 4, 5, 7, 9, 11, 12)]
    jobs.append({"in": str(BANK / kept[tid]["file"]), "out": str(OUT / "screams" / f"scream-{bar}.f32"), "dur": dur, "step": 0.2 * dur, "transpose": 0,
                 "segs": [[0, notes]], "mode": "wild", "nat": 0.35, "halo": 0.2})
for j in jobs: Path(j["out"]).unlink(missing_ok=True); Path(j["out"] + ".json").unlink(missing_ok=True)
jf = OUT / "screams" / "jobs.json"; json.dump({"sr": 48000, "jobs": jobs}, open(jf, "w"))
subprocess.run([sys.executable, str(HERE.parents[1] / "frothyscreama/bin/aesthetivox-scream.py"), str(jf)], check=True)
db = lambda y: 10 * np.log10(np.mean(y ** 2) + 1e-20)
band = lambda y, fs: db(sosfilt(butter(4, [2000, 8000], "bandpass", fs=fs, output="sos"), y))
fs = 44100; ref = OUT / "screams" / "old-levels.json"   # the old rise, measured once from the vox stem that still held it
if not ref.exists():
    old, fs = sf.read(OUT / "stems-reveal/vox.wav", dtype="float32"); old = old.mean(1).astype(np.float64)
    json.dump({str(b): [float(db(old[int(b * BAR * fs): int((b + 2) * BAR * fs)])), float(band(old[int(b * BAR * fs): int((b + 2) * BAR * fs)], fs))] for b, _, _ in PLAN}, open(ref, "w"))
OLD = json.load(open(ref))
report = []
for (bar, root, tid), j in zip(PLAN, jobs):
    y = resample_poly(np.fromfile(j["out"], dtype=np.float32).astype(np.float64), 147, 160)   # 48 k → 44.1 k
    y = y[: int(dur * fs)]; fade = int(0.06 * fs); y[-fade:] *= np.linspace(1, 0, fade)        # it lets go on the downbeat
    ol, ob = OLD[str(bar)]
    y *= 10 ** ((ol - 1.0 - db(y)) / 20)                                                       # 1 dB under the old rise
    sf.write(OUT / "screams" / f"scream-{bar}.wav", np.stack([y, y], 1).astype(np.float32), fs, subtype="FLOAT")
    info = json.load(open(j["out"] + ".json"))
    report.append({"bar": bar, "take": tid, "text": kept[tid].get("text"), "choice": info["choice"], "seconds_before": kept[tid]["seconds"], "seconds_after": round(len(y) / fs, 2),
                   "old_level_db": round(ol, 1), "new_level_db": round(float(db(y)), 1), "old_2_8k_db": round(ob, 1), "new_2_8k_db": round(float(band(y, fs)), 1), "notes": j["segs"][0][1]})
    print(report[-1])
json.dump(report, open(OUT / "screams" / "report.json", "w"), indent=1)
