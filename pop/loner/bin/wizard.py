# wizard.py — hand the chart to ChartWizard.
#
# @jeffrey: "can we maybe work on a drag and drop gui for this · so i can
# better adjust timing in realtime for these · like melodyne style · then
# we can recompute and play it back?"
#
# Everything the GUI draws has already been measured somewhere in this
# lane — the warped spans in vox4/.manifest.json, the beat slots and sung
# semitones in vox4/.chart.json, and audit.py's three columns (pitch,
# level, brightness) plus the NOTE/FRIC/PUFF events it splits them into.
# None of that should be re-derived in Swift, where it would drift from
# what halo3 actually renders. So this collects it into one file the app
# opens, and the app writes its edits back to chart-edits.json, which
# halo3 merges over the CHART literal.
#
#   pop/.venv/bin/python pop/loner/bin/wizard.py   # → vox4/.wizard.json

import json, os, sys
import numpy as np
import soundfile as sf

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
sys.path.insert(0, HERE)
from audit import columns, events, TONIC, FRAME_S   # one set of measurements

BPM = 122.0


def main():
    chart = json.load(open(os.path.join(LANE, "vox4", ".chart.json")))
    man = json.load(open(os.path.join(LANE, "vox4", ".manifest.json")))
    out = {"lane": os.path.basename(LANE), "bpm": BPM, "tonic": TONIC,
           "frame_s": FRAME_S, "phrases": {}}

    for name, ch in chart.items():
        # a phrase only reaches the GUI once halo3 has actually warped it —
        # a partial build (PHRASES=…) leaves the others without spans.
        if name not in man or "spans" not in man[name]:
            continue
        slice_name = man[name]["slice"]
        wav = os.path.join(LANE, "samples", f"{slice_name}.wav")
        if not os.path.exists(wav):
            continue
        x, fs = sf.read(wav, dtype="float64")
        if x.ndim > 1:
            x = x.mean(axis=1)

        f0, st, rms, hi, peak, m = columns(x, fs)
        db = 20.0 * np.log10(np.maximum(rms, 1e-9) / peak)
        spans = {t: (a, b) for (t, a, b) in man[name]["spans"]}

        # a unit is one word block: where it sits on the grid, and which
        # piece of her the block plays. The GUI drags exactly these two.
        units = []
        for n in ch["notes"]:
            a, b = spans.get(n["t"], (0.0, 0.0))
            units.append({"t": n["t"], "beat": n["beat"], "dur": n["dur"],
                          "st": n["st"], "src0": a, "src1": b})

        out["phrases"][name] = {
            "slice": slice_name, "wav": wav, "sr": int(fs),
            "leadIn": ch.get("leadIn", 0.0), "beats": ch["beats"],
            "units": units,
            "events": [{"a": a, "b": b, "kind": k,
                        "st": (None if np.isnan(s) else round(float(s), 2))}
                       for (a, b, s, k) in events(x, fs)],
            # the three columns, rounded to what a screen can show
            "frames": {
                "st": [None if np.isnan(v) else round(float(v), 2) for v in st],
                "db": [round(float(v), 1) for v in db],
                "hf": [round(float(v), 2) for v in hi],
            },
        }
        print(f"  {name:>20}  {len(units)} units · {len(out['phrases'][name]['events'])} events "
              f"· {m} frames")

    path = os.path.join(LANE, "vox4", ".wizard.json")
    json.dump(out, open(path, "w"))
    print(f"WROTE {path}  ({os.path.getsize(path) // 1024} KB, "
          f"{len(out['phrases'])} phrases)")


if __name__ == "__main__":
    main()
