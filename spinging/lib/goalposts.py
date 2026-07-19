#!/usr/bin/env python3
"""
goalposts.py — harvest goalpost vocal-shape templates from reference SUNG
vocals (round 3). "Analyze the proper vocal shape then conform to it."

Feeds spinging/cache/goalposts.json: per-feature percentile bands measured on
real singing (the repo's own acapellas — same singer as the TTS voice), plus
the raw per-note feature list so the engine can nearest-match templates by
interval size / note duration.

usage:
  pop/.venv/bin/python spinging/lib/goalposts.py OUT.json REF1.wav [REF2.wav …]
"""
import json
import os
import sys

import numpy as np
import soundfile as sf
import pyworld as pw

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from vocal_shapes import (FRAME_S, segment_notes, note_features, frame_rms_hf,
                          percentile_bands, hz_to_midi)


def analyze_ref(path):
    x, fs = sf.read(path, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)
    # normalize loudness a bit so energy features are comparable
    if np.abs(x).max() > 0:
        x = x / np.abs(x).max() * 0.7
    f0_raw, t = pw.harvest(x, fs, f0_floor=55.0, f0_ceil=600.0,
                           frame_period=FRAME_S * 1000.0)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    rms, hf = frame_rms_hf(x, fs, len(f0))
    # keep only frames with real vocal energy (acapellas have silences)
    floor = 0.05 * np.percentile(rms, 95)
    f0 = np.where(rms > floor, f0, 0.0)
    notes = segment_notes(f0, min_dur_s=0.12)
    feats = []
    for a, b in notes:
        f = note_features(f0, rms, hf, a, b)
        f["midi"] = round(float(np.median(hz_to_midi(f0[a:b][f0[a:b] > 0]))), 2)
        f["src"] = os.path.basename(path)
        feats.append(f)
    return feats


def main():
    out_path = sys.argv[1]
    refs = sys.argv[2:]
    all_feats = []
    per_ref = {}
    for r in refs:
        feats = analyze_ref(r)
        per_ref[os.path.basename(r)] = len(feats)
        all_feats.extend(feats)
        print(f"  {os.path.basename(r)}: {len(feats)} sung notes", file=sys.stderr)
    bands = percentile_bands(all_feats)
    doc = {
        "built": __import__("datetime").datetime.now().isoformat(timespec="seconds"),
        "references": refs, "notes_per_ref": per_ref,
        "n_notes": len(all_feats),
        "bands": bands,
        "notes": [{k: (round(v, 3) if isinstance(v, float) else v)
                   for k, v in f.items()} for f in all_feats],
    }
    with open(out_path, "w") as fh:
        json.dump(doc, fh, indent=1)
    print(json.dumps({"n_notes": len(all_feats),
                      "bands": {k: {"p10": v["p10"], "p50": v["p50"], "p90": v["p90"]}
                                for k, v in bands.items()}}, indent=1))


if __name__ == "__main__":
    main()
