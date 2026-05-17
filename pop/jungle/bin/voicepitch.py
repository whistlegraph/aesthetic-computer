#!/usr/bin/env python3
"""
voicepitch.py — analyze a (already track-aligned) vocal wav and emit a
quantized pitch timeline + a summary of her sung-voice quality.

WORLD harvest → median f0 per short segment → snap to a scale. The
timeline drives a soft "shadow" instrument in the render that octave-
doubles / counters her, so the mix is pitched to match her voice.

Usage:
  voicepitch.py <in.wav> <out.json> [--key A] [--scale minorpent]
                [--seg 0.28]
"""
import argparse
import json
import numpy as np
import soundfile as sf
import pyworld as pw

PC = {"c": 0, "d": 2, "e": 4, "f": 5, "g": 7, "a": 9, "b": 11}
SCALES = {
    "minorpent": [0, 3, 5, 7, 10], "majorpent": [0, 2, 4, 7, 9],
    "minor": [0, 2, 3, 5, 7, 8, 10], "major": [0, 2, 4, 5, 7, 9, 11],
    "chromatic": list(range(12)),
}
NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]


def key_pc(k):
    k = k.strip().lower(); pc = PC[k[0]]
    if len(k) > 1 and k[1] == "#": pc += 1
    if len(k) > 1 and k[1] == "b": pc -= 1
    return pc % 12


def midi_name(m):
    return f"{NAMES[int(round(m)) % 12]}{int(round(m)) // 12 - 1}"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inp"); ap.add_argument("out")
    ap.add_argument("--key", default="A")
    ap.add_argument("--scale", default="minorpent")
    ap.add_argument("--seg", type=float, default=0.28)
    a = ap.parse_args()

    x, fs = sf.read(a.inp)
    if x.ndim > 1: x = x.mean(axis=1)
    x = np.ascontiguousarray(x, dtype=np.float64)
    f0, t = pw.harvest(x, fs)
    f0 = pw.stonemask(x, f0, t, fs)

    v = f0[f0 > 0]
    if v.size < 5:
        json.dump({"medianHz": 0, "segments": []}, open(a.out, "w"))
        print("voicepitch: no voiced frames"); return
    vmid = 69.0 + 12.0 * np.log2(v / 440.0)
    med = float(np.median(vmid))
    lo, hi = float(np.percentile(vmid, 5)), float(np.percentile(vmid, 95))

    root = key_pc(a.key)
    allowed = sorted({(root + d) % 12 for d in SCALES.get(a.scale, SCALES["minorpent"])})

    def snap(m):
        b = int(np.floor(m)); best, bd = m, 99.0
        for c in range(b - 7, b + 8):
            if c % 12 in allowed:
                d = abs(c - m)
                if d < bd: bd, best = d, float(c)
        return best

    fp = (t[1] - t[0]) if len(t) > 1 else 0.005
    segs = []
    step = max(1, int(round(a.seg / fp)))
    for i in range(0, len(f0), step):
        chunk = f0[i:i + step]
        vc = chunk[chunk > 0]
        if vc.size < step * 0.4:        # mostly unvoiced → rest
            continue
        m = 69.0 + 12.0 * np.log2(np.median(vc) / 440.0)
        segs.append({"t": round(i * fp, 4),
                     "dur": round(len(chunk) * fp, 4),
                     "midi": round(snap(m), 3)})

    out = {
        "medianHz": round(float(np.median(v)), 2),
        "medianNote": midi_name(med),
        "rangeLow": midi_name(lo), "rangeHigh": midi_name(hi),
        "key": a.key, "scale": a.scale,
        "segments": segs,
    }
    json.dump(out, open(a.out, "w"))
    print(f"voicepitch: median {out['medianNote']} ({out['medianHz']} Hz) "
          f"range {out['rangeLow']}–{out['rangeHigh']} · {len(segs)} segs")


if __name__ == "__main__":
    main()
