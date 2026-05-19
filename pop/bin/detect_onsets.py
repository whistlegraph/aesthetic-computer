#!/usr/bin/env python3
"""detect_onsets.py — emit a flat array of librosa onsets for the
whole audio so downstream tools can pick syllable boundaries within
known word windows.

Output shape (JSON array):
  [{"ms": 60, "strength": 0.83}, {"ms": 320, "strength": 1.4}, ...]

`strength` is the onset_strength sample at the detection frame, so
consumers can prefer LOUDER onsets when they need to pick fewer than
detected (e.g. "this word should have 2 syllable boundaries, but I
detected 4 onsets — keep the strongest 2").

Usage:
  detect_onsets.py audio.mp3 out.json [--hop 256] [--sr 22050]
"""
import argparse
import json
import sys
from pathlib import Path

import numpy as np
import librosa

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("audio")
    ap.add_argument("out")
    ap.add_argument("--hop", type=int, default=256)
    ap.add_argument("--sr",  type=int, default=22050)
    args = ap.parse_args()

    p = Path(args.audio)
    if not p.exists():
        print(f"✗ audio missing: {p}", file=sys.stderr); return 1

    y, sr = librosa.load(str(p), sr=args.sr)
    onset_env = librosa.onset.onset_strength(y=y, sr=sr, hop_length=args.hop)
    onset_frames = librosa.onset.onset_detect(
        onset_envelope=onset_env, sr=sr, hop_length=args.hop,
        backtrack=True,                         # walk to local energy min
    )
    if len(onset_frames) == 0:
        Path(args.out).write_text("[]")
        print("⚠ no onsets detected"); return 0

    times_s = librosa.frames_to_time(onset_frames, sr=sr, hop_length=args.hop)
    strengths = onset_env[np.clip(onset_frames, 0, len(onset_env) - 1)]

    rows = [{"ms": int(round(t * 1000)), "strength": float(s)}
            for t, s in zip(times_s, strengths)]
    Path(args.out).write_text(json.dumps(rows))
    smin, smax, smean = float(strengths.min()), float(strengths.max()), float(strengths.mean())
    print(f"  detected {len(rows)} onsets · strength range [{smin:.2f}, {smax:.2f}] · mean {smean:.2f}")
    return 0

if __name__ == "__main__":
    sys.exit(main())
