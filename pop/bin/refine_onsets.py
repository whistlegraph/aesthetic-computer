#!/usr/bin/env python3
"""
refine_onsets.py — VALIDATOR (not modifier).

The score (.np file → events.json with `snappedStart` beat positions)
is the authoritative source of timing in this pipeline. The visual
storyboard rigidly follows the score; any drift between score and
audio is a *pitchsnap* problem to fix at the audio generation layer,
not papered over visually.

This script measures the gap between score-defined slide.start times
and detected audio onsets, prints a report of mismatches, and exits.
It does NOT modify the storyboard. Use the report to identify which
words pitchsnap rendered late/early — that's where the audio pipeline
needs slot-rigid trimming/padding.

Usage:
  refine_onsets.py <storyboard.json> <audio.mp3>
"""
import json
import sys
from pathlib import Path

import numpy as np
import librosa

SEARCH_WIN_S = 0.85   # how far from snappedStart to look for an onset
MIN_FIX_MS = 25       # don't override if onset is within this of original
                      # (avoids jitter from low-confidence detections)


def main():
    if len(sys.argv) < 3:
        print("usage: refine_onsets.py <storyboard.json> <audio>")
        sys.exit(1)
    storyboard_path = Path(sys.argv[1])
    audio_path = sys.argv[2]

    sb = json.loads(storyboard_path.read_text())
    slides = sb["slides"]
    audio_dur = float(sb["duration"])

    print(f"loading audio: {audio_path}")
    y, sr = librosa.load(audio_path, sr=22050)
    print(f"  {len(y)/sr:.2f}s @ {sr}Hz")

    print("detecting onsets…")
    onset_env = librosa.onset.onset_strength(y=y, sr=sr, hop_length=512)
    onsets = librosa.onset.onset_detect(
        onset_envelope=onset_env, sr=sr, hop_length=512,
        backtrack=True, units="time",
    )
    print(f"  {len(onsets)} onsets detected")

    # For each slide, find nearest onset within search window
    print()
    print(f"  {'i':>3}  {'text':<10}  {'score':>8}  {'audio':>8}  {'drift':>7}")
    drift_total = 0.0
    n_off = 0
    n_bad = 0
    for i, s in enumerate(slides):
        score_start = float(s["start"])
        win_lo = score_start - SEARCH_WIN_S
        win_hi = score_start + SEARCH_WIN_S
        nearby = onsets[(onsets >= win_lo) & (onsets <= win_hi)]
        if len(nearby) == 0:
            line = f"  {i:>3}  {s['text'][:10]:<10}  {score_start:>8.3f}  {'—':>8}  {'no onset':>7}"
        else:
            audio_start = float(nearby[np.argmin(np.abs(nearby - score_start))])
            drift_ms = (audio_start - score_start) * 1000
            tag = ""
            if abs(drift_ms) >= 200:
                tag = " ✗ BAD"
                n_bad += 1
            elif abs(drift_ms) >= MIN_FIX_MS:
                tag = " ⚠"
                n_off += 1
            drift_total += abs(drift_ms)
            line = f"  {i:>3}  {s['text'][:10]:<10}  {score_start:>8.3f}  {audio_start:>8.3f}  {drift_ms:>+6.0f}ms{tag}"
        print(line)

    print()
    print(f"  {n_bad} slides with >200ms audio drift (likely pitchsnap slot overflow)")
    print(f"  {n_off} slides with 25-200ms drift")
    print(f"  total drift: {drift_total:.0f}ms across {len(slides)} slides")
    print()
    print("  → storyboard NOT modified — score (snappedStart) remains authoritative.")
    print("  → fix audio drift in pitchsnap.mjs (slot-rigid trim/pad), not here.")


if __name__ == "__main__":
    main()
