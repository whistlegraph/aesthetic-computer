#!/usr/bin/env python3
"""refine_words.py — snap whisper word boundaries to librosa onsets.

Whisper-cli on synthesized speech recognizes WORDS reliably but its
boundaries can drift 50-150 ms from the actual energy onset. librosa's
onset_strength + onset_detect operate directly on the audio and find
where each new sound actually fires (~30 ms precision).

Strategy:
  1. Load whisper words.json: [{text, fromMs, toMs}, ...]
  2. Run librosa onset detection on the same audio.
  3. For each whisper word, find the closest detected onset within
     ±SEARCH_WIN_MS of its fromMs. If found, replace fromMs with the
     onset; bump toMs to keep dur >= MIN_DUR_MS.
  4. Write refined words.json with same schema.

Usage:
  refine_words.py <audio.mp3> <words.json> <out.json> [--win-ms 200]
"""
import argparse
import json
import sys
from pathlib import Path

import numpy as np
import librosa

DEFAULT_WIN_MS = 200      # search window around whisper.fromMs
MIN_DUR_MS = 50           # never let a word collapse to zero length
SR = 22050                # downsample target — onset detection is fine here

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("audio")
    ap.add_argument("words")
    ap.add_argument("out")
    ap.add_argument("--win-ms", type=int, default=DEFAULT_WIN_MS)
    args = ap.parse_args()

    audio = Path(args.audio)
    words_path = Path(args.words)
    out_path = Path(args.out)
    if not audio.exists():
        print(f"✗ audio missing: {audio}", file=sys.stderr); return 1
    if not words_path.exists():
        print(f"✗ words missing: {words_path}", file=sys.stderr); return 1

    words = json.loads(words_path.read_text())
    if not words:
        out_path.write_text("[]")
        print("⚠ empty words.json"); return 0

    # Detect onsets. backtrack=True walks back to the nearest local energy
    # minimum so we land on the consonant attack, not the vowel peak.
    y, sr = librosa.load(str(audio), sr=SR)
    onset_env = librosa.onset.onset_strength(y=y, sr=sr, hop_length=256)
    onsets_s = librosa.onset.onset_detect(
        onset_envelope=onset_env, sr=sr, hop_length=256,
        backtrack=True, units="time",
    )
    onsets_ms = (onsets_s * 1000).astype(int)
    print(f"  librosa: {len(onsets_ms)} onsets across {len(y)/sr:.1f}s")

    # Sort once so we can binary-search per whisper word.
    onsets_ms.sort()

    refined = []
    n_snapped = 0
    n_drift_ms = []
    for w in words:
        from_ms = int(w["fromMs"])
        to_ms = int(w["toMs"])
        # Find nearest onset within window
        i = np.searchsorted(onsets_ms, from_ms)
        candidates = []
        if i > 0: candidates.append(onsets_ms[i - 1])
        if i < len(onsets_ms): candidates.append(onsets_ms[i])
        best = None
        best_diff = args.win_ms + 1
        for c in candidates:
            d = abs(int(c) - from_ms)
            if d < best_diff: best_diff = d; best = int(c)
        if best is not None:
            new_from = best
            new_to = max(to_ms, new_from + MIN_DUR_MS)
            n_drift_ms.append(new_from - from_ms)
            n_snapped += 1
            refined.append({"text": w["text"], "fromMs": new_from, "toMs": new_to})
        else:
            refined.append({"text": w["text"], "fromMs": from_ms, "toMs": to_ms})

    out_path.write_text(json.dumps(refined))
    if n_snapped:
        avg = sum(n_drift_ms) / len(n_drift_ms)
        amax = max(abs(d) for d in n_drift_ms)
        print(f"  snapped {n_snapped}/{len(words)} (avg drift {avg:+.0f} ms · max |drift| {amax} ms)")
    else:
        print(f"  no snaps — every word outside ±{args.win_ms}ms window")
    return 0

if __name__ == "__main__":
    sys.exit(main())
