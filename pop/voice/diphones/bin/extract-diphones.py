#!/usr/bin/env python3
"""extract-diphones.py — slice carrier audio into per-diphone WAVs.

Reads aligned/p*.json, walks each consecutive phoneme pair, and cuts a
~midpoint-to-midpoint slice from the source mp3 (~50–100 ms). For each
unique diphone we keep the *best* occurrence — heuristically, the one
whose duration is closest to the median across all occurrences (avoids
outlier-fast / outlier-stretched takes).

Output:
    bank/<a>_<b>.wav        — 16 kHz mono PCM, 50–100 ms each
    manifest.json           — phoneme pair → bank file + provenance

Usage:
    python bin/extract-diphones.py
    python bin/extract-diphones.py --crossfade-ms 8   # default 6 ms
"""
from __future__ import annotations

import argparse
import json
from collections import defaultdict
from pathlib import Path

import librosa                  # type: ignore
import numpy as np
import soundfile as sf          # type: ignore

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
RAW = ROOT / "raw"
ALIGNED = ROOT / "aligned"
BANK = ROOT / "bank"
BANK.mkdir(exist_ok=True)


def collect_occurrences():
    """Yield (a, b, source_id, mp3_path, t_start_s, t_end_s) for each
    consecutive phoneme pair in every aligned paragraph."""
    occurrences = defaultdict(list)
    for align_file in sorted(ALIGNED.glob("p*.json")):
        d = json.loads(align_file.read_text())
        cid = d["id"]
        mp3 = RAW / f"{cid}.mp3"
        phs = d["phonemes"]
        for i in range(len(phs) - 1):
            a = phs[i]
            b = phs[i + 1]
            # Diphone span: midpoint of a → midpoint of b
            a_mid = (a["fromS"] + a["toS"]) / 2.0
            b_mid = (b["fromS"] + b["toS"]) / 2.0
            occurrences[(a["ph"], b["ph"])].append({
                "source": cid, "mp3": str(mp3),
                "t0": a_mid, "t1": b_mid,
                "dur": b_mid - a_mid,
                "word_a": a["word"], "word_b": b["word"],
            })
    return occurrences


def pick_best(occs):
    """Among occurrences for one diphone, pick the one whose duration
    is closest to the median. Filters out same-word boundary collisions
    that span >250 ms (g2p_en uniform distribution can produce
    accidentally long phonemes inside long words)."""
    if len(occs) == 1:
        return occs[0]
    durs = sorted(o["dur"] for o in occs)
    med = durs[len(durs) // 2]
    # Penalize anything outside [0.020, 0.250] s
    def score(o):
        if o["dur"] < 0.02 or o["dur"] > 0.25:
            return 1e6
        return abs(o["dur"] - med)
    return min(occs, key=score)


def crossfade_taper(n: int, fade_n: int) -> np.ndarray:
    """Hann-like crossfade taper: 1 in the middle, ramped at both ends."""
    env = np.ones(n, dtype=np.float32)
    if fade_n > 0:
        ramp = 0.5 * (1 - np.cos(np.linspace(0, np.pi, fade_n, endpoint=False)))
        env[:fade_n] = ramp
        env[-fade_n:] = ramp[::-1]
    return env


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--crossfade-ms", type=float, default=6.0)
    parser.add_argument("--sample-rate", type=int, default=22050,
                        help="output sample rate")
    parser.add_argument("--max-diphones", type=int, default=None)
    args = parser.parse_args()

    sr = args.sample_rate
    fade_n = int(args.crossfade_ms / 1000.0 * sr)

    occurrences = collect_occurrences()
    print(f"→ {len(occurrences)} unique diphones across paragraphs")

    # Cache audio per source file (avoid repeated decode)
    audio_cache = {}
    def get_audio(mp3_path: str) -> np.ndarray:
        if mp3_path not in audio_cache:
            y, _ = librosa.load(mp3_path, sr=sr, mono=True)
            audio_cache[mp3_path] = y.astype(np.float32)
        return audio_cache[mp3_path]

    bank_entries = {}
    n_written = 0
    for (a, b), occs in sorted(occurrences.items()):
        if args.max_diphones is not None and n_written >= args.max_diphones:
            break
        best = pick_best(occs)
        y = get_audio(best["mp3"])
        i0 = max(0, int(best["t0"] * sr))
        i1 = min(y.shape[0], int(best["t1"] * sr))
        if i1 - i0 < int(0.020 * sr):
            # Too short — couldn't get a sensible slice
            continue
        slice_audio = y[i0:i1].copy()
        # Apply crossfade taper so concat at synth time blends smoothly
        env = crossfade_taper(slice_audio.shape[0], min(fade_n, slice_audio.shape[0] // 4))
        slice_audio *= env
        out_name = f"{a}_{b}.wav"
        sf.write(str(BANK / out_name), slice_audio, sr, subtype="PCM_16")
        bank_entries[f"{a}_{b}"] = {
            "a": a, "b": b,
            "wav": f"bank/{out_name}",
            "source_carrier": best["source"],
            "source_word_a": best["word_a"],
            "source_word_b": best["word_b"],
            "duration_s": round(best["dur"], 4),
            "occurrences_in_corpus": len(occs),
        }
        n_written += 1

    manifest = {
        "version": 1,
        "generated": "2026-05-04",
        "sample_rate": sr,
        "crossfade_ms": args.crossfade_ms,
        "n_diphones": n_written,
        "n_unique_seen": len(occurrences),
        "diphones": bank_entries,
    }
    (ROOT / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    print(f"✓ wrote {n_written} diphone WAVs to {BANK}")
    print(f"✓ manifest: {ROOT}/manifest.json")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
