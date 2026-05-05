#!/usr/bin/env python3
"""synth-word.py — concat-synthesize a word from the diphone bank.

Pipeline:
    word → g2p_en → ARPABet phonemes (with #B start, #E end markers)
    walk consecutive pairs → load diphone WAVs from bank/
    overlap-add concat → output WAV

Usage:
    python bin/synth-word.py kidlisp --out ~/Desktop/jeff-says-kidlisp.wav
    python bin/synth-word.py "every piece a url"
"""
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

import librosa                  # type: ignore
import numpy as np
import soundfile as sf          # type: ignore
from g2p_en import G2p          # type: ignore

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
BANK = ROOT / "bank"
MANIFEST = json.loads((ROOT / "manifest.json").read_text())

# Keep parity with the extractor's AC overrides.
AC_VOCAB = {
    "kidlisp":      ["K", "IH", "D", "L", "IH", "S", "P"],
    "notepat":      ["N", "OW", "T", "P", "AE", "T"],
    "fedac":        ["F", "EH", "D", "AE", "K"],
    "plork":        ["P", "L", "AO", "R", "K"],
    "whistlegraph": ["W", "IH", "S", "AH", "L", "G", "R", "AE", "F"],
    "nopaint":      ["N", "OW", "P", "EY", "N", "T"],
    "sotce":        ["S", "AH", "T", "S", "IY"],
    "honeydo":      ["HH", "AH", "N", "IY", "D", "UW"],
    "lacma":        ["L", "AE", "K", "M", "AH"],
    "aesthetic":    ["EH", "S", "TH", "EH", "T", "IH", "K"],
    "fia":          ["F", "IY", "AH"],
    "menuband":     ["M", "EH", "N", "Y", "UW", "B", "AE", "N", "D"],
    "geckos":       ["G", "EH", "K", "OW", "Z"],
    "jamsocket":    ["JH", "AE", "M", "S", "AA", "K", "AH", "T"],
}


def text_to_phonemes(text: str, g2p: G2p) -> list[list[str]]:
    """Lowercase, split on whitespace, return per-word ARPABet lists."""
    text = re.sub(r"[^a-zA-Z' ]", " ", text.lower())
    text = re.sub(r"\s+", " ", text).strip()
    out = []
    for w in text.split():
        if w in AC_VOCAB:
            out.append(list(AC_VOCAB[w]))
            continue
        phs = g2p(w)
        cleaned = []
        for p in phs:
            if p == " ":
                continue
            if p and p[-1].isdigit():
                p = p[:-1]
            cleaned.append(p)
        out.append(cleaned)
    return out


def load_diphone(a: str, b: str, sr: int) -> np.ndarray | None:
    """Load a single diphone WAV. Returns None if missing from bank."""
    entry = MANIFEST["diphones"].get(f"{a}_{b}")
    if entry is None:
        return None
    path = ROOT / entry["wav"]
    y, _sr = sf.read(str(path), dtype="float32")
    assert _sr == sr, f"diphone {a}_{b} at {_sr} Hz, expected {sr}"
    return y


def concat_word(phs: list[str], sr: int, fade_ms: float = 6.0,
                inter_word_silence_ms: float = 60.0) -> tuple[np.ndarray, list[dict]]:
    """Walk consecutive (a, b) phoneme pairs, overlap-add concat."""
    fade_n = int(fade_ms / 1000.0 * sr)
    out = np.zeros(0, dtype=np.float32)
    used = []
    misses = []

    pairs = list(zip(["#B"] + phs, phs + ["#E"]))
    for a, b in pairs:
        slice_audio = load_diphone(a, b, sr)
        if slice_audio is None:
            # Try the reverse pair as a last-ditch fallback (rare —
            # CV vs VC orderings sometimes alias acoustically). If
            # still missing, insert a tiny silence and log the gap.
            misses.append(f"{a}_{b}")
            slice_audio = np.zeros(int(0.04 * sr), dtype=np.float32)
        else:
            used.append(f"{a}_{b}")
        if out.shape[0] == 0:
            out = slice_audio.copy()
        else:
            # Overlap-add: prepend last fade_n of `out` with fade_n of slice
            overlap = min(fade_n, out.shape[0], slice_audio.shape[0])
            if overlap > 0:
                tail = out[-overlap:]
                head = slice_audio[:overlap]
                out[-overlap:] = tail + head
                out = np.concatenate([out, slice_audio[overlap:]])
            else:
                out = np.concatenate([out, slice_audio])
    return out, [{"used": used, "misses": misses}]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("text", nargs="+", help="text to synthesize")
    parser.add_argument("--out", default=None,
                        help="output WAV path (default: ~/Desktop/jeff-says-<word>.wav)")
    parser.add_argument("--silence-ms", type=float, default=80.0,
                        help="inter-word silence")
    parser.add_argument("--rate", type=float, default=0.82,
                        help="speech rate; <1.0 = slower (preserves pitch). "
                             "Default 0.82 ≈ 22%% slower than the carrier audio.")
    parser.add_argument("--lead-ms", type=float, default=120.0,
                        help="leading silence padding")
    parser.add_argument("--trail-ms", type=float, default=180.0,
                        help="trailing silence padding")
    args = parser.parse_args()

    text = " ".join(args.text)
    g2p = G2p()
    word_phs = text_to_phonemes(text, g2p)
    if not word_phs:
        print("✗ empty text", file=sys.stderr)
        return 1

    sr = MANIFEST["sample_rate"]
    print(f"→ synth: {text!r}")
    chunks = []
    silence = np.zeros(int(args.silence_ms / 1000.0 * sr), dtype=np.float32)
    all_used = []
    all_misses = []
    for i, phs in enumerate(word_phs):
        print(f"  word {i+1}/{len(word_phs)}: {phs}")
        audio, log = concat_word(phs, sr)
        chunks.append(audio)
        all_used += log[0]["used"]
        all_misses += log[0]["misses"]
        if i < len(word_phs) - 1:
            chunks.append(silence)

    out = np.concatenate(chunks)

    # Time-stretch (preserves pitch via STFT). Slowing diphone-concat
    # to ~80% of carrier speed gives the words breathing room.
    if abs(args.rate - 1.0) > 1e-3:
        out = librosa.effects.time_stretch(out, rate=args.rate)

    # Add lead/trail silence padding so words don't slam in/out.
    lead = np.zeros(int(args.lead_ms / 1000.0 * sr), dtype=np.float32)
    trail = np.zeros(int(args.trail_ms / 1000.0 * sr), dtype=np.float32)
    out = np.concatenate([lead, out, trail])

    # Normalize to peak −3 dBFS
    peak = max(1e-9, float(np.max(np.abs(out))))
    out = out * (0.7079 / peak)

    out_path = Path(args.out) if args.out else Path.home() / "Desktop" / f"jeff-says-{re.sub(r'[^a-z0-9]+', '-', text.lower())}.wav"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    sf.write(str(out_path), out, sr, subtype="PCM_16")
    print(f"✓ {out_path}  ({out.shape[0]/sr:.2f}s · {len(all_used)} hits · {len(all_misses)} misses)")
    if all_misses:
        print(f"  misses: {', '.join(set(all_misses))}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
