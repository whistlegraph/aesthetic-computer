#!/usr/bin/env python3
"""align-paragraphs.py — phoneme-level forced alignment for the
diphone-bank carrier paragraphs.

Pipeline per carrier:
    1. Load mp3 → 16 kHz mono float
    2. Run torchaudio.pipelines.MMS_FA acoustic model → per-frame logits
    3. Tokenize the carrier transcript into characters (the MMS_FA vocab)
    4. CTC forced alignment via torchaudio.functional.forced_align
    5. Expand char-level spans to word-level (split by " ")
    6. For each word, run g2p_en → ARPABet sequence, distribute the
       word's time uniformly across phonemes
    7. Write aligned/<id>.json with {words: [...], phonemes: [...]}

Limitations (acknowledged):
    * Phoneme-time distribution within a word is uniform, not acoustic.
      Diphone extraction with ~50 ms slices + crossfade tolerates this.
    * g2p_en approximates OOV words via a neural seq2seq fallback. Not
      perfect for AC neologisms (e.g. "kidlisp" came out missing /P/).
      A manual override dict for known AC vocabulary is in this file.
    * MMS_FA was trained on broad multilingual data; English-only
      character set is used here. Punctuation is stripped.

Usage:
    python bin/align-paragraphs.py
    python bin/align-paragraphs.py --only p000,p001
"""
from __future__ import annotations

import argparse
import json
import re
import sys
import unicodedata
from pathlib import Path

import librosa                  # type: ignore
import numpy as np
import torch                    # type: ignore
import torchaudio               # type: ignore
import torchaudio.functional as F   # type: ignore
from g2p_en import G2p          # type: ignore

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent              # pop/voice/diphones/
RAW = ROOT / "raw"
ALIGNED = ROOT / "aligned"
ALIGNED.mkdir(exist_ok=True)

# AC neologism overrides — g2p_en's neural fallback is unreliable for
# these. Keys are lowercase whole words; values are ARPABet phonemes
# (no stress markers — alignment doesn't use them).
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


def normalize_text(s: str) -> str:
    """Lowercase, strip punctuation, collapse whitespace."""
    s = unicodedata.normalize("NFKD", s)
    s = s.lower()
    # Replace em-dash and other separators with space
    s = re.sub(r"[—–\-]", " ", s)
    # Drop everything that's not a-z, space, or apostrophe
    s = re.sub(r"[^a-z' ]", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def words_to_phonemes(words: list[str], g2p: G2p) -> list[list[str]]:
    """Per-word ARPABet phoneme list. Strip stress markers."""
    out = []
    for w in words:
        if w in AC_VOCAB:
            out.append(list(AC_VOCAB[w]))
            continue
        phs = g2p(w)
        # g2p output may include word-internal spaces if multi-word; we
        # called per-word so that shouldn't happen.
        cleaned = []
        for p in phs:
            if p == " ":
                continue
            # strip trailing digit (stress marker 0/1/2)
            if p and p[-1].isdigit():
                p = p[:-1]
            cleaned.append(p)
        out.append(cleaned)
    return out


def align_one(mp3_path: Path, transcript: str, bundle, model, dictionary,
              g2p: G2p) -> dict:
    """Forced-align one carrier."""
    SAMPLE_RATE = bundle.sample_rate
    y, _ = librosa.load(str(mp3_path), sr=SAMPLE_RATE, mono=True)
    waveform = torch.from_numpy(y).unsqueeze(0)

    norm_text = normalize_text(transcript)
    words = norm_text.split()
    if not words:
        return {"error": "empty_transcript"}

    # Tokenize transcript to model token ids. MMS_FA uses character
    # tokens with "*" as a "star" / blank-equivalent class for unknowns.
    # We map each character of each word to its index; unknowns silently
    # become the star token.
    star_id = dictionary.get("*", 0)
    transcript_ids: list[int] = []
    word_token_spans: list[tuple[int, int]] = []   # (start_idx, end_idx) in transcript_ids
    for w in words:
        s = len(transcript_ids)
        for ch in w:
            transcript_ids.append(dictionary.get(ch, star_id))
        e = len(transcript_ids)
        word_token_spans.append((s, e))

    targets = torch.tensor([transcript_ids], dtype=torch.int32)

    # Run model → log-probs
    with torch.inference_mode():
        emission, _ = model(waveform)
        log_probs = torch.log_softmax(emission, dim=-1)

    # CTC forced alignment
    aligned, scores = F.forced_align(
        log_probs, targets, blank=0,
    )
    # aligned: [B=1, T] of token indices (with repeats + blanks)
    # Convert to per-token spans by collapsing repeats and tracking
    # frame indices.
    aligned = aligned[0].tolist()
    spans: list[tuple[int, int]] = []  # (start_frame, end_frame) per output token
    cur_token = None
    cur_start = 0
    last_was_blank = True
    out_idx = 0
    for t, tok in enumerate(aligned):
        if tok == 0:
            # blank — closes any pending token span
            if cur_token is not None:
                spans.append((cur_start, t))
                cur_token = None
            last_was_blank = True
        else:
            if cur_token == tok and not last_was_blank:
                # continuation of same token
                pass
            else:
                # new token (or restart after blank)
                if cur_token is not None and cur_token != tok:
                    spans.append((cur_start, t))
                cur_token = tok
                cur_start = t
            last_was_blank = False
    if cur_token is not None:
        spans.append((cur_start, len(aligned)))

    # Compress repeated-token spans into one per transcript token.
    # CTC alignment outputs as many non-blank spans as the unrolled CTC
    # path; for a forced alignment to a target sequence of length L, we
    # expect L distinct spans (one per target token).
    if len(spans) != len(transcript_ids):
        # Fallback: distribute spans uniformly. Don't crash.
        T = log_probs.shape[1]
        per_token_frames = T / max(1, len(transcript_ids))
        spans = [(int(i * per_token_frames), int((i + 1) * per_token_frames))
                 for i in range(len(transcript_ids))]

    # Convert frame indices to seconds. MMS_FA is 20 ms per frame
    # (16 kHz × 320-sample stride).
    SECS_PER_FRAME = 320.0 / SAMPLE_RATE
    def fr2s(fr: int) -> float:
        return round(fr * SECS_PER_FRAME, 4)

    # Word-level timings
    word_records = []
    for i, (s, e) in enumerate(word_token_spans):
        if e <= s:
            continue
        f0 = spans[s][0]
        f1 = spans[e - 1][1]
        word_records.append({
            "text": words[i],
            "fromS": fr2s(f0),
            "toS": fr2s(f1),
        })

    # Phoneme-level: distribute each word's duration uniformly across
    # its g2p_en-derived ARPABet phonemes.
    word_phs = words_to_phonemes(words, g2p)
    phoneme_records = []
    for i, w in enumerate(word_records):
        phs = word_phs[i] if i < len(word_phs) else []
        if not phs:
            continue
        dur = w["toS"] - w["fromS"]
        slot = dur / len(phs)
        for k, ph in enumerate(phs):
            phoneme_records.append({
                "ph": ph,
                "fromS": round(w["fromS"] + k * slot, 4),
                "toS":   round(w["fromS"] + (k + 1) * slot, 4),
                "word":  w["text"],
                "wordIdx": i,
            })

    return {
        "audio_seconds": round(waveform.shape[-1] / SAMPLE_RATE, 3),
        "n_words": len(word_records),
        "n_phonemes": len(phoneme_records),
        "words": word_records,
        "phonemes": phoneme_records,
        "transcript_normalized": norm_text,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--only", default=None,
                        help="comma-separated carrier ids (e.g. p000,p001)")
    args = parser.parse_args()

    only = set(args.only.split(",")) if args.only else None

    bundle = torchaudio.pipelines.MMS_FA
    print(f"→ loading MMS_FA model (~1 GB on first run)…", flush=True)
    model = bundle.get_model()
    model.eval()
    dictionary = bundle.get_dict()
    print(f"  vocab size: {len(dictionary)}  · sample rate: {bundle.sample_rate}")

    g2p = G2p()

    txt_files = sorted(RAW.glob("p*.txt"))
    if not txt_files:
        print(f"✗ no carrier texts under {RAW}/p*.txt", file=sys.stderr)
        return 1

    successes = 0
    for txt in txt_files:
        cid = txt.stem
        if only and cid not in only:
            continue
        mp3 = txt.with_suffix(".mp3")
        if not mp3.exists():
            print(f"  - {cid}: missing mp3, skip")
            continue
        transcript = txt.read_text().strip()
        try:
            print(f"→ {cid}  ({len(transcript)} chars)", flush=True)
            result = align_one(mp3, transcript, bundle, model, dictionary, g2p)
            out = ALIGNED / f"{cid}.json"
            out.write_text(json.dumps({"id": cid, **result}, indent=2) + "\n")
            print(f"  ✓ {result['n_words']} words · {result['n_phonemes']} phonemes · {result['audio_seconds']}s")
            successes += 1
        except Exception as e:
            print(f"  ✗ {cid}: {type(e).__name__}: {e}", file=sys.stderr)
    print(f"\n✓ aligned {successes}/{len(txt_files)} carriers → {ALIGNED}")
    return 0 if successes else 1


if __name__ == "__main__":
    raise SystemExit(main())
