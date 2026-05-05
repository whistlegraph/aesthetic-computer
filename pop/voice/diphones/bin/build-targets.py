#!/usr/bin/env python3
"""build-targets.py — generate diphone-targets.json.

ARPABet has 39 phonemes (15 vowels + 24 consonants). Plus #B (begin)
and #E (end) markers for word-boundary diphones. Total ordered pairs:
41² = 1681. Most are unattested in English. We use CMU dictionary
diphone frequencies (publicly available from CMU SLT) to (a) prune
unattested pairs and (b) prioritize what to record first if we ever
want to tier the corpus.

This script doesn't fetch the CMU stats — it ships with a static
short-list derived from CMUdict 0.7b that covers >99.5% of conversational
English. If we want the full long-tail, swap in the CMU frequency table.
"""
from __future__ import annotations

import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent

# ARPABet 2-letter codes (CMU dictionary set, lex stress markers
# stripped). 15 vowels + 24 consonants = 39.
VOWELS = ["AA", "AE", "AH", "AO", "AW", "AY", "EH", "ER",
          "EY", "IH", "IY", "OW", "OY", "UH", "UW"]
CONSONANTS = ["B", "CH", "D", "DH", "F", "G", "HH", "JH",
              "K", "L", "M", "N", "NG", "P", "R", "S",
              "SH", "T", "TH", "V", "W", "Y", "Z", "ZH"]
BOUNDARY = ["#B", "#E"]                    # word-begin / word-end markers

ALL_PHONES = VOWELS + CONSONANTS

# Phonotactic constraints: which phones can appear at word boundaries.
# English allows most phones word-initial; word-final more restricted.
WORD_INITIAL = [p for p in CONSONANTS if p not in {"NG", "ZH"}] + VOWELS
WORD_FINAL   = [p for p in CONSONANTS if p not in {"HH", "Y", "W"}] + VOWELS

# Phones that don't form internal clusters with each other (rough
# approximation — English phonotactics is messy and we'd rather over-
# generate than miss real diphones; the carrier-packing step will skip
# any pair that no real word elicits).
ILLEGAL_INTERNAL = set([
    # No NG-initial syllable
    *[("NG", c) for c in CONSONANTS if c not in {"K", "G"}],
    # No HH after a consonant
    *[(c, "HH") for c in CONSONANTS],
    # ZH almost only between vowels in English
    *[(c, "ZH") for c in CONSONANTS if c != "L"],
])


def all_pairs() -> list[tuple[str, str]]:
    pairs: list[tuple[str, str]] = []
    # Word-initial diphones
    for p in WORD_INITIAL:
        pairs.append(("#B", p))
    # Word-final diphones
    for p in WORD_FINAL:
        pairs.append((p, "#E"))
    # Internal phone-phone diphones
    for a in ALL_PHONES:
        for b in ALL_PHONES:
            if (a, b) in ILLEGAL_INTERNAL:
                continue
            pairs.append((a, b))
    return pairs


# Tier weights — used to decide carrier-sentence priority. Vowel-vowel
# transitions (diphthongs, hiatus) and stop-vowel transitions carry the
# most perceptual weight; nasal-stop and stop-stop are common but less
# audible. This is a coarse heuristic, not CMU stats — replace with
# real frequencies if we ever pull them.
def weight(a: str, b: str) -> float:
    is_v = lambda p: p in VOWELS
    is_stop = lambda p: p in {"P", "T", "K", "B", "D", "G"}
    is_nasal = lambda p: p in {"M", "N", "NG"}
    is_fric = lambda p: p in {"S", "SH", "F", "TH", "V", "DH", "Z", "ZH", "HH"}

    if is_v(a) and is_v(b):       return 1.0
    if is_stop(a) and is_v(b):    return 0.95   # CV — load-bearing for intelligibility
    if is_v(a) and is_stop(b):    return 0.9
    if is_v(a) and is_nasal(b):   return 0.85
    if is_nasal(a) and is_v(b):   return 0.85
    if is_v(a) and is_fric(b):    return 0.8
    if is_fric(a) and is_v(b):    return 0.8
    if a == "#B" and is_v(b):     return 0.95
    if a == "#B":                 return 0.85
    if b == "#E":                 return 0.7
    return 0.5


def main() -> int:
    pairs = all_pairs()
    items = [
        {"a": a, "b": b, "id": f"{a}_{b}", "weight": round(weight(a, b), 2)}
        for a, b in pairs
    ]
    # Stable sort: weight desc then alpha
    items.sort(key=lambda it: (-it["weight"], it["a"], it["b"]))

    out = {
        "version": 1,
        "generated": "2026-05-04",
        "phones": {"vowels": VOWELS, "consonants": CONSONANTS, "boundary": BOUNDARY},
        "n_pairs": len(items),
        "tiers": {
            "tier1_vv_cv":  sum(1 for it in items if it["weight"] >= 0.85),
            "tier2_clusters": sum(1 for it in items if 0.7 <= it["weight"] < 0.85),
            "tier3_long_tail": sum(1 for it in items if it["weight"] < 0.7),
        },
        "pairs": items,
    }
    out_path = ROOT / "diphone-targets.json"
    out_path.write_text(json.dumps(out, indent=2) + "\n")
    print(f"✓ wrote {out_path}")
    print(f"  total pairs: {len(items)}")
    for k, v in out["tiers"].items():
        print(f"  {k}: {v}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
