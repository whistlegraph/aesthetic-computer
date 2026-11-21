#!/usr/bin/env python3
"""
wild-probe.py — the lead lines in the chaos delivery, measured against the
takes in use.

For every scream line: the take the renderer uses now and each candidate in
scream.mjs WILD, measured the way the chaos bank is (bin/chaos-probe.py:
roughness, how much has no pitch, how unsteady the pitch is, a chaos score)
and read back by whisper.cpp against the lyric (the share of the line's
words heard, in order). Prints the table; picks nothing — a winner is
written into scream.mjs PICKS by hand.

  pop/.venv/bin/python pop/frothyscreama/bin/wild-probe.py
"""
import difflib, json, os, re, subprocess, sys, warnings
warnings.filterwarnings("ignore")
from pathlib import Path

HERE = Path(__file__).resolve().parent
# chaos-probe.py's measures, taken as written (the file runs its table at import, so only its head is used)
_src = (HERE / "chaos-probe.py").read_text()
exec(_src[:_src.index("takes = json.loads(")])
CACHE = HERE.parent / "out" / "takes" / "wild-measure.json"
norm = lambda s: [re.sub(r"(.)\1+", r"\1", w) for w in re.sub(r"\[[^\]]*\]|[^a-z ]", "", s.lower().replace("-", " ")).split()]


def recall(want, got):
    a, b = norm(want), norm(got)
    return sum(m.size for m in difflib.SequenceMatcher(None, a, b).get_matching_blocks()) / max(1, len(a))


cache = json.load(open(CACHE)) if CACHE.exists() else {}
lines = json.loads(subprocess.run(["node", str(HERE / "scream.mjs"), "--list"], capture_output=True, text=True, check=True).stdout)
for L in lines:
    rows = [("in use", L["inUse"]["path"], L["inUse"]["sent"])] + [(f"wild {w['n']}", w["path"], w["sent"]) for w in L["wild"]]
    for label, path, sent in rows:
        if not os.path.exists(path): continue
        k = os.path.basename(path)
        if k not in cache:
            cache[k] = measure(path); json.dump(cache, open(CACHE, "w"), indent=1)
        m = cache[k]
        print(f"{L['id']:4s} {label:7s} chaos {chaos(m):5.1f} flat {m['flatness']:.4f} unp {m['unpitched']:.2f} f0 {m['f0_hz']:4.0f} ±{m['f0_spread_st']:.1f}st {m['seconds']:4.1f}s recall {recall(L['text'], m['heard']):.2f}  {sent!r} -> {m['heard'][:60]!r}")
    print()
