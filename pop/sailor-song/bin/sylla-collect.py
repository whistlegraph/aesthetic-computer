#!/usr/bin/env python3
"""sylla-collect.py — SyllaWizard's saved boxes → src/word-bounds.json (take seconds).

Reads each line's saved syllables (src/sylla/sylls-NN.json: label, wi, fromMs, toMs,
ms into the clip) and adds the clip's offset back. A word's bounds are the span of
its syllables. Lines never opened keep the aligner's bounds.
  pop/.venv/bin/python pop/sailor-song/bin/sylla-collect.py
"""
import json, os
LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(LANE, "src"); SY = os.path.join(SRC, "sylla")
words = json.load(open(os.path.join(SRC, "words-aligned.json")))
spec = json.load(open(os.path.join(SY, "spec.json")))
out, lines = [], 0
for t in spec["takes"]:
    seed = json.load(open(os.path.join(SY, f"words-{t['id']}.json")))
    sp = os.path.join(SY, f"sylls-{t['id']}.json")
    if not os.path.exists(sp): continue
    lines += 1; off = seed["offsetSec"]
    by = {}
    for s in json.load(open(sp))["sylls"]: by.setdefault(s["wi"], []).append(s)
    for wi, ss in by.items():
        if wi >= len(seed["wordIndex"]): continue
        i = seed["wordIndex"][wi]
        out.append({"i": i, "text": words[i]["text"], "from": round(off + min(s["fromMs"] for s in ss) / 1000, 3), "to": round(off + max(s["toMs"] for s in ss) / 1000, 3)})
json.dump({"_": "hand word bounds, TAKE seconds, from SyllaWizard via bin/sylla-collect.py; applied last by bin/word-times.py", "words": sorted(out, key=lambda w: w["i"])},
          open(os.path.join(SRC, "word-bounds.json"), "w"), indent=1)
print(f"{len(out)} words from {lines} drawn lines → src/word-bounds.json")
