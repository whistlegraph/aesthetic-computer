#!/usr/bin/env python3
"""melody-stats.py — the statistical melody of a song, across every
performance in a corpus.

Aligns each take's whisper words to a template lyric (fuzzy local
alignment, so count-ins and banter don't matter), maps each template
word to its sung syllable nuclei (from syllnote.py — plosive-safe),
normalizes pitch per take (relative to that take's weighted-median
sung pitch) and rhythm per phrase (inter-onset ratios), then reports
per-syllable distributions: the melody skeleton the performances agree
on, with spread.

  pop/.venv/bin/python melody-stats.py downloads/imab.corpus.json \
      --template "i'm a butterfly, flapping for you guys, just a costume, i put on, in my room"

Commas in the template mark phrase boundaries. Writes
downloads/<slug>.melody-stats.json and prints the skeleton table.
"""
import json, os, re, sys
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
DL = os.path.join(HERE, "downloads")
NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]

SYLLS = {"i'm":1,"a":1,"butterfly":3,"flapping":2,"flopping":2,"for":1,"you":1,
         "guys":1,"it's":1,"just":1,"costume":2,"i":1,"put":1,"on":1,"in":1,
         "my":1,"room":1}
def syll_count(w):
    if w in SYLLS: return SYLLS[w]
    groups = len(re.findall(r"[aeiouy]+", w))
    return max(1, groups)

def norm(w): return re.sub(r"[^a-z']", "", w.lower())
def fuzzy(a, b):
    if a == b: return True
    if len(a) > 3 and len(b) > 3 and (a.startswith(b[:4]) or b.startswith(a[:4])): return True
    return False

def align(template, words):
    """Smith-Waterman over words; returns take-word index per template word (or None)."""
    T, W = len(template), len(words)
    H = np.zeros((T + 1, W + 1)); ptr = np.zeros((T + 1, W + 1), dtype=int)
    for i in range(1, T + 1):
        for j in range(1, W + 1):
            m = H[i-1][j-1] + (2 if fuzzy(template[i-1], words[j-1]) else -1)
            best = max(0, m, H[i-1][j] - 1, H[i][j-1] - 1)
            H[i][j] = best
            ptr[i][j] = 1 if best == m and best > 0 else (2 if best == H[i-1][j] - 1 else (3 if best == H[i][j-1] - 1 else 0))
    i, j = np.unravel_index(np.argmax(H), H.shape)
    out = [None] * T
    while i > 0 and j > 0 and H[i][j] > 0:
        if ptr[i][j] == 1:
            if fuzzy(template[i-1], words[j-1]): out[i-1] = j - 1
            i, j = i - 1, j - 1
        elif ptr[i][j] == 2: i -= 1
        else: j -= 1
    return out, float(H.max())

def main():
    corpus_path = sys.argv[1]
    ti = sys.argv.index("--template"); template_raw = sys.argv[ti + 1]
    corpus = json.load(open(corpus_path))
    slug = corpus["slug"]
    phrases = [p.strip() for p in template_raw.split(",")]
    tmpl, phrase_of = [], []
    for pi, p in enumerate(phrases):
        for w in p.split():
            tmpl.append(norm(w)); phrase_of.append(pi)
    n_syll = [syll_count(w) for w in tmpl]
    syl_labels = []
    for w, n in zip(tmpl, n_syll):
        syl_labels += [w if n == 1 else f"{w}·{k+1}" for k in range(n)]

    takes = []
    for clip in corpus["clips"]:
        path = os.path.join(DL, f"whistlegraph-{clip['id']}.syllnote.json")
        if not os.path.exists(path): continue
        doc = json.load(open(path))
        words = [norm(w["text"]) for w in doc["words"]]
        hit, score = align(tmpl, words)
        matched = sum(1 for h in hit if h is not None)
        if matched < 0.65 * len(tmpl): continue
        # per template word → nuclei of its matched take word
        per_word = []
        for wi, h in enumerate(hit):
            per_word.append(doc["words"][h]["nuclei"] if h is not None else [])
        # per-take pitch center: weighted median over all matched nuclei
        allm = [(n["midi"] + n["cents"] / 100, n["rms"] * n["durSec"]) for ns in per_word for n in ns]
        if len(allm) < 8: continue
        vals = np.array([m for m, _ in allm]); wts = np.array([w for _, w in allm])
        order = np.argsort(vals); cum = np.cumsum(wts[order])
        center = float(vals[order[np.searchsorted(cum, cum[-1] / 2)]])
        # spread word nuclei onto syllable slots
        syls = []
        for wi, ns in enumerate(per_word):
            need = n_syll[wi]
            ns = sorted(ns, key=lambda n: n["startSec"])
            if len(ns) > need:                       # keep the strongest, in time order
                keep = sorted(sorted(ns, key=lambda n: -(n["rms"] * n["durSec"]))[:need],
                              key=lambda n: n["startSec"])
            else: keep = ns
            for k in range(need):
                syls.append(keep[k] if k < len(keep) else None)
        onsets = [s["startSec"] for s in syls if s]
        if len(onsets) < 8: continue
        t0 = min(onsets)
        iois = np.diff(sorted(onsets)); med_ioi = float(np.median(iois[iois > 0.05])) if len(iois) else 0.25
        takes.append({"id": clip["id"], "date": clip.get("date"), "score": score,
                      "center": center, "medIoi": med_ioi,
                      "syls": [None if s is None else
                               {"rel": round(s["midi"] + s["cents"] / 100 - center, 2),
                                "beat": round((s["startSec"] - t0) / med_ioi, 2),
                                "dur": round(s["durSec"] / med_ioi, 2)} for s in syls]})
    if not takes:
        print("✗ no takes matched the template"); sys.exit(1)

    # ── iterative register refinement ─────────────────────────────────
    # A take's median pitch is a crude register anchor (he sings this low,
    # high and in falsetto across the years). Refit each take's offset
    # against the corpus skeleton, folding per-syllable octave errors
    # toward it, until the skeleton stops moving.
    nslots = len(syl_labels)
    for _ in range(3):
        skel = []
        for si in range(nslots):
            rels = [t["syls"][si]["rel"] for t in takes if t["syls"][si]]
            skel.append(float(np.median(rels)) if rels else None)
        for t in takes:
            for s in t["syls"]:
                if not s: continue
            diffs = [t["syls"][si]["rel"] - skel[si]
                     for si in range(nslots) if t["syls"][si] and skel[si] is not None]
            if not diffs: continue
            off = float(np.median(diffs))
            for si in range(nslots):
                s = t["syls"][si]
                if not s: continue
                s["rel"] = round(s["rel"] - off, 2)
                if skel[si] is not None:                 # fold octave errors
                    while s["rel"] - skel[si] > 7:  s["rel"] = round(s["rel"] - 12, 2)
                    while s["rel"] - skel[si] < -7: s["rel"] = round(s["rel"] + 12, 2)

    stats = []
    for si, label in enumerate(syl_labels):
        rels = [t["syls"][si]["rel"] for t in takes if t["syls"][si]]
        beats = [t["syls"][si]["beat"] for t in takes if t["syls"][si]]
        durs = [t["syls"][si]["dur"] for t in takes if t["syls"][si]]
        if not rels:
            stats.append({"syl": label, "phrase": phrase_of[min(si, len(phrase_of)-1)], "n": 0}); continue
        stats.append({"syl": label, "phrase": phrase_of[sum(n_syll[:next(i for i in range(len(tmpl)) if sum(n_syll[:i+1]) > si)])] if False else None,
                      "n": len(rels),
                      "rel": round(float(np.median(rels)), 2),
                      "relIQR": [round(float(np.percentile(rels, 25)), 2), round(float(np.percentile(rels, 75)), 2)],
                      "beat": round(float(np.median(beats)), 2),
                      "dur": round(float(np.median(durs)), 2)})
    # phrase index per syllable slot
    k = 0
    for wi, w in enumerate(tmpl):
        for _ in range(n_syll[wi]):
            stats[k]["phrase"] = phrase_of[wi]; k += 1

    out = {"slug": slug, "template": template_raw, "takes": len(takes),
           "takeIds": [t["id"] for t in takes],
           "skeleton": stats}
    out_path = os.path.join(DL, f"{slug}.melody-stats.json")
    json.dump(out, open(out_path, "w"), indent=1)
    print(f"✓ {out_path} · {len(takes)} takes matched")
    print(f"{'syllable':<14}{'n':>4}{'rel':>7}{'IQR':>15}{'beat':>7}{'dur':>6}")
    for s in stats:
        if s["n"] == 0: print(f"{s['syl']:<14}{0:>4}      —"); continue
        print(f"{s['syl']:<14}{s['n']:>4}{s['rel']:>7}{str(s['relIQR']):>15}{s['beat']:>7}{s['dur']:>6}")

if __name__ == "__main__":
    main()
