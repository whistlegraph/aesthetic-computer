#!/usr/bin/env python3
"""shapebound.py — boundaries by SHAPE: learn the phrase's articulation
from takes that sing it cleanly, then DTW-align the target take against
each reference and carry the reference boundaries through the warp.

The smooshed region problem: one take blurs "flapping for you guys" so
no local analysis can segment it. But the corpus knows the shape of the
phrase — MFCC+energy sequences from articulate takes, whose whisper
boundaries are trustworthy, warp onto the target and vote.

  pop/.venv/bin/python pop/imab/bin/shapebound.py <target_take> <ref_take> [<ref_take>…]
  → ~/.cache/ac/imab/bounds-<target>.json (median of the refs' mapped boundaries)
"""
import json, os, re, sys
import numpy as np
import librosa

DL = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))),
                  "toolchain/whistlegraph/downloads")
WORK = os.path.expanduser("~/.cache/ac/imab")
TMPL = "i'm a butterfly flapping for you guys just a costume i put on in my room".split(" ")
NSYL = {"butterfly": 3, "flapping": 2, "costume": 2}
SYL_TEXT = {"butterfly": ["but", "ter", "fly"], "flapping": ["flap", "ping"], "costume": ["cos", "tume"]}
HOP = 256

def norm(w): return re.sub(r"[^a-z']", "", w.lower())
def fuzzy(a, b):
    return a == b or (len(a) > 3 and len(b) > 3 and (a.startswith(b[:4]) or b.startswith(a[:4]))) \
        or (a in ("a", "the") and b in ("a", "the"))

def load_take(take):
    """audio features + matched word windows for one take"""
    wav = f"{WORK}/sep/htdemucs/whistlegraph-{take}/vocals.wav"
    if not os.path.exists(wav):
        wav = f"{DL}/whistlegraph-{take}.wav"
    syl = json.load(open(f"{DL}/whistlegraph-{take}.syllnote.json"))
    seq = []
    ti = 0
    for w in syl["words"]:
        if ti < len(TMPL) and fuzzy(TMPL[ti], norm(w["text"])):
            seq.append({"ti": ti, "text": TMPL[ti], "a": w["fromMs"] / 1000, "b": w["toMs"] / 1000})
            ti += 1
    if ti < 15: return None
    t0 = max(0.0, seq[0]["a"] - 0.2)
    t1 = seq[-1]["b"] + 0.4
    y, sr = librosa.load(wav, sr=22050, mono=True, offset=t0, duration=t1 - t0)
    mf = librosa.feature.mfcc(y=y, sr=sr, n_mfcc=20, hop_length=HOP)
    e = librosa.feature.rms(y=y, frame_length=1024, hop_length=HOP)[0]
    feats = np.vstack([mf, np.log(e + 1e-6)[None, :]])
    feats = (feats - feats.mean(axis=1, keepdims=True)) / (feats.std(axis=1, keepdims=True) + 1e-6)
    return {"take": take, "t0": t0, "sr": sr, "feats": feats, "words": seq}

def syll_bounds(tk):
    """per-syllable boundaries inside each word window (even split)"""
    out = []
    for w in tk["words"]:
        n = NSYL.get(w["text"], 1)
        for k in range(n):
            a = w["a"] + (w["b"] - w["a"]) * k / n
            b = w["a"] + (w["b"] - w["a"]) * (k + 1) / n
            lab = SYL_TEXT[w["text"]][k] if w["text"] in SYL_TEXT else w["text"]
            out.append({"ti": w["ti"], "label": lab, "a": a, "b": b})
    return out

target = sys.argv[1]
refs = sys.argv[2:]
tgt = load_take(target)
assert tgt, "target didn't match template"
spf = HOP / 22050.0

votes = {}                                       # syll index → list of (a,b) in target time
for r in refs:
    ref = load_take(r)
    if not ref: print(f"  ✗ ref {r} unusable"); continue
    D, wp = librosa.sequence.dtw(X=ref["feats"], Y=tgt["feats"], metric="euclidean")
    wp = wp[::-1]                                # start→end
    ref_frames = wp[:, 0]; tgt_frames = wp[:, 1]
    def map_time(ts):
        fr = (ts - ref["t0"]) / spf
        i = np.searchsorted(ref_frames, fr)
        i = min(len(tgt_frames) - 1, i)
        return tgt["t0"] + tgt_frames[i] * spf
    for si, s in enumerate(syll_bounds(ref)):
        votes.setdefault(si, {"label": s["label"], "ti": s["ti"], "abs": []})
        votes[si]["abs"].append((map_time(s["a"]), map_time(s["b"])))
    print(f"  ✓ ref {r} warped ({len(wp)} path points)")

sylls = []
for si in sorted(votes):
    v = votes[si]
    a = float(np.median([x[0] for x in v["abs"]]))
    b = float(np.median([x[1] for x in v["abs"]]))
    sylls.append({"ti": v["ti"], "label": v["label"], "a": a, "b": b})
for i in range(len(sylls) - 1):                  # tile: monotonic, no overlap
    if sylls[i + 1]["a"] < sylls[i]["a"] + 0.06:
        sylls[i + 1]["a"] = sylls[i]["a"] + 0.06
    sylls[i]["b"] = max(sylls[i]["a"] + 0.05, min(sylls[i]["b"], sylls[i + 1]["a"]))

words_out = []
for wi, word in enumerate(TMPL):
    mine = [s for s in sylls if s["ti"] == wi]
    if not mine: continue
    words_out.append({"ti": wi, "text": word,
                      "fromMs": int(mine[0]["a"] * 1000), "toMs": int(mine[-1]["b"] * 1000),
                      "sylls": [{"label": s["label"], "fromMs": int(s["a"] * 1000),
                                 "toMs": int(s["b"] * 1000), "voiced": True} for s in mine]})
json.dump({"source": f"shapebound: refs {' '.join(refs)}", "words": words_out},
          open(f"{WORK}/bounds-{target}.json", "w"), indent=1)
for w in words_out:
    det = "  ".join(f"{s['label']}:{s['fromMs']/1000:.2f}-{s['toMs']/1000:.2f}" for s in w["sylls"])
    print(f"  {w['text']:<11} {det}")
print(f"✓ {WORK}/bounds-{target}.json")
