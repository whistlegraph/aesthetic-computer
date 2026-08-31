#!/usr/bin/env python3
"""syllabound.py — word/syllable boundaries from the SIGNAL, not whisper.

Whisper's word windows collapse on fast sung runs (it gave 'flapping'
2.6 s and hid 'for you' inside it). This works from what's actually
there: segment the stem into voiced NUCLEI (vowel cores) and unvoiced/
breath spans, then assign nuclei to the expected syllable sequence in
order — whisper only supplies soft anchors. Words whose vowels are
breathy (no pyin nucleus) get their share of the unvoiced span between
their neighbours. Output = the bounds format holyvox consumes, one
entry PER SYLLABLE (word boundaries derive from syllable ones).

  pop/.venv/bin/python pop/imab/bin/syllabound.py <stem.wav> <syllnote.json> <out.json>

Hand overrides (boundaries-<take>.json) still win when present.
"""
import json, os, re, sys
import numpy as np
import librosa

TMPL = "i'm a butterfly flapping for you guys just a costume i put on in my room".split(" ")
NSYL = {"butterfly": 3, "flapping": 2, "costume": 2}
SYL_TEXT = {"butterfly": ["but", "ter", "fly"], "flapping": ["flap", "ping"], "costume": ["cos", "tume"]}

wav, sylj, outp = sys.argv[1], sys.argv[2], sys.argv[3]
doc = json.load(open(sylj))
y, sr = librosa.load(wav, sr=22050, mono=True)
hop = 128
f0, v, vp = librosa.pyin(y, sr=sr, fmin=80, fmax=700, frame_length=1024, hop_length=hop)
rms = librosa.feature.rms(y=y, frame_length=1024, hop_length=hop)[0]
t = librosa.times_like(rms, sr=sr, hop_length=hop)
peak = np.percentile(rms, 97)
sung = rms > peak * 0.06

# ── voiced nuclei: vowel cores ────────────────────────────────────────
ok = v & (vp > 0.2) & np.isfinite(f0) & sung
nuclei = []
cur = None
for i in range(len(t)):
    if ok[i]:
        cur = [i, i] if cur is None else [cur[0], i]
    else:
        if cur and t[cur[1]] - t[cur[0]] >= 0.05:
            nuclei.append(cur)
        cur = None
if cur and t[cur[1]] - t[cur[0]] >= 0.05: nuclei.append(cur)
# split long nuclei at strong internal energy dips (two vowels joined)
split = []
for a, b in nuclei:
    seg = rms[a:b + 1]
    if t[b] - t[a] > 0.55 and len(seg) > 20:
        mid = seg[5:-5]
        k = np.argmin(mid) + 5
        if mid.min() < seg.max() * 0.45:
            split += [[a, a + k], [a + k + 1, b]]
            continue
    split.append([a, b])
nuclei = split

# ── expected syllable stream with whisper soft anchors ────────────────
def norm(w): return re.sub(r"[^a-z']", "", w.lower())
def fuzzy(a, b):
    return a == b or (len(a) > 3 and len(b) > 3 and (a.startswith(b[:4]) or b.startswith(a[:4]))) \
        or (a in ("a", "the") and b in ("a", "the"))
anchors = {}
ti = 0
for w in doc["words"]:
    if ti < len(TMPL) and fuzzy(TMPL[ti], norm(w["text"])):
        anchors[ti] = (w["fromMs"] / 1000, w["toMs"] / 1000); ti += 1
sylls = []
for wi, word in enumerate(TMPL):
    n = NSYL.get(word, 1)
    for k in range(n):
        label = SYL_TEXT[word][k] if word in SYL_TEXT else word
        sylls.append({"wi": wi, "k": k, "label": label,
                      "anchor": anchors.get(wi, (None, None))})

# ── assign nuclei→syllables in order (DP, anchor-priced) ──────────────
NN, NS = len(nuclei), len(sylls)
INF = 1e9
cost = np.full((NS + 1, NN + 1), INF)
back = np.zeros((NS + 1, NN + 1), dtype=int)   # 0=skip syll, 1=take, 2=skip nucleus
cost[0, 0] = 0
for si in range(NS + 1):
    for ni in range(NN + 1):
        c = cost[si, ni]
        if c >= INF: continue
        if si < NS and cost[si + 1, ni] > c + 1.2:        # syllable with no nucleus
            cost[si + 1, ni] = c + 1.2; back[si + 1, ni] = 0
        if ni < NN and cost[si, ni + 1] > c + 1.0:        # stray nucleus
            cost[si, ni + 1] = c + 1.0; back[si, ni + 1] = 2
        if si < NS and ni < NN:
            a, b = nuclei[ni]
            mid = (t[a] + t[b]) / 2
            aw = sylls[si]["anchor"]
            pen = 0.0
            if aw[0] is not None:
                d = max(0.0, aw[0] - 0.6 - mid, mid - (aw[1] + 0.6))
                pen = d * 2.0
            if cost[si + 1, ni + 1] > c + pen:
                cost[si + 1, ni + 1] = c + pen; back[si + 1, ni + 1] = 1
si, ni = NS, NN
take = {}
while si > 0 or ni > 0:
    m = back[si, ni]
    if m == 1: si -= 1; ni -= 1; take[si] = ni
    elif m == 0: si -= 1
    else: ni -= 1

# ── boundaries: valleys between assigned nuclei; breathy sylls share gaps
for si, s in enumerate(sylls):
    if si in take:
        a, b = nuclei[take[si]]
        s["nt0"], s["nt1"] = float(t[a]), float(t[b])
placed = [si for si in range(NS) if "nt0" in sylls[si]]
for idx, si in enumerate(placed):
    s = sylls[si]
    prev_end = sylls[placed[idx - 1]]["nt1"] if idx else max(0.0, s["nt0"] - 0.25)
    nxt_start = sylls[placed[idx + 1]]["nt0"] if idx + 1 < len(placed) else s["nt1"] + 0.4
    # onset: energy valley between previous nucleus end and this nucleus start
    lo, hi = prev_end, s["nt0"]
    sel = (t >= lo) & (t <= hi)
    s["t0"] = float(t[sel][np.argmin(rms[sel])]) if sel.sum() > 2 else lo
    s["t1"] = s["nt1"]
# fill breathy syllables into the span between neighbours, evenly
i = 0
while i < NS:
    if "t0" in sylls[i]: i += 1; continue
    j = i
    while j < NS and "t0" not in sylls[j]: j += 1
    left = sylls[i - 1]["t1"] if i else 0.0
    right = sylls[j]["t0"] if j < NS else left + 0.4 * (j - i)
    span = max(0.12 * (j - i), right - left)
    for k in range(i, j):
        sylls[k]["t0"] = left + (k - i) * span / (j - i)
        sylls[k]["t1"] = left + (k - i + 1) * span / (j - i)
    i = j
# neighbours must tile: each syllable runs to the next one's onset
for i in range(NS - 1):
    sylls[i]["t1"] = max(sylls[i]["t1"], sylls[i]["t0"] + 0.08)
    sylls[i]["t1"] = min(sylls[i]["t1"], sylls[i + 1]["t0"]) if sylls[i + 1]["t0"] > sylls[i]["t0"] else sylls[i]["t1"]

# word records for the bounds file + per-syllable detail
words_out = []
for wi, word in enumerate(TMPL):
    mine = [s for s in sylls if s["wi"] == wi]
    words_out.append({"ti": wi, "text": word,
                      "fromMs": int(mine[0]["t0"] * 1000), "toMs": int(mine[-1]["t1"] * 1000),
                      "sylls": [{"label": s["label"], "fromMs": int(s["t0"] * 1000),
                                 "toMs": int(s["t1"] * 1000), "voiced": "nt0" in s} for s in mine]})
# hand overrides still win
ov_path = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                       f"boundaries-{wav.split('whistlegraph-')[-1].split('/')[0]}.json")
if os.path.exists(ov_path):
    ov = json.load(open(ov_path)).get("overrides", {})
    for w in words_out:
        o = ov.get(str(w["ti"]))
        if o and o.get("syllabound") is not False and o.get("wins", False):
            w.update({k: o[k] for k in ("fromMs", "toMs") if k in o})
json.dump({"source": os.path.basename(wav), "method": "syllabound", "words": words_out},
          open(outp, "w"), indent=1)
for w in words_out:
    det = "  ".join(f"{s['label']}:{s['fromMs']/1000:.2f}-{s['toMs']/1000:.2f}{'' if s['voiced'] else '·breathy'}"
                    for s in w["sylls"])
    print(f"  {w['text']:<11} {det}")
print(f"✓ {outp}")
