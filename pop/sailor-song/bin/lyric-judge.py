#!/usr/bin/env python
# lyric-judge.py — does the video's lyric timing land on her words? (v103: "let's use whisper openai to judge that.")
#
# src/words-record.json (master clock, the forced alignment + jeffrey's ear) is carried onto the take clock through
# the timemap and set beside OpenAI whisper-1's word times for the dry stem (src/.word-times/openai-whisper1.json,
# take clock; word-times.py cached it). Words are matched in order by text, each onset's difference measured, and
# the verdict printed per line. Where a whole line disagrees with confidence (≥ 6 matched words, 60–400 ms) that
# line is shifted by its median into src/words-video.json, which perf-relight.mjs prefers when it exists —
# words-record.json itself is never touched. whisper-1 drifts on held notes and by ~a second at the END of a take
# (pop/factory/bin/audit.py saw 0.6–1.4 s; here the last three bridge lines read −1.1 s together), so single-word
# outliers and whole-second line offsets are reported, not applied.
#
# --apply also gives every word its SYLLABLES (words-record.json carries none): the spelling is cut at vowel groups
# and the cuts are placed on the dry stem's strongest energy onsets inside the word's span, so the ball can land once
# per syllable and the letters fill in syllable by syllable.
#
#   pop/.venv/bin/python pop/sailor-song/bin/lyric-judge.py [--apply]
import json, os, re, sys, statistics as st

LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__))); SRC = os.path.join(LANE, "src")
WORDS = json.load(open(os.path.join(SRC, "words-record.json")))
WH = json.load(open(os.path.join(SRC, ".word-times", "openai-whisper1.json")))["words"]
R = sorted(json.load(open(sorted([os.path.join(LANE, "out", f) for f in os.listdir(os.path.join(LANE, "out")) if re.match(r"sailor-song-v\d+\.events\.json$", f)],
          key=lambda f: int(re.search(r"v(\d+)", f).group(1)))[-1])).get("startSec", 0) for _ in [0])[0]
pairs = [tuple(map(int, l.split())) for l in open(os.path.join(SRC, "vox/reg/timemap.txt")) if l.strip()]
def takeOf(reg):
    x = reg * 48000; i = 1
    while i < len(pairs) - 1 and pairs[i][1] < x: i += 1
    (s0, d0), (s1, d1) = pairs[i - 1], pairs[i]; f = 0 if d1 == d0 else (x - d0) / (d1 - d0); return (s0 + (s1 - s0) * f) / 48000
norm = lambda t: re.sub(r"[^a-z0-9']", "", t.lower().replace("’", "'"))
lines = [l.strip() for l in open(os.path.join(SRC, "lyrics-sung.txt")) if l.strip() and not l.startswith("[")]

# words-record → lines (the same greedy walk perf-relight uses), each word's onset on the take clock
k = 0; LINES = []
for line in lines:
    got = []; kk = k
    for tok in line.split():
        n = norm(tok)
        if not n: continue
        j = kk
        while j < len(WORDS) and j < kk + 6 and norm(WORDS[j]["text"]) != n: j += 1
        if j < len(WORDS) and j < kk + 6: got.append(j); kk = j + 1
    if got: LINES.append((line, got)); k = kk
# whisper words, in order, matched to record words by text within a window
wi = 0; deltas = {}
for line, idx in LINES:
    for j in idx:
        n = norm(WORDS[j]["text"]); t = takeOf(WORDS[j]["fromMs"] / 1000 + R)
        for q in range(wi, min(len(WH), wi + 8)):
            if norm(WH[q]["word"]) == n and abs(WH[q]["start"] - t) < 1.5:
                deltas[j] = WH[q]["start"] - t; wi = q + 1; break
all_d = list(deltas.values())
print(f"▸ {len(all_d)}/{len(WORDS)} words matched to whisper-1 · median {st.median(all_d) * 1000:+.0f} ms · "
      f"p90 |Δ| {sorted(abs(d) for d in all_d)[int(0.9 * len(all_d)) - 1] * 1000:.0f} ms  (whisper minus record; + = record early)")
shift = {}
for line, idx in LINES:
    d = [deltas[j] for j in idx if j in deltas]
    if not d: print(f"   ?    {line}"); continue
    med = st.median(d); flag = "SHIFT" if len(d) >= 6 and 0.06 < abs(med) < 0.4 else ("drift" if len(d) >= 3 and abs(med) > 0.06 else "ok   ")
    worst = max((abs(deltas[j]), WORDS[j]["text"], deltas[j]) for j in idx if j in deltas)
    print(f"  {flag} {med * 1000:+5.0f} ms  n={len(d):2d}  worst {worst[1]!r} {worst[2] * 1000:+.0f} ms   {line}")
    if flag == "SHIFT": shift.update({j: med for j in idx})
def syllables(text):
    """vowel-group split of the spelling: 'sailor?' → ['sai', 'lor?'], 'believe' → ['be', 'lieve'], 'coughed' → ['coughed'],
    'rightest' → ['right', 'est'], 'Hathaway' → ['Ha', 'tha', 'way']. Digraphs stay whole; a final silent e or -ed is no syllable."""
    core = text.rstrip("?,.!\"'"); low = core.lower(); groups = [m for m in re.finditer(r"[aeiouy]+", low)]
    if len(groups) >= 2 and groups[-1].group() == "e" and (low.endswith("e") or (low.endswith("ed") and low[-3:-2] not in "td" and groups[-1].start() == len(low) - 2)): groups = groups[:-1]
    if len(groups) < 2: return [text]
    cuts = []
    for g0, g1 in zip(groups, groups[1:]):
        run = low[g0.end():g1.start()]; n = len(run)
        if n <= 1: c = 0                                                # V|CV
        elif run[:2] in ("ch", "sh", "th", "ph", "wh"): c = 0            # the digraph opens the next syllable
        elif run[:2] == "gh": c = 2 + (1 if run[2:3] == "t" else 0)      # a coda: laugh|in', right|est
        elif run[:2] in ("ck", "ng"): c = 2
        else: c = 1                                                     # VC|CV
        cuts.append(g0.end() + c)
    parts, prev = [], 0
    for c in cuts: parts.append(text[prev:c]); prev = c
    parts.append(text[prev:]); return [p for p in parts if p]
if "--apply" in sys.argv:
    import numpy as np, librosa
    y, sr = librosa.load(os.path.join(SRC, ".word-times", "dry16.wav"), sr=16000)
    env = librosa.onset.onset_strength(y=y, sr=sr, hop_length=160); tt = librosa.frames_to_time(np.arange(len(env)), sr=sr, hop_length=160)
    # a SHIFT is applied only when it brings the line's onsets measurably nearer her attacks on the dry stem (> 15 ms)
    ons = librosa.onset.onset_detect(y=y, sr=sr, units="time", hop_length=160, backtrack=True)
    near = lambda t: float(np.min(np.abs(ons - t)))
    for line, idx in LINES:
        if idx[0] not in shift: continue
        d0 = st.median(near(takeOf(WORDS[j]["fromMs"] / 1000 + R)) for j in idx); d1 = st.median(near(takeOf(WORDS[j]["fromMs"] / 1000 + shift[idx[0]] + R)) for j in idx)
        keep = d1 < d0 - 0.015; print(f"  {'keep ' if keep else 'DROP '} shift {shift[idx[0]] * 1000:+.0f} ms: record {d0 * 1000:.0f} ms from her attacks, shifted {d1 * 1000:.0f} ms   {line}")
        if not keep:
            for j in idx: shift.pop(j, None)
    out = []; nsyl = 0
    for j, w in enumerate(WORDS):
        w = dict(w)
        if j in shift: w["fromMs"] = round(w["fromMs"] + shift[j] * 1000); w["toMs"] = round(w["toMs"] + shift[j] * 1000); w["judge"] = round(shift[j], 3)
        parts = syllables(w["text"]); a, b = w["fromMs"] / 1000, w["toMs"] / 1000
        if len(parts) > 1:
            ta, tb = takeOf(a + R), takeOf(b + R); m = (tt > ta + 0.06) & (tt < tb - 0.04); cand = np.where(m)[0]
            peaks = [i for i in cand if env[i] >= env[max(0, i - 2):i + 3].max()]; peaks.sort(key=lambda i: -env[i])
            picked = []
            for i in peaks:                                            # the strongest onsets, at least 80 ms apart
                if all(abs(tt[i] - tt[q]) > 0.08 for q in picked): picked.append(i)
                if len(picked) == len(parts) - 1: break
            if len(picked) < len(parts) - 1:                           # too few onsets heard: the rest spread evenly
                have = sorted(tt[i] for i in picked); need = len(parts) - 1 - len(have); have += [ta + (tb - ta) * (k + 1) / (need + 1) for k in range(need)]
                bounds = sorted(have)
            else: bounds = sorted(tt[i] for i in picked)
            fr = [a] + [a + (tk - ta) / (tb - ta) * (b - a) for tk in bounds]; to = fr[1:] + [b]   # back onto the master clock, proportionally
            w["tokens"] = [{"text": p, "fromMs": round(f * 1000), "toMs": round(t * 1000)} for p, f, t in zip(parts, fr, to)]; nsyl += len(parts)
        else: w["tokens"] = [{"text": w["text"], "fromMs": w["fromMs"], "toMs": w["toMs"]}]; nsyl += 1
        out.append(w)
    json.dump(out, open(os.path.join(SRC, "words-video.json"), "w"), indent=1)
    print(f"✓ src/words-video.json — {len(shift)} words in {sum(1 for l, idx in LINES if idx[0] in shift)} lines shifted; {nsyl} syllables over {len(out)} words")
