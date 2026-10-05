#!/usr/bin/env python
# lyric-bounds.py — every sung word's start and end, measured on HER VOCAL TRACK (v103: "make sure we make the timing
# of every sung word properly, to the vocal track, so we have better bounds").
#
# The record's words (src/words-record.json, master clock) are carried onto the take clock through the timemap and set
# against two witnesses on the dry stem (src/.word-times/dry16.wav): the MMS forced alignment (forced-align.json: the
# known lyric placed phonetically) and the stem's own loudness. A word STARTS at the steepest rise of the voiced-energy
# envelope near its recorded onset — the forced alignment's onset when the two agree within 150 ms, snapped to the rise
# — and ENDS where the voice has gone (the envelope within 8 dB of the stem's floor, or 26 dB under the word's peak: a held
# note decays and is still sung), or at the next word's start when she sings through (legato), whichever is first. Syllables are then re-placed on the energy onsets inside the
# word (lyric-judge.py's rule). Written to src/words-video.json (master clock), which perf-relight.mjs prefers;
# words-record.json is never touched. Every move over 60 ms is printed, so the ear can check the worst.
#
#   pop/.venv/bin/python pop/sailor-song/bin/lyric-bounds.py [--clip A,B]   (clip: also cut a lyric check A–B s, record clock)
import json, os, re, sys, subprocess, statistics as st
import numpy as np, librosa

LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__))); SRC = os.path.join(LANE, "src"); OUTD = os.path.join(LANE, "out")
WORDS = json.load(open(os.path.join(SRC, "words-record.json")))
FA = json.load(open(os.path.join(SRC, ".word-times", "forced-align.json")))
newest = sorted([f for f in os.listdir(OUTD) if re.match(r"sailor-song-v\d+\.events\.json$", f)], key=lambda f: int(re.search(r"v(\d+)", f).group(1)))[-1]
T0 = json.load(open(os.path.join(OUTD, newest))).get("startSec", 0); STEM = newest.replace(".events.json", "")
pairs = [tuple(map(int, l.split())) for l in open(os.path.join(SRC, "vox/reg/timemap.txt")) if l.strip()]
def takeOf(reg):
    x = reg * 48000; i = 1
    while i < len(pairs) - 1 and pairs[i][1] < x: i += 1
    (s0, d0), (s1, d1) = pairs[i - 1], pairs[i]; f = 0 if d1 == d0 else (x - d0) / (d1 - d0); return (s0 + (s1 - s0) * f) / 48000
def regOf(take):
    x = take * 48000; i = 1
    while i < len(pairs) - 1 and pairs[i][0] < x: i += 1
    (s0, d0), (s1, d1) = pairs[i - 1], pairs[i]; f = 0 if s1 == s0 else (x - s0) / (s1 - s0); return (d0 + (d1 - d0) * f) / 48000
master = lambda take: regOf(take) - T0
norm = lambda t: re.sub(r"[^a-z0-9']", "", t.lower().replace("’", "'"))

# the stem's loudness: RMS every 5 ms over 25 ms, in dB, lightly smoothed; its rise rate
y, sr = librosa.load(os.path.join(SRC, ".word-times", "dry16.wav"), sr=16000)
HOP = 80; rms = librosa.feature.rms(y=y, frame_length=400, hop_length=HOP)[0]; db = 20 * np.log10(rms + 1e-6)
db = np.convolve(db, np.ones(5) / 5, mode="same"); tt = librosa.frames_to_time(np.arange(len(db)), sr=sr, hop_length=HOP)
rise = np.gradient(db, tt); floor = np.percentile(db, 10)
at = lambda t: int(np.clip(np.searchsorted(tt, t), 0, len(tt) - 1))
def steepest_rise(t0, t1, fallback):
    """the steepest rise strictly inside (t0, t1); a best that sits on the window's edge is the neighbour's rise, not this word's"""
    a, b = at(t0), at(t1)
    if b <= a + 2: return fallback
    i = a + int(np.argmax(rise[a:b]))
    if i <= a or i >= b - 1 or rise[i] < 60: return fallback                 # < 60 dB/s is not an attack
    return float(tt[i])
def offset_after(peak_t, limit_t, peak_db):
    """first time after the peak where the envelope sits 10 dB under it (or 6 dB over the floor), before the limit"""
    a, b = at(peak_t), at(limit_t); thr = max(peak_db - 26, floor + 8)
    for i in range(a, max(a + 1, b)):
        if db[i] < thr: return float(tt[i])
    return limit_t

# forced alignment, matched in order by text (both lists are the whole lyric, so this is a straight walk with slack)
fa_i = 0; fa_of = {}
for j, w in enumerate(WORDS):
    for q in range(fa_i, min(len(FA), fa_i + 4)):
        if norm(FA[q]["text"]) == norm(w["text"]): fa_of[j] = FA[q]; fa_i = q + 1; break

# whisper-1's words (take clock), matched in order by text, as the SECOND independent witness: where it and the
# forced alignment agree within 250 ms on a word's onset, that onset is the anchor even when the record disagrees
# (v103: the bridge — words-record runs ~1.2 s late from "we can run away" on; both witnesses say so)
WH = json.load(open(os.path.join(SRC, ".word-times", "openai-whisper1.json")))["words"]; wh_i = 0; wh_of = {}
for j, w in enumerate(WORDS):
    for q in range(wh_i, min(len(WH), wh_i + 8)):
        if norm(WH[q]["word"]) == norm(w["text"]): wh_of[j] = WH[q]; wh_i = q + 1; break
anchor = {j: fa_of[j]["from"] for j in fa_of if j in wh_of and abs(wh_of[j]["start"] - fa_of[j]["from"]) < 0.25}
# an anchored word vouches for the next one when the forced alignment runs them together (< 0.5 s apart) — whisper
# mis-segments a held last word ("sit it OUT": it heard 'it' for three seconds), the phonetic aligner does not
for j in sorted(list(anchor)):
    k = j + 1
    while k < len(WORDS) and k not in anchor and k in fa_of and j in fa_of and fa_of[k]["from"] - fa_of[k - 1]["to"] < 0.5 and fa_of[k - 1]["to"] >= fa_of[k - 1]["from"]:
        anchor[k] = fa_of[k]["from"]; j = k; k += 1
# the bounds
rec = [(takeOf(w["fromMs"] / 1000 + T0), takeOf(w["toMs"] / 1000 + T0)) for w in WORDS]
# a record onset more than 0.4 s from an anchored neighbourhood is slid onto the witnesses: each unanchored word takes
# the median offset of the anchored words within ±3 s of it (when there are at least three), before the fine search
offs = {j: anchor[j] - rec[j][0] for j in anchor}
for j in range(len(WORDS)):
    if j in anchor: continue
    near = [offs[k] for k in offs if abs(rec[k][0] - rec[j][0]) < 3.0]
    if len(near) >= 3 and abs(st.median(near)) > 0.4: rec[j] = (rec[j][0] + st.median(near), rec[j][1] + st.median(near))
for j in anchor: rec[j] = (anchor[j], rec[j][1] + (anchor[j] - rec[j][0]))
slid = sum(1 for j in range(len(WORDS)) if abs(rec[j][0] - takeOf(WORDS[j]["fromMs"] / 1000 + T0)) > 0.4)
print(f"▸ {len(anchor)} words anchored by forced alignment ∧ whisper; {slid} words slid > 0.4 s onto the witnesses before the fine search")
starts, ends = [], []
for j, w in enumerate(WORDS):
    a, b = rec[j]; fa = fa_of.get(j)
    lo = max(a - 0.15, (starts[-1] + 0.1) if starts else -1)                                                # never back into the word before
    if fa and abs(fa["from"] - a) < 0.15: s = steepest_rise(max(lo, fa["from"] - 0.06), fa["from"] + 0.06, fa["from"])   # the two witnesses agree: the phonetic onset, on the rise
    else: s = steepest_rise(lo, a + 0.12, a)                                                                # else the loudest rise around the record's onset
    if starts: s = max(s, starts[-1] + 0.08)                                                                # order is kept
    starts.append(s)
    nxt = rec[j + 1][0] if j + 1 < len(WORDS) else b + 1.0
    far = max(b, fa["to"] if fa and fa["to"] > fa["from"] else b)                                            # a held last note runs as long as the aligner heard it
    p0, p1 = at(s), at(max(s + 0.06, min(far + 0.25, nxt + 0.1))); pk = p0 + int(np.argmax(db[p0:p1])) if p1 > p0 else p0
    e = offset_after(tt[pk], min(far + 0.3, nxt + 0.05), db[pk]); e = max(e, tt[pk] + 0.04, s + 0.08)
    ends.append(e)
for j in range(len(WORDS) - 1):                                                                             # a word never runs into the next one's start
    if ends[j] > starts[j + 1]: ends[j] = starts[j + 1]

# syllables (lyric-judge.py's rule): the spelling cut at vowel groups, the cuts on the strongest onsets inside the word
def syllables(text):
    core = text.rstrip("?,.!\"'"); low = core.lower(); groups = [m for m in re.finditer(r"[aeiouy]+", low)]
    if len(groups) >= 2 and groups[-1].group() == "e" and (low.endswith("e") or (low.endswith("ed") and low[-3:-2] not in "td" and groups[-1].start() == len(low) - 2)): groups = groups[:-1]
    if len(groups) < 2: return [text]
    cuts = []
    for g0, g1 in zip(groups, groups[1:]):
        run = low[g0.end():g1.start()]; n = len(run)
        if n <= 1: c = 0
        elif run[:2] in ("ch", "sh", "th", "ph", "wh"): c = 0
        elif run[:2] == "gh": c = 2 + (1 if run[2:3] == "t" else 0)
        elif run[:2] in ("ck", "ng"): c = 2
        else: c = 1
        cuts.append(g0.end() + c)
    parts, prev = [], 0
    for c in cuts: parts.append(text[prev:c]); prev = c
    parts.append(text[prev:]); return [p for p in parts if p]
env = librosa.onset.onset_strength(y=y, sr=sr, hop_length=160); et = librosa.frames_to_time(np.arange(len(env)), sr=sr, hop_length=160)

# the holds ring on in the RECORD longer than on the dry stem — the sister voices, the mirror and the aaa carry a 'long'
# for a second after she stops — so a word held > 1.2 s stays lit until the receipt's last vocal-layer event over it
# has ended (the newest events.json is the mix the video plays). (v103: "the second long cuts off a few seconds early")
EV = json.load(open(os.path.join(OUTD, newest))).get("events", []); VOC = {"vox", "sister", "chorale", "mirror", "aaa", "ooo", "halo"}
ext = 0
for j in range(len(WORDS) - 1):                                               # (not the last word: the final 'out' keeps her own utterance's length — jeffrey)
    if ends[j] - starts[j] < 1.2: continue
    a, b = regOf(starts[j]), regOf(ends[j]); ring = b
    while True:                                                                   # chain outward: a layer that starts before the ring ends (+0.3 s) extends it (the ooo's carry the second long into the break)
        nb = max([e["t"] + e.get("dur", 0) for e in EV if e["voice"] in VOC and e["t"] < ring + 0.3 and e["t"] + e.get("dur", 0) > a], default=ring)
        if nb <= ring + 0.01: break
        ring = nb                                                                 # (the next word's start is the only cap: a 'long' rides its whole ring)
    nxt = regOf(starts[j + 1]) if j + 1 < len(WORDS) else ring + 1; new_end = takeOf(min(ring, nxt - 0.05))
    if new_end > ends[j] + 0.2: ext += 1; print(f"  hold {WORDS[j]['text']!r} @ {master(starts[j]):.2f}s: stem +{ends[j] - starts[j]:.2f}s → the record's layers ring to +{new_end - starts[j]:.2f}s"); ends[j] = new_end
print(f"▸ {ext} holds extended to the record's ring ({newest})")
out = []; moved = []
for j, w in enumerate(WORDS):
    s, e = starts[j], ends[j]; a, b = rec[j]; ds, de = s - a, e - b
    w2 = dict(w); w2["fromMs"] = round(master(s) * 1000); w2["toMs"] = round(master(e) * 1000); w2["bounds"] = {"dStart": round(ds, 3), "dEnd": round(de, 3)}
    parts = syllables(w["text"])
    if len(parts) > 1:
        m = (et > s + 0.06) & (et < e - 0.04); cand = np.where(m)[0]
        peaks = sorted([i for i in cand if env[i] >= env[max(0, i - 2):i + 3].max()], key=lambda i: -env[i]); picked = []
        for i in peaks:
            if all(abs(et[i] - et[q]) > 0.08 for q in picked): picked.append(i)
            if len(picked) == len(parts) - 1: break
        have = sorted(et[i] for i in picked); need = len(parts) - 1 - len(have)
        bounds = sorted(have + [s + (e - s) * (k + 1) / (need + 1) for k in range(need)])
        fr = [s] + bounds; to = fr[1:] + [e]
        w2["tokens"] = [{"text": p, "fromMs": round(master(f) * 1000), "toMs": round(master(t) * 1000)} for p, f, t in zip(parts, fr, to)]
    else: w2["tokens"] = [{"text": w["text"], "fromMs": w2["fromMs"], "toMs": w2["toMs"]}]
    out.append(w2)
    if abs(ds) > 0.06 or abs(de) > 0.06: moved.append((j, w["text"], ds, de, master(s)))
json.dump(out, open(os.path.join(SRC, "words-video.json"), "w"), indent=1)
ds_all = [starts[j] - rec[j][0] for j in range(len(WORDS))]; de_all = [ends[j] - rec[j][1] for j in range(len(WORDS))]
print(f"✓ src/words-video.json — {len(WORDS)} words bounded on the stem · start moved median {st.median(ds_all) * 1000:+.0f} ms, |Δ| p90 {sorted(abs(d) for d in ds_all)[int(0.9 * len(ds_all))] * 1000:.0f} ms"
      f" · end moved median {st.median(de_all) * 1000:+.0f} ms, |Δ| p90 {sorted(abs(d) for d in de_all)[int(0.9 * len(de_all))] * 1000:.0f} ms · {len(moved)} words moved > 60 ms")
for j, text, ds, de, m in sorted(moved, key=lambda x: -max(abs(x[2]), abs(x[3])))[:24]:
    print(f"  {m:7.2f}s  {text!r:14s} start {ds * 1000:+5.0f} ms   end {de * 1000:+5.0f} ms")
if "--plot" in sys.argv:
    import cv2
    A, B = map(float, sys.argv[sys.argv.index("--plot") + 1].split(",")); ta, tb = takeOf(A + T0), takeOf(B + T0)
    Wd, Hd = 1800, 420; img = np.full((Hd, Wd, 3), 20, np.uint8); X = lambda t: int((t - ta) / (tb - ta) * Wd); Y = lambda d: int(Hd - 40 - (d - floor + 5) / (db.max() - floor + 10) * (Hd - 120))
    a0, a1 = at(ta), at(tb); pts = np.array([[X(tt[i]), Y(db[i])] for i in range(a0, a1)], np.int32); cv2.polylines(img, [pts], False, (200, 200, 200), 1)
    for j, w in enumerate(WORDS):
        if rec[j][1] < ta or rec[j][0] > tb: continue
        x0, x1 = X(rec[j][0]), X(rec[j][1]); cv2.rectangle(img, (x0, 30), (x1, 60), (90, 90, 230), 1); cv2.putText(img, w["text"], (x0 + 2, 25), cv2.FONT_HERSHEY_SIMPLEX, 0.45, (120, 120, 255), 1)
        x0, x1 = X(starts[j]), X(ends[j]); cv2.rectangle(img, (x0, Hd - 30), (x1, Hd - 4), (90, 230, 90), -1); cv2.putText(img, w["text"], (x0 + 2, Hd - 36), cv2.FONT_HERSHEY_SIMPLEX, 0.45, (120, 255, 120), 1)
        cv2.line(img, (x0, 60), (x0, Hd - 30), (60, 160, 60), 1)
    cv2.putText(img, f"record {A}-{B}s   top/red: words-record   bottom/green: measured on the stem   grey: dB envelope", (8, Hd - 44 - 20), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 255, 255), 1)
    dst = os.path.join(os.environ.get("PLOT_DIR", "/tmp"), f"bounds-{A:g}-{B:g}.png"); cv2.imwrite(dst, img); print(f"▸ {dst}")
if "--clip" in sys.argv:
    A, B = sys.argv[sys.argv.index("--clip") + 1].split(","); dst = os.path.expanduser(f"~/Desktop/sailor-song-lyric-{A}-{B}.mp4")
    subprocess.run(["node", os.path.join(LANE, "bin/perf-relight.mjs"), "--audio", os.path.join(OUTD, STEM + ".mp3"), "--lyric-only", "--small", "--from", A, "--to", B, "--out", dst], stderr=subprocess.DEVNULL)
    subprocess.run(["open", "-a", "QuickTime Player", dst]); print(f"▸ {dst}")
