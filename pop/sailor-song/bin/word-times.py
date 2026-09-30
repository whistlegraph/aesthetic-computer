#!/usr/bin/env python3
"""word-times.py — sung-word timing as tight as the take allows.

  1. whisper.cpp on the DRY vocal with per-token timestamps
       whisper-cli -m ggml-small.bin -f dry16.wav -l en -ml 1 -sow -ojf
     (one segment per word; every token inside carries its own offset, so a
     word like "Hathaway" has three timed pieces — that is what drives the
     partial highlight).
  2. The true lyric (src/lyrics-sung.txt) is aligned onto whisper's word
     sequence by Needleman–Wunsch (pop/bin/mfa-align.mjs's move, re-done here
     so the tokens ride along): substitutions keep the slot, insertions split
     the neighbour's slot, deletions drop the slot.
  3. Every word onset is SNAPPED to her nearest vocal onset — librosa onset
     detection on the dry stem, backtracked to the attack — within ±150 ms
     (whisper's boundaries after a breath run ~250 ms early), monotonic, and
     a word never shorter than 60 ms. Its end is the next word's onset or the
     stem's energy fall, whichever comes first.
  4. Take time → record time through the lock and regularize maps minus the
     record's startSec (read from the newest events receipt).

Writes src/words-aligned.json (take time, with tokens) and src/words-record.json
(record time, with tokens) — both local (src/ is gitignored: the lyric is Gigi
Perez's). Prints a placement check: how many onsets sit within 40 ms of a
detected vocal onset, before and after.

  pop/.venv/bin/python pop/sailor-song/bin/word-times.py [--whisper-json path] [--snap-ms 150]
"""
import argparse, glob, json, os, re, subprocess, sys
import numpy as np, librosa, soundfile as sf

ap = argparse.ArgumentParser()
ap.add_argument("--whisper-json", default=None)
ap.add_argument("--shape", action="store_true", help="re-cut words on syllable bumps (off by default: bounds-study.py scored it 30/273 within 40 ms of the ear vs 61 for raw FA)")
ap.add_argument("--breath-ms", type=float, default=250, help="quiet this long ends a word before the next one; shorter dips belong to the word (the ear tiles words edge to edge)")
ap.add_argument("--fa", action="store_true", help="MMS forced alignment (bin/forced-align.py → .word-times/forced-align.json): the known lyric placed on her audio")
ap.add_argument("--openai", action="store_true", help="OpenAI whisper-1 word timestamps, primed with the lyric (loner's align.py move)")
ap.add_argument("--snap-ms", type=float, default=40, help="snap an onset to a detected attack this close (40 scored best against the hand bounds; 150 pulled onsets onto the wrong attack)")
ap.add_argument("--model", default=os.path.expanduser("~/.whisper-models/ggml-small.bin"))
a = ap.parse_args()
LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(LANE, "src"); VOX = os.path.join(SRC, "vox")
DRY = os.path.join(VOX, "vocals-dry-48k.wav")
TMP = os.path.join(SRC, ".word-times"); os.makedirs(TMP, exist_ok=True)

# ── 1. whisper with token times ──────────────────────────────────────────
wj = a.whisper_json
if not wj and not a.openai and not a.fa:
    wav16 = os.path.join(TMP, "dry16.wav")
    subprocess.run(["ffmpeg", "-v", "error", "-y", "-i", DRY, "-ar", "16000", "-ac", "1", wav16], check=True)
    # no -sns: with non-speech suppression the model drops her first line; the bracket tags are filtered below
    subprocess.run(["whisper-cli", "-m", a.model, "-f", wav16, "-l", "en", "-ml", "1", "-sow", "-ojf", "-of", os.path.join(TMP, "dry16")],
                   check=True, capture_output=True)
    wj = os.path.join(TMP, "dry16.json")
def openai_words():
    """whisper-1 on the dry stem with word timestamps, the true lyric as the prompt.
    whisper.cpp -ml 1 returns SUB-WORD tokens (loner: "curled" → "cur" + "led") and every
    label after a split slides by a syllable; whisper-1 returns whole words."""
    key = os.environ.get("OPENAI_API_KEY")
    env = os.path.join(LANE, "..", "..", "aesthetic-computer-vault", ".devcontainer", "envs", "devcontainer.env")
    if not key and os.path.exists(env):
        for line in open(env):
            if line.startswith("OPENAI_API_KEY="): key = line.split("=", 1)[1].strip().strip('"')
    if not key: sys.exit("no OPENAI_API_KEY (env or vault)")
    cache = os.path.join(TMP, "openai-whisper1.json")
    if not os.path.exists(cache):
        mp3 = os.path.join(TMP, "dry16.mp3")
        subprocess.run(["ffmpeg", "-v", "error", "-y", "-i", DRY, "-ar", "16000", "-ac", "1", "-b:a", "64k", mp3], check=True)
        lyric = " ".join(l.strip() for l in open(os.path.join(SRC, "lyrics-sung.txt")) if l.strip() and not l.startswith("["))
        r = subprocess.run(["curl", "-s", "https://api.openai.com/v1/audio/transcriptions", "-H", f"Authorization: Bearer {key}",
                            "-F", f"file=@{mp3}", "-F", "model=whisper-1", "-F", "language=en", "-F", "response_format=verbose_json",
                            "-F", f"prompt={lyric[:800]}", "-F", "timestamp_granularities[]=word"], capture_output=True, text=True, check=True)
        d = json.loads(r.stdout)
        if "words" not in d: sys.exit(f"whisper-1 failed: {r.stdout[:300]}")
        json.dump(d, open(cache, "w"), indent=1)
    d = json.load(open(cache))
    return [{"text": w["word"], "from": w["start"], "to": max(w["end"], w["start"] + 0.06),
             "tokens": [{"text": w["word"], "from": w["start"], "to": max(w["end"], w["start"] + 0.06)}]} for w in d["words"]]

hyp = []
W = [] if (a.openai or a.fa) else json.load(open(wj))["transcription"]
if a.openai: hyp = openai_words()
if a.fa: hyp = [{k: w[k] for k in ("text", "from", "to", "tokens")} for w in json.load(open(os.path.join(TMP, "forced-align.json")))]
for seg in W:
    txt = seg["text"].strip()
    if not txt or txt.startswith("[") or txt.startswith("(") or txt in ("]", ")") or txt.lower() in ("silence", "music"): continue
    if seg["offsets"]["to"] - seg["offsets"]["from"] > 3000: continue      # no sung word lasts three seconds
    toks = [t for t in seg["tokens"] if not t["text"].startswith("[") and t["text"].strip()]
    if not toks: continue
    hyp.append({"text": txt, "from": seg["offsets"]["from"] / 1000, "to": seg["offsets"]["to"] / 1000,
                "tokens": [{"text": t["text"].strip(), "from": t["offsets"]["from"] / 1000, "to": t["offsets"]["to"] / 1000} for t in toks]})
# drop whisper words that sit in silence on the dry stem (hallucinations over the guitar-only bars)
_y, _sr = librosa.load(DRY, sr=22050, mono=True)
_rms = librosa.feature.rms(y=_y, frame_length=1024, hop_length=256)[0]; _rt = librosa.frames_to_time(np.arange(len(_rms)), sr=_sr, hop_length=256)
_floor = np.percentile(_rms[_rms > 0], 20)
def _loud(w): a0, a1 = np.searchsorted(_rt, w["from"]), np.searchsorted(_rt, w["to"] + 0.05); return len(_rms[a0:a1]) and _rms[a0:a1].max() > _floor * 2.5
nraw = len(hyp); hyp = [w for w in hyp if _loud(w)]
print(f"whisper: {nraw} words, {len(hyp)} on voiced audio")

# ── 2. align the true lyric onto whisper's words ─────────────────────────
lines = [l.strip() for l in open(os.path.join(SRC, "lyrics-sung.txt")) if l.strip() and not l.startswith("[")]
ref = [w for l in lines for w in l.split()]
norm = lambda s: re.sub(r"[^a-z0-9']", "", s.lower())
def sim(x, y):
    x, y = norm(x), norm(y)
    if x == y: return 2.0
    if not x or not y: return -1.0
    # crude phonetic tolerance: shared prefix / edit distance
    d = librosa.sequence.dtw if False else None
    la, lb = len(x), len(y); dp = list(range(lb + 1))
    for i in range(1, la + 1):
        prev, dp[0] = dp[0], i
        for j in range(1, lb + 1):
            cur = dp[j]; dp[j] = min(dp[j] + 1, dp[j - 1] + 1, prev + (x[i - 1] != y[j - 1])); prev = cur
    return 1.5 - 2.5 * dp[lb] / max(la, lb)
n, m = len(ref), len(hyp)
S = np.zeros((n + 1, m + 1)); S[:, 0] = -np.arange(n + 1) * 0.8; S[0, :] = -np.arange(m + 1) * 0.8
for i in range(1, n + 1):
    for j in range(1, m + 1):
        S[i, j] = max(S[i - 1, j - 1] + sim(ref[i - 1], hyp[j - 1]["text"]), S[i - 1, j] - 0.8, S[i, j - 1] - 0.8)
i, j, pairs = n, m, []
while i > 0 or j > 0:
    if i > 0 and j > 0 and abs(S[i, j] - (S[i - 1, j - 1] + sim(ref[i - 1], hyp[j - 1]["text"]))) < 1e-9: pairs.append((i - 1, j - 1)); i -= 1; j -= 1
    elif i > 0 and abs(S[i, j] - (S[i - 1, j] - 0.8)) < 1e-9: pairs.append((i - 1, None)); i -= 1
    else: j -= 1
pairs.reverse()
words = []
exact = sum(1 for ri, hj in pairs if hj is not None and norm(ref[ri]) == norm(hyp[hj]["text"]))
for ri, hj in pairs:
    if hj is None: words.append({"text": ref[ri], "from": None, "to": None, "tokens": []})
    else: h = hyp[hj]; words.append({"text": ref[ri], "from": h["from"], "to": h["to"], "tokens": h["tokens"]})
# insertions: split the neighbour's slot evenly
k = 0
while k < len(words):
    if words[k]["from"] is None:
        j = k
        while j < len(words) and words[j]["from"] is None: j += 1
        prev = words[k - 1] if k > 0 else None; nxt = words[j] if j < len(words) else None
        a0 = prev["from"] if prev else (nxt["from"] - 0.3); a1 = nxt["from"] if nxt else (prev["to"] + 0.3 * (j - k))
        cnt = (j - k) + (1 if prev else 0); step = (a1 - a0) / cnt
        if prev: prev["to"] = a0 + step
        for q in range(k, j): words[q]["from"] = a0 + step * (q - k + (1 if prev else 0)); words[q]["to"] = words[q]["from"] + step; words[q]["tokens"] = []
        k = j
    else: k += 1
print(f"aligned: {len(words)} lyric words · {exact} exact · {sum(1 for w in words if not w['tokens'])} interpolated")

# ── 3. snap onsets to her vocal onsets ───────────────────────────────────
y, sr = librosa.load(DRY, sr=22050, mono=True)
env = librosa.onset.onset_strength(y=y, sr=sr, hop_length=256)
on = librosa.onset.onset_detect(onset_envelope=env, sr=sr, hop_length=256, backtrack=True, units="time", delta=0.05)
rms = librosa.feature.rms(y=y, frame_length=1024, hop_length=256)[0]; rt = librosa.frames_to_time(np.arange(len(rms)), sr=sr, hop_length=256)
floor = np.percentile(rms[rms > 0], 20)
def energy_at(t): return float(np.interp(t, rt, rms))
def near_onset(t):
    d = np.abs(on - t); k = int(np.argmin(d)); return float(on[k]), float(d[k])
before = sum(1 for w in words if near_onset(w["from"])[1] <= 0.04)
snap = a.snap_ms / 1000
# inside a continuous phrase there is no energy onset to snap to: those words snap to her
# tuned-note onsets instead (vox-notes.json, take time) — a word change is a pitch change
NOTE_T = np.array(sorted(n["t"] for n in json.load(open(os.path.join(LANE, "vox-notes.json")))["notes"]))
def near_note(t): d = np.abs(NOTE_T - t); k = int(np.argmin(d)); return float(NOTE_T[k]), float(d[k])
n_energy = n_note = 0
for i, w in enumerate(words):
    o, d = near_onset(w["from"])
    if d <= 0.04: n_energy += 1
    elif d <= snap and energy_at(w["from"] - 0.06) < floor * 1.5: n_energy += 1        # a real onset out of a gap
    else:
        o2, d2 = near_note(w["from"])
        if d2 <= 0.12: o, d = o2, d2; n_note += 1
    if d <= snap:
        shift = o - w["from"]; w["from"] = o
        for tk in w["tokens"]: tk["from"] += shift; tk["to"] += shift
print(f"snapped: {n_energy} words to a vocal onset, {n_note} to a tuned-note onset")
# hand edits (src/word-edits.json): the ear wins over the tracker — a word's onset pinned to a take time
edits_path = os.path.join(SRC, "word-edits.json")
if os.path.exists(edits_path):
    for e in json.load(open(edits_path))["edits"]:
        cands = [w for w in words if w["text"] == e["text"] and abs(w["from"] - e["near"]) < 0.4]
        if not cands: print(f"  ! edit not matched: {e}"); continue
        w = min(cands, key=lambda w: abs(w["from"] - e["near"])); shift = e["from"] - w["from"]; w["from"] = e["from"]
        for tk in w["tokens"]: tk["from"] += shift; tk["to"] += shift
        print(f"  edit: {e['text']!r} → {e['from']:.3f} ({e.get('why', '')})")
# monotonic + minimum length + ends
for i, w in enumerate(words):
    if i > 0 and w["from"] < words[i - 1]["from"] + 0.06: w["from"] = words[i - 1]["from"] + 0.06
for i, w in enumerate(words):
    nxt = words[i + 1]["from"] if i + 1 < len(words) else w["from"] + 1.0
    # v19: EVERY word runs on while her voice does — aligners end sung vowels early (the blocks
    # stopped short of the waveform). From its aligned end, it continues until the stem has sat
    # under the floor for a breath (--breath-ms), or until the next word starts, whichever is
    # first. bounds-study.py on the hand pass: 267 of 268 word pairs butt together (gap ≤ 15 ms);
    # the one real gap is > 350 ms. A dip shorter than a breath is inside the word.
    breath_s = a.breath_ms / 1000
    t = max(w["to"] or w["from"], w["from"] + 0.08); quiet = 0.0
    while t < nxt - 0.01 and quiet < breath_s:
        quiet = quiet + 0.01 if energy_at(t) < floor * 1.2 else 0.0; t += 0.01
    end = min(t - quiet, nxt - 0.01)
    w["to"] = max(w["from"] + 0.06, end)
    if w["tokens"]:
        for tk in w["tokens"]: tk["from"] = min(max(tk["from"], w["from"]), w["to"]); tk["to"] = min(max(tk["to"], tk["from"]), w["to"])
        w["tokens"][-1]["to"] = w["to"]
# ── 3b. re-bound by SHAPE: syllable bumps in her energy, dealt out to the words ───────
# v19: the aligner's edges sit inside a word's sound, so a fast short word ("to", "the",
# "you'd") could swallow its neighbour's bump. Here every phrase (words closer than 0.3 s)
# is re-cut on her own envelope: find the syllable bumps (peaks of the 10 ms RMS with at
# least 2.5 dB of dip on each side), estimate each word's syllables from its spelling, and
# deal the bumps out in order by DP (cost: onset distance to the aligned time + syllable
# mismatch). A word then runs from the dip before its first bump to the dip after its last.
if a.shape:
    hop10 = int(sr * 0.01)
    e10 = librosa.feature.rms(y=y, frame_length=hop10 * 3, hop_length=hop10)[0]
    e10 = np.convolve(20 * np.log10(e10 + 1e-6), np.ones(3) / 3, "same"); t10 = np.arange(len(e10)) * 0.01
    def syllables(word):
        w = re.sub(r"[^a-z]", "", word.lower())
        if not w: return 1
        n = len(re.findall(r"[aeiouy]+", w))
        if w.endswith("e") and not w.endswith(("le", "ee", "ye")) and n > 1: n -= 1
        return max(1, n)
    def bumps_in(t0, t1):
        i0, i1 = max(1, int(t0 / 0.01)), min(len(e10) - 1, int(t1 / 0.01))
        seg = e10[i0:i1]
        if len(seg) < 5: return []
        loud = seg.max() - 24
        pk = [k for k in range(1, len(seg) - 1) if seg[k] >= seg[k - 1] and seg[k] > seg[k + 1] and seg[k] > loud]
        out = []                                  # keep peaks with 2.5 dB of dip between them
        for k in pk:
            if out and (k - out[-1] < 7 or seg[out[-1]:k + 1].min() > min(seg[out[-1]], seg[k]) - 2.5):
                if seg[k] > seg[out[-1]]: out[-1] = k
                continue
            out.append(k)
        return [(i0 + k) * 0.01 for k in out]
    def dip(ta, tb):
        i0, i1 = int(ta / 0.01), max(int(ta / 0.01) + 1, int(tb / 0.01))
        return (i0 + int(np.argmin(e10[i0:i1]))) * 0.01
    # one alignment for the whole song: every syllable bump in her voice against every lyric
    # word, in order. A word takes 1..6 consecutive bumps with no breath (> 0.35 s of quiet)
    # inside; a bump that belongs to no word (an ad-lib, a hum, bleed) may be skipped at a
    # cost. The aligned time only NUDGES (capped at 1 s), so a line the aligner packed into
    # silence cannot drag its words there.
    voiced = e10 > np.percentile(e10, 35)
    B = [t for t in bumps_in(0.05, t10[-1] - 0.05) if voiced[int(t / 0.01)]]
    def breath(k, j):          # quiet gap between bumps k and j-1 (inclusive)?
        return any(dip_gap(B[q], B[q + 1]) for q in range(k, j - 1))
    def dip_gap(ta, tb):
        seg = voiced[int(ta / 0.01):int(tb / 0.01)]; run = best = 0
        for v in seg: run = 0 if v else run + 1; best = max(best, run)
        return best * 0.01 > 0.35
    n, m, KMAX, SKIP = len(words), len(B), 6, 0.9
    syl = [syllables(w["text"]) for w in words]
    INF = 1e18; D = np.full((n + 1, m + 1), INF); P = np.full((n + 1, m + 1), -1, int); D[0, :] = np.arange(m + 1) * SKIP
    gaps = np.array([dip_gap(B[q], B[q + 1]) for q in range(m - 1)] + [False])
    for i in range(1, n + 1):
        w = words[i - 1]
        for j in range(1, m + 1):
            best, arg = D[i, j - 1] + SKIP, -2              # skip bump j-1
            for k in range(max(0, j - KMAX), j):            # word i takes bumps k .. j-1
                if D[i - 1, k] >= INF or gaps[k:j - 1].any(): continue
                c = D[i - 1, k] + min(1.0, abs(B[k] - w["from"])) * 2.0 + abs((j - k) - syl[i - 1]) * 0.7
                if c < best: best, arg = c, k
            D[i, j], P[i, j] = best, arg
    take, i, j = [None] * n, n, int(np.argmin(D[n]))
    while i > 0 and j > 0:
        k = P[i, j]
        if k == -2: j -= 1; continue
        take[i - 1] = (k, j); i -= 1; j = k
    moved = 0
    for q, w in enumerate(words):
        if take[q] is None: continue
        k, j = take[q]
        start = dip(B[k - 1], B[k]) if k > 0 and not gaps[k - 1] else B[k] - 0.05
        end = dip(B[j - 1], B[j]) if j < m and not gaps[j - 1] else B[j - 1] + 0.12
        while end < t10[-1] - 0.02 and voiced[int(end / 0.01)] and (j >= m or end < B[j] - 0.03): end += 0.01   # ride the tail out
        if abs(start - w["from"]) > 0.02: moved += 1
        w["from"], w["to"] = round(start, 3), round(max(end, start + 0.06), 3)
        for tk in w["tokens"]: tk["from"], tk["to"] = w["from"], w["to"]
    for q in range(1, n):      # neighbours never overlap
        if words[q]["from"] < words[q - 1]["to"]: words[q - 1]["to"] = words[q]["from"]
    print(f"shape: {m} syllable bumps dealt to {n} words · {sum(t is None for t in take)} unplaced · {moved} starts moved")

# hand pins win over the shape pass too (it re-deals every word)
if os.path.exists(EDITS := os.path.join(SRC, "word-edits.json")):
    for e in json.load(open(EDITS))["edits"]:
        c = [q for q, w in enumerate(words) if w["text"] == e["text"] and abs(w["from"] - e["near"]) < 1.5]
        if not c: continue
        q = min(c, key=lambda q: abs(words[q]["from"] - e["near"])); w = words[q]
        w["from"] = e["from"]; w["to"] = max(w["to"], e["from"] + 0.06)
        if q: words[q - 1]["to"] = min(words[q - 1]["to"], e["from"])
        for tk in w["tokens"]: tk["from"], tk["to"] = w["from"], w["to"]
        print(f"  pin after shape: {e['text']!r} → {e['from']:.3f}")
# bounds dragged by hand in SyllaWizard (bin/sylla-collect.py → word-bounds.json) win over everything
if os.path.exists(BND := os.path.join(SRC, "word-bounds.json")):
    hand = json.load(open(BND))["words"]
    for b in hand:
        if b["i"] < len(words) and words[b["i"]]["text"] == b["text"]:
            w = words[b["i"]]; w["from"], w["to"] = b["from"], b["to"]
            for tk in w["tokens"]: tk["from"], tk["to"] = w["from"], w["to"]
    print(f"  hand bounds: {len(hand)} words from SyllaWizard")
after = sum(1 for w in words if near_onset(w["from"])[1] <= 0.04)
print(f"onsets within 40 ms of a vocal onset: {before}/{len(words)} → {after}/{len(words)}")

# ── 4. take → record ─────────────────────────────────────────────────────
def mapf(path):
    mm = np.array([[float(v) / 48000 for v in l.split()] for l in open(path).read().strip().split("\n")])
    return lambda t: float(np.interp(t, mm[:, 0], mm[:, 1]))
RAW = os.environ.get("SAILOR_CLOCK") == "raw"
lock = (lambda t: t) if RAW else mapf(os.path.join(VOX, "locked/timemap.txt")); reg = (lambda t: t) if RAW else mapf(os.path.join(VOX, "reg/timemap.txt"))
receipts = sorted(glob.glob(os.path.join(LANE, "out/sailor-song-v*.events.json")), key=os.path.getmtime)
start = json.load(open(receipts[-1]))["startSec"] if receipts else 0.0
segmap = os.path.join(VOX, "cut/segmap.txt")
segs = [tuple(map(float, l.split())) for l in open(segmap).read().strip().split("\n")] if os.path.exists(segmap) and not RAW else []
def cut_of(t):
    if not segs: return t
    # a word whose onset IS a seam (the splice cuts on that onset) belongs to the segment
    # that plays it, not to the one that ends there — hence the 5 ms guard on the end
    for a0, b0, o in segs:
        if a0 <= t < b0 - 0.005: return t - a0 + o
    return None
def rec(t):
    c = cut_of(reg(lock(t))); return None if c is None else c - start
def rec_end(t0, t1):
    """a word's end, kept inside the segment its start is in (a word can straddle a seam)"""
    r0 = reg(lock(t0)); r1 = reg(lock(t1))
    for a0, b0, o in segs:
        if a0 <= r0 < b0 - 0.005: return min(r1, b0 - 0.01) - a0 + o - start if r1 >= a0 else r0 - a0 + o - start + 0.06
    return None if not segs else None if cut_of(r1) is None else cut_of(r1) - start
out_take = [{"text": w["text"], "fromMs": round(w["from"] * 1000), "toMs": round(w["to"] * 1000),
             "tokens": [{"text": t["text"], "fromMs": round(t["from"] * 1000), "toMs": round(t["to"] * 1000)} for t in w["tokens"]]} for w in words]
out_rec = [{"text": w["text"], "fromMs": round(rec(w["from"]) * 1000), "toMs": round((rec_end(w["from"], w["to"]) if segs else (rec(w["to"]) if rec(w["to"]) is not None else rec(w["from"]) + 0.06)) * 1000),
            "tokens": [{"text": t["text"], "fromMs": round(rec(t["from"]) * 1000), "toMs": round((rec(t["to"]) if rec(t["to"]) is not None else rec(t["from"])) * 1000)}
                       for t in w["tokens"] if rec(t["from"]) is not None]} for w in words if rec(w["from"]) is not None]
# v20: sung sound past the words — a vowel held over a seam (extend that word), a fragment re-sung
# over the screw (a new word, appended AFTER the lyric so the per-line walk in the video stays true)
extras_path = os.path.join(VOX, "cut/voice-extras.json")
if segs and os.path.exists(extras_path):
    extras = json.load(open(extras_path))["extras"]
    for e in [e for e in extras if e.get("mute")]:            # words under a held voice are not sung
        f_ms, t_ms = round((e["from"] - start) * 1000), round((e["to"] - start) * 1000)
        gone = [w for w in out_rec if f_ms - 5 <= w["fromMs"] < t_ms - 5]
        out_rec[:] = [w for w in out_rec if w not in gone]
        if gone: print(f"  muted: {' '.join(w['text'] for w in gone)} ({f_ms/1000:.2f}–{t_ms/1000:.2f}s)")
    extras = [e for e in extras if not e.get("mute")]
    for k, e in enumerate(extras):
        f_ms, t_ms = round((e["from"] - start) * 1000), round((e["to"] - start) * 1000)
        if k + 1 < len(extras): t_ms = min(t_ms, round((extras[k + 1]["from"] - start) * 1000))   # a hold yields to the next re-attack
        if e.get("extend"):
            cands = [w for w in out_rec if (e["text"] is None or norm(w["text"]) == norm(e["text"])) and abs(w["toMs"] - f_ms) <= 30]
            if cands:
                w = cands[0]; w["toMs"] = t_ms
                if w["tokens"]: w["tokens"][-1]["toMs"] = t_ms
                print(f"  extra: {w['text']!r} held to {t_ms/1000:.2f}s")
        else:
            out_rec.append({"text": e["text"], "fromMs": f_ms, "toMs": t_ms, "tokens": [{"text": e["text"], "fromMs": f_ms, "toMs": t_ms}]})
            print(f"  extra: {e['text']!r} re-sung at {f_ms/1000:.2f}–{t_ms/1000:.2f}s")
# (kept in lyric order: the video tools slice words per lyric line, then sort the lines by time)
if segs: print(f"arrangement: {len(segs)} segments, {len(words) - len(out_rec)} words outside them dropped")
json.dump(out_take, open(os.path.join(SRC, "words-aligned.json"), "w"), indent=1)
json.dump(out_rec, open(os.path.join(SRC, "words-record.json"), "w"), indent=1)
print(f"✓ {len(words)} words · record start {start:.3f}s (from {os.path.basename(receipts[-1]) if receipts else '-'}) · first word {out_rec[0]['fromMs']/1000:.2f}s · last ends {out_rec[-1]['toMs']/1000:.2f}s")
