#!/usr/bin/env python3
"""takes.py — the per-phrase take comparison (loner's take-rotation move).

All eight tagged Flower Eater performances, transcribed word-level
(whisper.cpp ggml-small.en, -ml 1 — bin/transcribe.sh writes the raw
JSONs into analysis/whisper-raw/), each song region located, phrases
grouped at >0.3 s gaps, and every canonical phrase of the primary take
matched into every other take by fuzzy text. Per matched phrase, three
measurements:

  voiced     pyin voiced coverage (vp>0.5) inside the phrase — how much
             of it is actually singable material
  grid_dev   median |cents| to the derived flwe scale in the take's own
             tonic frame (each take may sit in its own transposition:
             the take tonic is fitted per take as the histogram peak
             nearest 133.9 within +/-3 st)
  noise      HF noise floor (median rms of the quietest decile, 4-8 kHz)
             — room hiss, crowd, wind

The verdict stays with the UNBROKEN-take philosophy: the render defaults
to the primary take for every phrase (one warp, no seams); takes.json
records where another take is measurably better so a future dub pass
(loner's singdub move) knows where to look.

  ../../.venv/bin/python3 bin/takes.py
"""
import difflib, glob, json, os, re
import numpy as np
import librosa

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
ANA = os.path.join(LANE, "analysis")
RAW = os.path.join(ANA, "whisper-raw")
PRIMARY = "6992837952212569350"
SCALE = np.array(json.load(open(os.path.join(ANA, "notes.json")))["scale_semitones"])
TONIC0 = 133.9


def load_words(path):
    # whisper.cpp -ml 1 can split a multibyte char across tokens; the raw
    # JSON is then not valid UTF-8. Those tokens aren't words anyway.
    segs = json.loads(open(path, "rb").read().decode(
        "utf-8", errors="replace"))["transcription"]
    words = []
    for s in segs:
        txt = s["text"]
        if not txt.strip() or txt.strip() in ".,!?":
            continue
        t0, t1 = s["offsets"]["from"] / 1000, s["offsets"]["to"] / 1000
        if words and not txt.startswith(" "):
            words[-1]["word"] += txt
            words[-1]["end"] = t1
        else:
            words.append({"word": txt.strip(), "start": t0, "end": t1})
    return words


def phrases_of(words):
    if not words:
        return []
    groups = [[words[0]]]
    for w in words[1:]:
        if w["start"] - groups[-1][-1]["end"] > 0.3:
            groups.append([w])
        else:
            groups[-1].append(w)
    return [{"text": " ".join(w["word"] for w in g),
             "t0": g[0]["start"], "t1": g[-1]["end"]} for g in groups]


def norm(s):
    return re.sub(r"[^a-z ]", "", s.lower())


takes = {}
for path in sorted(glob.glob(os.path.join(RAW, "flwe-*.json"))):
    tid = re.search(r"flwe-(\d+)\.json", path).group(1)
    src = os.path.join(LANE, "source", f"flwe-{tid}.wav")
    if not os.path.exists(src):
        continue
    y, sr = librosa.load(src, sr=22050, mono=True)
    f0, vf, vp = librosa.pyin(y, sr=sr, fmin=60, fmax=600,
                              frame_length=2048, hop_length=256)
    t = librosa.times_like(f0, sr=sr, hop_length=256)
    # HF noise floor: quietest-decile rms in the 4-8 kHz band
    hf = librosa.stft(y, n_fft=1024, hop_length=256)
    fr = librosa.fft_frequencies(sr=sr, n_fft=1024)
    band = np.abs(hf[(fr >= 4000) & (fr <= 8000)]).mean(axis=0)
    noise = float(np.median(np.sort(band)[:max(1, len(band) // 10)]))
    # per-take tonic: histogram peak nearest 133.9 within +/-3 st
    v = f0[vf & (vp > 0.6)]
    v = v[np.isfinite(v)]
    tonic = TONIC0
    if len(v) > 200:
        st = 12.0 * np.log2(v / TONIC0)
        pc = np.mod(st, 1.0)
        # fractional-semitone offset of the take's own grid
        hist, edges = np.histogram(pc, bins=50, range=(0, 1))
        off = (edges[np.argmax(hist)] + 0.01)
        off = off - 1.0 if off > 0.5 else off
        tonic = TONIC0 * 2.0 ** (off / 12.0)
    takes[tid] = dict(words=load_words(path), phrases=phrases_of(load_words(path)),
                      f0=f0, vf=vf, vp=vp, t=t, noise=noise, tonic=tonic)

canon = takes[PRIMARY]["phrases"]
# the primary's sung region only
canon = [p for p in canon if 18.0 <= p["t0"] <= 80.0]

STEPS = np.concatenate([SCALE + 12 * o for o in range(-3, 4)]).astype(float)


def measure(take, t0, t1):
    f0, vf, vp, t = take["f0"], take["vf"], take["vp"], take["t"]
    m = (t >= t0) & (t <= t1)
    tot = int(m.sum()) or 1
    mv = m & vf & (vp > 0.5)
    v = f0[mv]
    v = v[np.isfinite(v)]
    voiced = len(v) / tot
    if len(v) < 5:
        return voiced, None
    st = 12.0 * np.log2(v / take["tonic"])
    dev = np.abs(st[:, None] - STEPS[None, :]).min(axis=1) * 100.0
    return voiced, float(np.median(dev))


out = {"primary": PRIMARY,
       "method": "see docstring; verdict defaults to the primary "
                 "(unbroken-take philosophy), better-elsewhere noted",
       "take_noise_floor": {k: round(v["noise"], 5) for k, v in takes.items()},
       "take_tonic_hz": {k: round(v["tonic"], 1) for k, v in takes.items()},
       "phrases": []}

for cp in canon:
    row = {"phrase": cp["text"], "primary_t": [cp["t0"], cp["t1"]],
           "matches": {}}
    v0, g0 = measure(takes[PRIMARY], cp["t0"], cp["t1"])
    row["matches"][PRIMARY] = dict(voiced=round(v0, 2),
                                   grid_dev=round(g0, 1) if g0 else None,
                                   noise=round(takes[PRIMARY]["noise"], 5))
    best, best_score = PRIMARY, None
    for tid, take in takes.items():
        if tid == PRIMARY:
            continue
        cand = max(take["phrases"], key=lambda p: difflib.SequenceMatcher(
            None, norm(cp["text"]), norm(p["text"])).ratio(), default=None)
        if cand is None:
            continue
        ratio = difflib.SequenceMatcher(None, norm(cp["text"]),
                                        norm(cand["text"])).ratio()
        if ratio < 0.55:
            continue
        v, g = measure(take, cand["t0"], cand["t1"])
        row["matches"][tid] = dict(text=cand["text"],
                                   t=[round(cand["t0"], 2), round(cand["t1"], 2)],
                                   match=round(ratio, 2), voiced=round(v, 2),
                                   grid_dev=round(g, 1) if g else None,
                                   noise=round(take["noise"], 5))
    # a challenger wins on paper if it is (a) clearly more voiced and no
    # further off the grid, (b) clearly cleaner without losing voicing or
    # grid, or (c) sings a phrase the primary barely voices at all
    for tid, m in row["matches"].items():
        if tid == PRIMARY or m.get("grid_dev") is None:
            continue
        n0 = row["matches"][PRIMARY]["noise"]
        if g0 is None:
            if m["voiced"] >= v0 + 0.2:                       # (c)
                row["better_elsewhere"] = tid
        elif (m["voiced"] >= v0 + 0.05 and m["grid_dev"] <= g0):   # (a)
            row["better_elsewhere"] = tid
        elif (m["noise"] < 0.7 * n0 and m["voiced"] >= v0 - 0.05
                and m["grid_dev"] <= g0 + 5):                 # (b)
            row["better_elsewhere"] = tid
    row["verdict"] = PRIMARY
    out["phrases"].append(row)

json.dump(out, open(os.path.join(ANA, "takes.json"), "w"), indent=1)
for row in out["phrases"]:
    extra = f"  << {row['better_elsewhere']} cleaner" if "better_elsewhere" in row else ""
    n = len(row["matches"]) - 1
    print(f"{row['phrase'][:44]:44s} matched in {n} other takes{extra}")
print("wrote analysis/takes.json")
