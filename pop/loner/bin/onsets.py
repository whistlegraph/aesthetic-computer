# onsets.py — word boundaries for the lead renders, from the corpus.
#
# @jeffrey on v3: "we need to check her word boundaries, get the words
# nicely rhythmic / on rhythm" — and "we need better word boundaries and
# to know what the words are".
#
# v3.1 makes analysis/corpus.json (fresh whisper + flux refinement on
# every source take) the single source of truth. For each lead slice
# this script:
#
#   1. pulls the corpus words that fall inside the slice's span of its
#      source take (REAL words — "curled" is one word, not "cur"+"led"),
#   2. converts them to slice-relative stamps (relative to the first
#      word's onset, which is where bin/slice.mjs's dress trim put
#      sample zero),
#   3. re-detects each onset ON THE WORLD RENDER the score actually
#      plays (vox3/<name>.wav) — same 10 ms-RMS flux method as the
#      corpus, ±80 ms window — because the aesthetivox can move an
#      attack a hair,
#   4. flags anything that moved > 30 ms, and writes vox3/.onsets.json,
#      the timing receipt render3.mjs schedules by.
#
#   pop/.venv/bin/python pop/loner/bin/onsets.py

import json, os
import numpy as np
import soundfile as sf

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
VOX3 = os.path.join(LANE, "vox3")
ANA = os.path.join(LANE, "analysis")

HOP, WIN, SEARCH, RISE_MS, MIN_DB = 0.0025, 0.010, 0.080, 0.015, 1.5

# lead slice → (source post, [t0, t1] in the take) — from bin/slice.mjs
SLICES = {
    "f-sitting-curled":    ("7108062006980201771", 0.28, 5.80),
    "f-think-stone":       ("7108062006980201771", 5.70, 11.35),
    "f-of-a-stone":        ("7108062006980201771", 7.40, 11.35),
    "f-waiting-patiently": ("7108062006980201771", 11.35, 18.85),
    "f-for-time-to-pass":  ("7108062006980201771", 18.78, 25.20),
    "n-getting-curled":    ("7021262898479549702", 3.08, 8.40),
    "n-stone-waiting":     ("7021262898479549702", 9.10, 17.40),
    "n-of-a-stone":        ("7021262898479549702", 9.10, 11.25),
    "n-for-time-to-pass":  ("7021262898479549702", 18.82, 22.40),
}

corpus = json.load(open(os.path.join(ANA, "corpus.json")))

report, flagged, worst = {}, 0, (0.0, "", "")
for name, (vid, t0, t1) in SLICES.items():
    rows = [r for r in corpus[vid]["words"]
            if r["onset"] >= t0 - 0.05 and r["onset"] <= t1 + 0.05 and r["lyric"]]
    if not rows:
        print(f"  ! {name}: no corpus words in span")
        continue
    first = rows[0]["onset"]

    path = os.path.join(VOX3, f"{name}.wav")
    x, fs = sf.read(path, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)
    hop, win = int(HOP * fs), int(WIN * fs)
    n = (len(x) - win) // hop
    env = np.sqrt(np.array([np.mean(x[i * hop:i * hop + win] ** 2)
                            for i in range(n)]) + 1e-12)
    db = 20 * np.log10(env)
    lag = max(1, int(RISE_MS / HOP))
    rise = np.concatenate([np.zeros(lag), np.maximum(0, db[lag:] - db[:-lag])])

    out = []
    for r in rows:
        stamp = r["onset"] - first + 0.004
        a = max(0, int((stamp - SEARCH) / HOP))
        b = min(n - 1, int((stamp + SEARCH) / HOP))
        k = a + int(np.argmax(rise[a:b + 1])) if b > a else a
        onset = k * HOP if b > a and rise[k] >= MIN_DB else stamp
        dev = (onset - stamp) * 1000.0
        flag = abs(dev) > 30.0
        flagged += flag
        if abs(dev) > abs(worst[0]):
            worst = (dev, name, r["lyric"])
        out.append(dict(w=r["lyric"], stamp=round(stamp, 4), onset=round(onset, 4),
                        end=round(min(r["end"] - first + 0.004, len(x) / fs), 4),
                        dev_ms=round(dev, 1), flag=flag,
                        f0_hz=r["f0_hz"], note=r["note"], grade=r["grade"]))
    report[name] = dict(len=round(len(x) / fs, 3), words=out)
    devs = " ".join(f"{w['w']}{'*' if w['flag'] else ''}:{w['dev_ms']:+.0f}" for w in out)
    print(f"  {name:22s} {devs}")

json.dump(report, open(os.path.join(VOX3, ".onsets.json"), "w"), indent=1)
print(f"WROTE {VOX3}/.onsets.json — {flagged} flagged (>30 ms), "
      f"worst {worst[0]:+.0f} ms ({worst[1]} '{worst[2]}')")
