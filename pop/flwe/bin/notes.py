#!/usr/bin/env python3
"""notes.py — FIGURE OUT THE NOTES.

Derives the flwe scale FROM THE DATA (the directive: don't assume natural
minor) and charts every sung word onto the 110.3 BPM clickvox grid.

The scale: a frame-level pyin histogram over the sung region, folded into
pitch classes in the take's own tonic frame (133.9 Hz = C3 +40 cents).
A degree joins the scale if it carries >= 7% of the confidently-voiced
frame mass — the threshold sits in the histogram's own largest gap
(7.6% -> 5.6%). What falls out is NOT natural minor: it is a hand-drawn
8-degree set — a chromatic lower cluster (1 b2 2 b3), an empty middle
(3, 4, b5, b6 all under 6%), the dominant (5), and a chromatic upper
cluster (6 b7 7). That is the snap grid the aesthetivox regulates to.

Per word: the pitch is RE-MEASURED here, not read off melody.json — its
whole-word medians blur a scooped onset into the note (it called "that"
D#2 when the sustain sits near A2, and pulled the final "eater" down to
B-land when the note rises through B3 and resolves at C4). Each word's
f0 is the median of the LAST 60% of its confidently-voiced pyin frames —
the sustain, past the scoop. Target = nearest scale tone in the tonic
frame, any octave. One pin: the closing "eater" goes to 12 st (C4) — the
5 -> 1 cadence the README hears; its sustain median (11.3) sits on the
B3/C4 border only because the approach is a rise. Per phrase (clickvox.json's grid groups): word
onsets are distributed derive_units-style — the phrase span scales to a
whole number of beats, each word's duration quantizes to 8ths (min a
half-beat) — so onsets land on-grid by construction and the chart is a
score, not a transcription.

Writes analysis/notes.json (the word-level receipt) and
analysis/melody-chart.json (the general melody chart — what the
aesthetivox renders and any future arrangement reads).

  ../../.venv/bin/python3 bin/notes.py
"""
import json, os
import numpy as np
import librosa

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
ANA = os.path.join(LANE, "analysis")
SRC = os.path.join(LANE, "source", "flwe-6992837952212569350.wav")

TONIC = 133.9                    # C3 +40 cents — the take's own frame
BPM = 110.3
BEAT = 60.0 / BPM
MASS_THRESHOLD = 0.07            # the histogram's own largest gap

NOTE_NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
DEGREE_NAMES = ["1", "b2", "2", "b3", "3", "4", "b5", "5", "b6", "6", "b7", "7"]


def st_to_name(st):
    """Note name of `st` semitones from tonic, in the tonic frame
    (where C3 means 133.9 Hz, +40c sharp of concert)."""
    k = int(round(st))
    return f"{NOTE_NAMES[k % 12]}{3 + (k + 48) // 12 - 4}"


# ── the scale, from the frames ────────────────────────────────────────
y, sr = librosa.load(SRC, sr=22050, mono=True)
song = y[int(18.0 * sr):int(80.0 * sr)]
f0, vf, vp = librosa.pyin(song, sr=sr, fmin=70, fmax=500,
                          frame_length=2048, hop_length=256)
v = f0[vf & (vp > 0.6)]
v = v[np.isfinite(v)]
pc = np.mod(1200.0 * np.log2(v / TONIC), 1200.0)
mass = np.zeros(12)
for d in range(12):
    lo = (d * 100 - 50) % 1200
    dev = np.mod(pc - d * 100 + 600, 1200) - 600
    mass[d] = np.sum(np.abs(dev) < 50)
mass /= mass.sum()
SCALE = [d for d in range(12) if mass[d] >= MASS_THRESHOLD]

# ── per-word targets (re-measured: sustain median, not whole-word) ────
melody = json.load(open(os.path.join(ANA, "melody.json")))
click = json.load(open(os.path.join(ANA, "clickvox.json")))
sung = [w for w in melody["words"] if w.get("sung")]

# one pyin pass over the whole take, wide enough for his real range
f0w, vfw, vpw = librosa.pyin(y, sr=sr, fmin=60, fmax=600,
                             frame_length=2048, hop_length=256)
tw = librosa.times_like(f0w, sr=sr, hop_length=256)

PINS = {("eater", 78.44): 12.0}   # the 5 -> 1 cadence; see docstring

steps = np.array([s + 12 * o for o in range(-3, 4) for s in SCALE], float)


def word_hz(w):
    """Median of the last 60% of the word's confidently-voiced frames."""
    m = (tw >= w["start"]) & (tw <= max(w["end"], w["start"] + 0.08)) \
        & vfw & (vpw > 0.5)
    v = f0w[m]
    v = v[np.isfinite(v)]
    if len(v) < 3:
        return w.get("hz")            # melody.json's median as fallback
    return float(np.median(v[int(len(v) * 0.4):]))


ANCHOR = click["anchor_sec"]
notes = []
for w in sung:
    entry = {
        "word": w["word"], "start": w["start"], "end": w["end"],
        "beat_measured": round((w["start"] - ANCHOR) / BEAT, 2),
        "dur_beats_measured": round((w["end"] - w["start"]) / BEAT, 2),
    }
    hz = word_hz(w)
    if hz:
        st = 12.0 * np.log2(hz / TONIC)
        tgt = PINS.get((w["word"], w["start"]),
                       float(steps[np.argmin(np.abs(steps - st))]))
        entry.update(
            hz_measured=round(hz, 1),
            st_measured=round(st, 2),
            st_target=tgt,
            note=st_to_name(tgt),
            degree=DEGREE_NAMES[int(tgt) % 12],
            cents_from_tonic=int(round(tgt * 100)),
            cents_err=int(round((st - tgt) * 100)),
            pinned=(w["word"], w["start"]) in PINS or None,
        )
        entry = {k: v for k, v in entry.items() if v is not None}
    notes.append(entry)

# ── the chart: phrases onto the grid, derive_units-style ──────────────
chart = []
for p in click["phrases"]:
    ws = [n for n in notes if p["t0"] - 0.01 <= n["start"] < p["t1"]]
    if not ws:
        continue
    span = ws[-1]["end"] - ws[0]["start"]
    beats_total = max(1, round(span / BEAT))
    k = beats_total * BEAT / span
    acc, units = 0.0, []
    for i, w in enumerate(ws):
        end = ws[i + 1]["start"] if i + 1 < len(ws) else w["end"]
        d = (end - w["start"]) * k / BEAT
        dq = max(0.5, round(d * 2) / 2.0)
        u = {"word": w["word"], "beat": round(acc, 2), "dur_beats": dq,
             "src": [w["start"], w["end"]]}
        for key in ("note", "degree", "st_target", "cents_from_tonic",
                    "hz_measured", "cents_err"):
            if key in w:
                u[key] = w[key]
        units.append(u)
        acc += dq
    chart.append({
        "phrase": " ".join(w["word"] for w in ws),
        "beat": p["beat"],
        "beats_total": round(acc, 2),
        "t0": p["t0"], "t1": p["t1"],
        "words": units,
    })

receipt = {
    "source": os.path.basename(SRC),
    "tonic_hz": TONIC,
    "tonic_note": "C3 +40c",
    "bpm": BPM,
    "method": ("frame-level pyin (vp>0.6) over 18-80s folded to pitch "
               "classes in the 133.9 Hz frame; scale = degrees with >= 7% "
               "of voiced mass (the histogram's largest gap); word targets "
               "= nearest scale tone to melody.json's per-word median"),
    "degree_mass": {DEGREE_NAMES[d]: round(float(mass[d]), 3) for d in range(12)},
    "scale_semitones": SCALE,
    "scale_degrees": [DEGREE_NAMES[d] for d in SCALE],
    "scale_note": ("NOT natural minor: chromatic lower cluster (1 b2 2 b3), "
                   "empty middle (3 4 b5 b6 all under 6%), dominant 5, "
                   "chromatic upper cluster (6 b7 7)"),
    "voiced_frames": int(len(v)),
    "words": notes,
}
json.dump(receipt, open(os.path.join(ANA, "notes.json"), "w"), indent=1)
json.dump({
    "source": os.path.basename(SRC),
    "tonic_hz": TONIC, "bpm": BPM, "anchor_sec": ANCHOR,
    "scale_semitones": SCALE,
    "note": ("the general melody chart: each phrase starts on its clickvox "
             "beat; word onsets are on-grid by construction (8th-note "
             "quantize, min half a beat); src is the primary take's span"),
    "phrases": chart,
}, open(os.path.join(ANA, "melody-chart.json"), "w"), indent=1)

print(f"scale: {[DEGREE_NAMES[d] for d in SCALE]}  (mass "
      f"{[round(float(mass[d]), 2) for d in SCALE]})")
for ph in chart:
    line = " ".join(f"{u['word']}:{u.get('note','?')}({u['dur_beats']})"
                    for u in ph["words"])
    print(f"beat {ph['beat']:3d} [{ph['beats_total']:4.1f}b] {line}")
print("wrote analysis/notes.json + analysis/melody-chart.json")
