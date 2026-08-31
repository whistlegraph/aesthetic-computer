#!/usr/bin/env python3
"""flwe-clickvox — the vocalized click track study.

A steady click marks the measured grid (median BPM from
analysis/tempo.json) and the primary take's own sung phrases sit on it,
each phrase start snapped to the nearest beat (light quantize) with its
internal rubato left intact. Dry and legible: this is for tuning
alignment by ear, not a produced track.

Run with pop/.venv:  ../../.venv/bin/python3 clickvox.py
Writes out/flwe-clickvox.wav (+ .mp3 via ffmpeg) and
analysis/clickvox.json (the placement receipt).
"""
import json, os, subprocess
import numpy as np
import soundfile as sf

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = os.path.join(HERE, "..", "source", "flwe-6992837952212569350.wav")
ANA = os.path.join(HERE, "..", "analysis")
OUT = os.path.join(HERE, "..", "out")
SR = 48000

tempo = json.load(open(os.path.join(ANA, "tempo.json")))
words = json.load(open(os.path.join(ANA, "transcript.json")))
BPM = tempo["bpm_median"]
BEAT = 60.0 / BPM
SONG = words["song_region_sec"]

# ------------------------------------------------------------- phrases
# Group the sung words into phrases at gaps > 0.3 s between words.
sung = [w for w in words["words"] if SONG[0] <= w["start"] <= SONG[1]]
phrases = [[sung[0]]]
for w in sung[1:]:
    if w["start"] - phrases[-1][-1]["end"] > 0.3:
        phrases.append([w])
    else:
        phrases[-1].append(w)
phrases = [
    {"text": " ".join(w["word"] for w in p),
     "t0": p[0]["start"], "t1": p[-1]["end"]}
    for p in phrases
]

# ------------------------------------------------------------- grid
# Beat 0 = the first sung onset. Two bars of count-in click before it.
ANCHOR = phrases[0]["t0"]
COUNT_IN = 8  # beats
for p in phrases:
    beats = (p["t0"] - ANCHOR) / BEAT
    p["beat"] = round(beats)                # snapped
    p["beat_measured"] = round(beats, 2)
    p["shift_sec"] = round(p["beat"] * BEAT - (p["t0"] - ANCHOR), 3)

last_beat = max(p["beat"] + (p["t1"] - p["t0"]) / BEAT for p in phrases)
total_beats = COUNT_IN + int(np.ceil(last_beat / 4) * 4) + 4  # pad a bar
dur = total_beats * BEAT + 1.0
mix = np.zeros(int(dur * SR))

# ------------------------------------------------------------- clicks
def kick():
    """Downbeat: a soft kick — 65 ms sine sweep 150→52 Hz."""
    n = int(0.065 * SR)
    t = np.arange(n) / SR
    f = 150 * np.exp(np.log(52 / 150) * t / t[-1])
    ph = 2 * np.pi * np.cumsum(f) / SR
    return np.sin(ph) * np.exp(-t * 40) * 0.9

def tick():
    """Other beats: a 12 ms 1.8 kHz tick."""
    n = int(0.012 * SR)
    t = np.arange(n) / SR
    return np.sin(2 * np.pi * 1800 * t) * np.exp(-t * 350) * 0.35

k, tk = kick(), tick()
for b in range(total_beats):
    t = b * BEAT
    s = int(t * SR)
    g = k if b % 4 == 0 else tk
    mix[s:s + len(g)] += g

# ------------------------------------------------------------- voices
y, sr = sf.read(SRC)
assert sr == SR
if y.ndim > 1:
    y = y.mean(axis=1)

def fade(x, ms=12):
    n = int(ms / 1000 * SR)
    x = x.copy()
    x[:n] *= np.linspace(0, 1, n)
    x[-n:] *= np.linspace(1, 0, n)
    return x

PAD = 0.06  # take a hair of air either side of the whisper window
for p in phrases:
    a = max(0, int((p["t0"] - PAD) * SR))
    b = min(len(y), int((p["t1"] + PAD) * SR))
    clip = fade(y[a:b])
    pos = int(((COUNT_IN + p["beat"]) * BEAT - PAD) * SR)
    mix[pos:pos + len(clip)] += clip * 1.0

mix /= np.abs(mix).max() / 0.89  # ~ -1 dBFS
os.makedirs(OUT, exist_ok=True)
wav = os.path.join(OUT, "flwe-clickvox.wav")
sf.write(wav, mix, SR)
subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-i", wav,
                "-b:a", "192k", os.path.join(OUT, "flwe-clickvox.mp3")], check=True)

receipt = {
    "source": os.path.basename(SRC),
    "bpm": BPM,
    "beat_sec": round(BEAT, 4),
    "meter": "4/4 assumed; kick on downbeats, tick on beats",
    "anchor_sec": ANCHOR,
    "count_in_beats": COUNT_IN,
    "quantize": "phrase starts snapped to nearest beat; internal rubato kept",
    "phrases": phrases,
}
json.dump(receipt, open(os.path.join(ANA, "clickvox.json"), "w"), indent=1)
for p in phrases:
    print(f"beat {p['beat']:3d} ({p['beat_measured']:6.2f} measured, "
          f"{p['shift_sec']:+.3f}s) {p['text']}")
print("wrote", wav)
