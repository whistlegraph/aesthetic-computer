#!/usr/bin/env python3
"""Measure the Flower Eater primary take (6992837952212569350).

What comes out (all into analysis/):
  tempo.json    — onset-derived tempo: global, windowed drift, verdict
  melody.json   — per-word pyin: median f0, note name, cents off, degree
  transcript.json — the curated word stream (from whisper-raw/, -ml 1
                    tokens merged back into words with timestamps)

Run with pop/.venv:  ../../.venv/bin/python3 analyze.py
"""
import json, os, re
import numpy as np
import librosa

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = os.path.join(HERE, "..", "source", "flwe-6992837952212569350.wav")
ANA = os.path.join(HERE, "..", "analysis")
RAW = os.path.join(ANA, "whisper-raw", "flwe-6992837952212569350.json")

SONG = (18.0, 80.0)  # the sung piece inside the take (spoken intro/outro outside)

# ---------------------------------------------------------------- transcript
def load_words():
    """Merge whisper's -ml 1 sub-word tokens back into words."""
    segs = json.load(open(RAW))["transcription"]
    toks = []
    for s in segs:
        txt = s["text"]
        if not txt.strip() or txt.strip() in ".,!?":
            continue
        toks.append((s["offsets"]["from"] / 1000, s["offsets"]["to"] / 1000, txt))
    words = []
    for t0, t1, txt in toks:
        # a token that doesn't start with a space continues the previous word
        if words and not txt.startswith(" "):
            w = words[-1]
            w["word"] += txt.strip("'") if txt.strip() == "'" else txt
            w["end"] = t1
        else:
            words.append({"word": txt.strip(), "start": round(t0, 2), "end": round(t1, 2)})
    for w in words:
        w["end"] = round(w["end"], 2)
    return words

# ---------------------------------------------------------------- audio
y, sr = librosa.load(SRC, sr=48000, mono=True)

def clip(t0, t1):
    return y[int(t0 * sr):int(t1 * sr)]

# ---------------------------------------------------------------- tempo
song = clip(*SONG)
oenv = librosa.onset.onset_strength(y=song, sr=sr, hop_length=512)
tempo_global = float(librosa.feature.tempo(onset_envelope=oenv, sr=sr, hop_length=512)[0])
onsets = librosa.onset.onset_detect(onset_envelope=oenv, sr=sr, hop_length=512, units="time")

# windowed tempo → drift (10 s windows, 5 s hop)
win, hop = 10.0, 5.0
frames_per_sec = sr / 512
drift = []
t = 0.0
dur = len(song) / sr
while t + win <= dur:
    seg = oenv[int(t * frames_per_sec):int((t + win) * frames_per_sec)]
    bpm = float(librosa.feature.tempo(onset_envelope=seg, sr=sr, hop_length=512)[0])
    drift.append({"t": round(SONG[0] + t, 1), "bpm": round(bpm, 1)})
    t += hop
bpms = [d["bpm"] for d in drift]
# fold octave errors toward the median before judging drift
med = float(np.median(bpms))
folded = [b / 2 if b > med * 1.5 else b * 2 if b < med / 1.5 else b for b in bpms]
med = float(np.median(folded))

tempo = {
    "source": os.path.basename(SRC),
    "song_region_sec": list(SONG),
    "method": "librosa onset_strength -> feature.tempo, global + 10s windows (5s hop), octave-folded",
    "bpm_global": round(tempo_global, 1),
    "bpm_median": round(med, 1),
    "bpm_windows": drift,
    "bpm_windows_folded": [round(b, 1) for b in folded],
    "drift_range": [round(min(folded), 1), round(max(folded), 1)],
    "verdict": "rubato" if (max(folded) - min(folded)) > 0.12 * med else "steady",
    "onset_count": int(len(onsets)),
}

# ---------------------------------------------------------------- melody (pyin per word)
words = load_words()
f0, vflag, vprob = librosa.pyin(
    y, sr=sr, fmin=librosa.note_to_hz("C2"), fmax=librosa.note_to_hz("C6"),
    frame_length=2048, hop_length=512,
)
times = librosa.times_like(f0, sr=sr, hop_length=512)

def word_pitch(w):
    m = (times >= w["start"]) & (times <= max(w["end"], w["start"] + 0.08))
    fs = f0[m]
    fs = fs[~np.isnan(fs)]
    if len(fs) < 2:
        return None
    hz = float(np.median(fs))
    midi = librosa.hz_to_midi(hz)
    note = librosa.midi_to_note(round(midi))
    cents = round((midi - round(midi)) * 100)
    return {"hz": round(hz, 1), "note": note, "cents": cents}

melody = []
for w in words:
    sung = SONG[0] <= w["start"] <= SONG[1]
    p = word_pitch(w)
    entry = {**w, "sung": sung}
    if p:
        entry.update(p)
    melody.append(entry)

# key: chroma over the sung region, voiced pyin histogram
sung_notes = [m["note"][:-1] for m in melody if m["sung"] and "note" in m]
hist = {}
for n in sung_notes:
    hist[n] = hist.get(n, 0) + 1

chroma = librosa.feature.chroma_cqt(y=song, sr=sr)
chroma_mean = chroma.mean(axis=1)
pcs = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
chroma_rank = sorted(zip(pcs, chroma_mean.round(3).tolist()), key=lambda x: -x[1])

out = {
    "source": os.path.basename(SRC),
    "method": "librosa.pyin C2-C6 over full take, median f0 per whisper word window",
    "words": melody,
    "pitch_class_histogram": hist,
    "chroma_cqt_ranked": chroma_rank,
}

os.makedirs(ANA, exist_ok=True)
json.dump(tempo, open(os.path.join(ANA, "tempo.json"), "w"), indent=1)
json.dump(out, open(os.path.join(ANA, "melody.json"), "w"), indent=1)
json.dump({"source": os.path.basename(SRC), "words": words,
           "song_region_sec": list(SONG)},
          open(os.path.join(ANA, "transcript.json"), "w"), indent=1)

print("tempo:", json.dumps({k: tempo[k] for k in ("bpm_global", "bpm_median", "drift_range", "verdict")}))
print("pitch classes:", json.dumps(hist))
print("chroma:", chroma_rank[:6])
for m in melody:
    if m["sung"] and "note" in m:
        print(f"{m['start']:7.2f} {m['word']:<14} {m['note']:>4} {m['cents']:+4d}c {m['hz']}Hz")
