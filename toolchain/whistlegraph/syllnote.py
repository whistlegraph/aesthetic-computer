#!/usr/bin/env python3
"""syllnote.py — syllable → note mapping for a SUNG whistlegraph clip.

Plosive-aware by construction: whisper gives word windows, pyin gives a
voice-range f0 track, and the note is read only from the VOICED nucleus
frames — plosives/unvoiced consonants carry no f0 and are excluded, so
a word's burst never pollutes its pitch. Contiguous voiced runs inside
a word window become its syllable nuclei (energy-weighted median f0).

  pop/.venv/bin/python syllnote.py downloads/whistlegraph-<id>.wav
  → downloads/whistlegraph-<id>.syllnote.json

Words come from whisper-cli (recap model, -ml 1); pass --words to reuse
an existing [{text, fromMs, toMs}] sidecar instead.
"""
import json, os, subprocess, sys, tempfile

import numpy as np
import librosa

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))
MODEL = f"{REPO}/recap/models/ggml-base.en.bin"
NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]

def midi_name(m): return NAMES[int(round(m)) % 12] + str(int(round(m)) // 12 - 1)

def whisper_words(wav):
    with tempfile.TemporaryDirectory() as td:
        pref = os.path.join(td, "w")
        subprocess.run(["whisper-cli", "-m", MODEL, "-f", wav, "-ojf", "-of", pref,
                        "--max-len", "1", "-ml", "1", "-sow"],
                       check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        raw = json.load(open(pref + ".json"))
    return [{"text": s["text"].strip(), "fromMs": s["offsets"]["from"], "toMs": s["offsets"]["to"]}
            for s in raw["transcription"] if s["text"].strip()]

def main():
    wav = sys.argv[1]
    words_path = sys.argv[sys.argv.index("--words") + 1] if "--words" in sys.argv else None
    out_path = (sys.argv[sys.argv.index("--out") + 1] if "--out" in sys.argv
                else os.path.splitext(wav)[0] + ".syllnote.json")

    words = json.load(open(words_path)) if words_path else whisper_words(wav)
    y, sr = librosa.load(wav, sr=22050, mono=True)
    hop = 256
    f0, voiced, vprob = librosa.pyin(y, sr=sr, fmin=80, fmax=600,
                                     frame_length=2048, hop_length=hop)
    times = librosa.times_like(f0, sr=sr, hop_length=hop)
    rms = librosa.feature.rms(y=y, frame_length=2048, hop_length=hop)[0]
    rms_n = rms / max(1e-9, np.percentile(rms, 95))
    # the take's own sung register — junk filter reference (octave errors,
    # bleed at word edges)
    gv = np.isfinite(f0) & voiced & (vprob > 0.3)
    global_med = float(np.median(69 + 12 * np.log2(f0[gv] / 440.0))) if gv.any() else 57.0

    def nuclei(t0, t1):
        """voiced runs inside [t0,t1] with real energy → syllable nuclei"""
        sel = (times >= t0) & (times <= t1)
        idx = np.where(sel)[0]
        if not len(idx): return []
        ok = voiced[idx] & (vprob[idx] > 0.30) & (rms_n[idx] > 0.035) & np.isfinite(f0[idx])
        runs, cur = [], []
        gap = int(0.060 / (hop / sr))          # >60 ms unvoiced splits nuclei
        last = None
        for j, i in enumerate(idx):
            if ok[j]:
                if last is not None and i - last > gap and cur:
                    runs.append(cur); cur = []
                cur.append(i); last = i
        if cur: runs.append(cur)
        out = []
        for r in runs:
            if times[r[-1]] - times[r[0]] < 0.050: continue   # too short to be a nucleus
            fr = np.array([f0[i] for i in r]); wt = np.array([rms[i] for i in r])
            order = np.argsort(fr)
            cum = np.cumsum(wt[order]); med = fr[order[np.searchsorted(cum, cum[-1] / 2)]]
            midi = 69 + 12 * np.log2(med / 440.0)
            if abs(midi - global_med) > 14: continue           # octave junk / edge bleed
            out.append({"midi": int(round(midi)), "note": midi_name(midi),
                        "cents": round(100 * (midi - round(midi)), 1),
                        "startSec": round(float(times[r[0]]), 3),
                        "durSec": round(float(times[r[-1]] - times[r[0]]), 3),
                        "rms": round(float(wt.mean()), 4)})
        return out

    for w in words:
        w["nuclei"] = nuclei(w["fromMs"] / 1000 - 0.02, w["toMs"] / 1000 + 0.04)
    doc = {"source": os.path.basename(wav), "sr": sr,
           "words": words, "wordCount": len(words),
           "nucleusCount": sum(len(w["nuclei"]) for w in words)}
    json.dump(doc, open(out_path, "w"), indent=1)
    print(f"✓ {out_path} · {doc['wordCount']} words · {doc['nucleusCount']} nuclei")

if __name__ == "__main__":
    main()
