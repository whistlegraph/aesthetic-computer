#!/usr/bin/env python3
"""boundfix.py — refine whisper word boundaries to energy valleys.

Whisper's word edges drift; real word boundaries in singing sit at
energy minima (stop closures, breaths). For each adjacent matched-word
pair, this slides the shared boundary to the RMS valley within ±0.28s,
and extends the last word's tail to where energy actually dies. Output
is a windows file holyvox.mjs merges over the whisper timings.

  pop/.venv/bin/python pop/imab/bin/boundfix.py <stem.wav> <syllnote.json> <out.json>
"""
import json, sys, re
import numpy as np
import librosa

wav, sylj, outp = sys.argv[1], sys.argv[2], sys.argv[3]
doc = json.load(open(sylj))
y, sr = librosa.load(wav, sr=22050, mono=True)
hop = 128
rms = librosa.feature.rms(y=y, frame_length=1024, hop_length=hop)[0]
times = librosa.times_like(rms, sr=sr, hop_length=hop)
sm = np.convolve(rms, np.ones(9) / 9, mode="same")

TMPL = "i'm a butterfly flapping for you guys just a costume i put on in my room".split(" ")
def norm(w): return re.sub(r"[^a-z']", "", w.lower())
def fuzzy(a, b):
    return a == b or (len(a) > 3 and len(b) > 3 and (a.startswith(b[:4]) or b.startswith(a[:4]))) \
        or (a in ("a", "the") and b in ("a", "the"))
seq = []
ti = 0
for w in doc["words"]:
    if ti < len(TMPL) and fuzzy(TMPL[ti], norm(w["text"])):
        seq.append({"ti": ti, "text": TMPL[ti], "fromMs": w["fromMs"], "toMs": w["toMs"]})
        ti += 1

def valley(t, lo, hi, span=0.35):
    """energy minimum near t, clamped so neither word loses its core"""
    sel = (times > max(t - span, lo)) & (times < min(t + span, hi))
    idx = np.where(sel)[0]
    if not len(idx): return t
    return float(times[idx[np.argmin(sm[idx])]])

for i in range(len(seq) - 1):
    t = (seq[i]["toMs"] + seq[i + 1]["fromMs"]) / 2000
    lo = seq[i]["fromMs"] / 1000 + 0.12          # keep ≥120ms of each word
    hi = seq[i + 1]["toMs"] / 1000 - 0.12
    if hi <= lo: continue
    b = valley(t, lo, hi)
    seq[i]["toMs"] = int(b * 1000)
    seq[i + 1]["fromMs"] = int(b * 1000)
# the last word rings until its energy truly dies (max +1.2 s)
last = seq[-1]
t = last["toMs"] / 1000
sel = (times > t) & (times < t + 1.2)
idx = np.where(sel)[0]
th = sm[(times > last["fromMs"] / 1000) & (times < t)].max() * 0.06 if idx.size else 0
for i in idx:
    if sm[i] < th: last["toMs"] = int(times[i] * 1000); break
else:
    if idx.size: last["toMs"] = int(times[idx[-1]] * 1000)

json.dump({"source": wav.split("/")[-1], "words": seq}, open(outp, "w"), indent=1)
for w in seq:
    print(f"  {w['text']:<12}{w['fromMs']/1000:6.2f} – {w['toMs']/1000:6.2f}")
print(f"✓ {outp}")
