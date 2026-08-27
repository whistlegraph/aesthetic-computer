# melody.py — pin down the primary take before the score commits to a key.
#
# analyze.py's per-word medians are rough (whisper spans bleed across note
# boundaries, and pyin's 70 Hz floor reads shouted consonants as C#2).
# This probe works the other way: a continuous pyin track over the whole
# take, segmented into notes wherever the contour holds still, plus
# inter-onset intervals over the chant hits so the tempo is a measurement
# with a receipt instead of beat_track's one guess.
#
#   pop/.venv/bin/python pop/season/bin/melody.py   # → analysis/melody.json
#
# Run from pop/season/.

import json
import numpy as np
import librosa

SR = 48_000
ID = "7079639110025088298"          # "all four seasons in 9 seconds"
HOP = 256

y, _ = librosa.load(f"wav/{ID}.wav", sr=SR, mono=True)

f0, vflag, _ = librosa.pyin(y, fmin=80, fmax=800, sr=SR,
                            frame_length=2048, hop_length=HOP)
t = librosa.times_like(f0, sr=SR, hop_length=HOP)

def note_name(hz):
    names = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]
    m = 69 + 12 * np.log2(hz / 440.0)
    n = int(round(m))
    return f"{names[n % 12]}{n // 12 - 1}", round(float(100 * (m - n)), 1)

# ── segment the voiced contour into held notes ────────────────────────
# A note is >= 60 ms of voiced frames whose semitone track stays within
# ±0.6 st of its own running median. Glides split; vibrato doesn't.
notes, cur = [], []
for i in range(len(f0)):
    ok = bool(vflag[i]) and not np.isnan(f0[i])
    if ok:
        st = 12 * np.log2(f0[i] / 440.0)
        if cur and abs(st - np.median([c[1] for c in cur])) > 0.6:
            if len(cur) * HOP / SR >= 0.06:
                notes.append(cur)
            cur = []
        cur.append((t[i], st, f0[i]))
    else:
        if len(cur) * HOP / SR >= 0.06:
            notes.append(cur)
        cur = []
if len(cur) * HOP / SR >= 0.06:
    notes.append(cur)

out_notes = []
for seg in notes:
    hz = float(np.median([c[2] for c in seg]))
    name, cents = note_name(hz)
    out_notes.append(dict(start=round(seg[0][0], 3),
                          end=round(seg[-1][0], 3),
                          hz=round(hz, 1), note=name, cents=cents))
    print(f"  {seg[0][0]:6.2f}-{seg[-1][0]:6.2f}  {hz:6.1f} Hz  {name:4s} {cents:+6.1f}c")

# ── the chant's own tempo ─────────────────────────────────────────────
# Onsets across the spoken argument (0–2.6 s): "it's / too / hot / no
# it's not". The syllable grid is the tempo the take actually keeps.
oenv = librosa.onset.onset_strength(y=y, sr=SR, hop_length=HOP)
onsets = librosa.onset.onset_detect(onset_envelope=oenv, sr=SR,
                                    hop_length=HOP, units="time",
                                    backtrack=True)
chant = [o for o in onsets if o < 5.0]
ioi = np.diff(chant)
print("chant onsets:", [round(float(o), 3) for o in chant])
print("IOIs:", [round(float(v), 3) for v in ioi])
if len(ioi):
    med = float(np.median(ioi))
    print(f"median IOI {med:.3f} s  → {60/med:.1f} BPM at that pulse "
          f"(x2 = {120/med:.1f}, /2 = {30/med:.1f})")

json.dump(dict(id=ID, notes=out_notes,
               onsets=[round(float(o), 3) for o in onsets.tolist()],
               chant_median_ioi_s=round(float(np.median(ioi)), 3) if len(ioi) else None),
          open("analysis/melody.json", "w"), indent=2)
print("WROTE analysis/melody.json")
