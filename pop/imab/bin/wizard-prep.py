#!/usr/bin/env python3
"""wizard-prep.py — lay out the table for SyllaWizard on any take.

The wizard wants two things in ~/.cache/ac/imab: a spectrogram at 260
px/s and a bounds-<take>.json of machine guesses it can pre-seed as
editable rectangles. This renders both, and it seeds from notealign —
the known melody DTW'd against the measured pitch — so @jeffrey is
correcting a reading that already knows where the notes are, instead of
drawing twenty rectangles from a blank spectrogram.

  pop/.venv/bin/python wizard-prep.py <take-id> [--from S] [--to S]
  → ~/.cache/ac/imab/wizard-spec.png
  → ~/.cache/ac/imab/bounds-<take>.json
  then: cd sylla-wizard && swift run SyllaWizard <take-id>

--from/--to bound the sung phrase; without them the whole stem is
aligned, and anything the singer says around the hook will pull the
seeds off.
"""
import json, os, subprocess, sys
import numpy as np
import librosa
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
WORK = os.path.expanduser("~/.cache/ac/imab")
PXS = 260          # px per second — the wizard's fixed scale
SPEC_H = 420

take = sys.argv[1] if len(sys.argv) > 1 else "7311159624588070175"
def opt(name, d=None):
    return float(sys.argv[sys.argv.index(name) + 1]) if name in sys.argv else d
t_from, t_to = opt("--from"), opt("--to")

stem = f"{WORK}/sep/htdemucs/whistlegraph-{take}/vocals.wav"
if not os.path.exists(stem):
    sys.exit(f"missing stem {stem}")

y, sr = librosa.load(stem, sr=22050, mono=True)
dur = len(y) / sr
print(f"take {take} · stem {dur:.2f}s")

# ── spectrogram the wizard draws on ───────────────────────────────────
n_fft, hop = 2048, max(1, int(round(sr / PXS)))
S = librosa.feature.melspectrogram(y=y, sr=sr, n_fft=n_fft, hop_length=hop,
                                   n_mels=SPEC_H, fmin=60, fmax=6000)
D = librosa.power_to_db(S, ref=np.max)
D = np.clip((D + 70) / 70, 0, 1)
img = np.flipud(D)
rgb = np.zeros((img.shape[0], img.shape[1], 3), dtype=np.uint8)
rgb[..., 0] = (np.clip(img * 1.5, 0, 1) * 255)
rgb[..., 1] = (np.clip(img * 1.05, 0, 1) * 255)
rgb[..., 2] = (np.clip(0.25 + img * 0.8, 0, 1) * 255)
Image.fromarray(rgb).save(f"{WORK}/wizard-spec.png")
print(f"✓ wizard-spec.png {rgb.shape[1]}×{rgb.shape[0]} ({rgb.shape[1] / PXS:.1f}s at {PXS}px/s)")

# ── seeds: the written hook aligned to this take's pitch ──────────────
# SyllaWizard's own syllable roster, in order (label, word index).
SYLS = [("i'm", 0), ("a", 1), ("but", 2), ("ter", 2), ("fly", 2), ("flap", 3),
        ("ping", 3), ("for", 4), ("you", 5), ("guys", 6), ("just", 7), ("a", 8),
        ("cos", 9), ("tume", 9), ("i", 10), ("put", 11), ("on", 12), ("in", 13),
        ("my", 14), ("room", 15)]
GT = ["C4", "G4", "C4", "C4", "C4", "C4", "C5", "C4", "C4", "C4",
      "G4", "F4", "E4", "D4", "E4", "E4", "D4", "C4", "C4", "C4"]

clip = stem
if t_from is not None or t_to is not None:
    a, b = t_from or 0.0, t_to or dur
    clip = f"{WORK}/wizard-align-{take}.wav"
    subprocess.run(["ffmpeg", "-hide_banner", "-loglevel", "error", "-y",
                    "-ss", str(a), "-t", str(b - a), "-i", stem,
                    "-ac", "1", "-ar", "22050", clip], check=True)
    print(f"  aligning inside {a:.2f}–{b:.2f}s")
off_ms = (t_from or 0.0) * 1000

nj = f"{WORK}/wizard-notes-{take}.json"
json.dump([{"label": s[0], "note": n} for s, n in zip(SYLS, GT)], open(nj, "w"))
r = subprocess.run([sys.executable, os.path.join(HERE, "notealign.py"), clip, nj],
                   capture_output=True, text=True)
try:
    al = json.loads(r.stdout)
except Exception:
    sys.exit(f"notealign failed: {r.stderr[-400:]}")

sylls = []
for s, a in zip(SYLS, al["syllables"]):
    sylls.append({"label": s[0], "wi": s[1],
                  "fromMs": int(round(a["fromMs"] + off_ms)),
                  "toMs": int(round(a["toMs"] + off_ms)),
                  "fLo": 0.15, "fHi": 0.9})
json.dump({"take": take, "source": "notealign", "transpose": al["offset"],
           "words": [{"sylls": sylls}]}, open(f"{WORK}/bounds-{take}.json", "w"), indent=1)
print(f"✓ bounds-{take}.json — {len(sylls)} seeded rects (written hook fits at {al['offset']:+d} semitones)")
for s, a in zip(SYLS, al["syllables"]):
    sung = a.get("sungMidi")
    print(f"   {s[0]:<5} {int(a['fromMs'] + off_ms):>6}–{int(a['toMs'] + off_ms):<6}ms  "
          f"want {a['note']:<3}" + (f"  sung {librosa.midi_to_note(sung)}" if sung else "  (unvoiced)"))
