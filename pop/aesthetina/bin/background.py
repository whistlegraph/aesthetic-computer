#!/usr/bin/env python3
"""
background.py — the calls behind aesthetina: owls and far-off screams, cleaned and tuned.

(@jeffrey 2026-11-20: "can we add bird calls … in the background screams hoos and has".) The hoos and
has are jeffrey's own shouted syllables and are placed by voices/reveal.py; this makes the rest:

  owl-a, owl-b   one hoot each from two Freesound recordings already in the catalogue, both CC0
                 (465697 "Owl Hoot" by Breviceps; 784609 "Birds of Prey - Barred Owl Hooting, Distant"
                 by TheKingOfGeeks360 — attribution kept in the vault's freesound-cache/_attributions.json)
  far-a, far-b   two short wordless screams from the frothyscreama chaos bank (jeffrey-pvc)

Each is cut to its loudest call, band-limited (the recording's hiss and rumble are outside the band),
gated under −32 dB of its own peak, and — the owls — moved to the nearest note of the track's A major
pentatonic by resampling. Written to out/bg/<id>.wav (44.1 k), with what was done in out/bg/report.json;
bin/render.mjs lays them in from the arrangement's "background" list, far back and in the hall.

  pop/.venv/bin/python pop/aesthetina/bin/background.py
"""
import json, subprocess
from pathlib import Path
import numpy as np, soundfile as sf, pyworld as pw
from scipy.signal import butter, sosfiltfilt, resample

HERE = Path(__file__).resolve().parent; POP = HERE.parents[1]; OUT = HERE.parent / "out" / "bg"; OUT.mkdir(parents=True, exist_ok=True)
FS = 44100
SRC = {
    "owl-a": (POP / "wattajetta/assets/owls/465697-owl_hoot.mp3", 1.4, (250, 1500), True),
    "owl-b": (POP / "wattajetta/assets/owls/784609-birds_of_prey_barred_owl_hooting_distant.mp3", 1.6, (250, 1500), True),
    "far-a": (POP / "frothyscreama/out/chaos/stab-03.wav", 0.9, (450, 3500), False),
    "far-b": (POP / "frothyscreama/out/chaos/scream-09.wav", 1.2, (450, 3500), False),
}
PENT = [57, 59, 61, 64, 66, 69, 71, 73, 76, 78, 81]   # A B C♯ E F♯
report = {}
for name, (path, secs, (lo, hi), tune) in SRC.items():
    x = np.frombuffer(subprocess.run(["/opt/homebrew/bin/ffmpeg", "-v", "error", "-i", str(path), "-f", "f32le", "-ac", "1", "-ar", str(FS), "-"], capture_output=True, check=True).stdout, dtype=np.float32).astype(np.float64)
    x = sosfiltfilt(butter(4, [lo, hi], "bandpass", fs=FS, output="sos"), x)
    n = min(len(x), int(secs * FS)); e = np.convolve(x ** 2, np.ones(n), "valid"); a = int(np.argmax(e)); y = x[a: a + n].copy()
    env = np.sqrt(np.convolve(y ** 2, np.ones(882) / 882, "same")); gate = np.clip((env / (env.max() + 1e-12) - 10 ** (-32 / 20)) / 0.02, 0, 1)
    y *= np.convolve(gate, np.hanning(441) / np.hanning(441).sum(), "same")
    info = {"source": str(path.relative_to(POP)), "from_s": round(a / FS, 2), "seconds": round(n / FS, 2), "band_hz": [lo, hi]}
    if tune:
        f0, _ = pw.harvest(y, FS, f0_floor=150, f0_ceil=900, frame_period=5.0); v = f0[f0 > 0]
        if len(v) > 5:
            m = 69 + 12 * np.log2(np.median(v) / 440); to = min(PENT, key=lambda p: abs(p - m))
            y = resample(y, int(round(len(y) * 2 ** ((m - to) / 12)))); info.update(own_midi=round(float(m), 2), tuned_to=to)
    fade = int(0.03 * FS); y[:fade] *= np.linspace(0, 1, fade); y[-fade:] *= np.linspace(1, 0, fade)
    y /= np.abs(y).max() + 1e-12
    sf.write(OUT / f"{name}.wav", np.stack([y, y], 1).astype(np.float32), FS, subtype="FLOAT"); report[name] = info
    print(name, info)
json.dump(report, open(OUT / "report.json", "w"), indent=1)
