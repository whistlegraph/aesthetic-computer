#!/usr/bin/env python3
"""poster.py — hellsine "track data" PNG.

A single landscape poster of everything the all-sine engine emitted:
  · header — engine identity + stats
  · spectrogram — the whole master, section boundaries marked
  · waveform — same time axis, sections tinted
  · drum grid — every kick + snare onset from struct.json

Reads the master WAV + the engine's struct.json. All-sine, so the
spectrogram is the proof: discrete partials, no broadband sample hash.

Usage:
  python3 pop/hellsine/bin/poster.py [master.wav] [struct.json] [out.png]
"""

import json
import sys
import wave
from pathlib import Path

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

HOME = Path.home()
DEFAULT_DIR = HOME / "Documents" / "Shelf" / "hellsine"
WAV = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_DIR / "hellsine-MASTER.wav"
STRUCT = (Path(sys.argv[2]) if len(sys.argv) > 2
          else DEFAULT_DIR / ".hellsine-pre.assets" / "struct.json")
OUT = Path(sys.argv[3]) if len(sys.argv) > 3 else HOME / "Desktop" / "hellsine-track-data.png"

# ── palette — hellsine: citrus brand + hardcore heat ────────────────────
BG = "#0a0a12"
PANEL = "#11111c"
GRID = "#23232f"
INK = "#f3f0d8"
DIM = "#7d7d8f"
CITRUS = "#c6ff4f"
KICKC = "#ff5a3c"   # hot — the HOLE kick
SNAREC = "#5fe8b8"  # mint — the new backbeat snare
SECCOL = ["#ffd166", "#ff5a3c", "#5fe8b8", "#8ab4ff", "#ff7ed4", "#c6ff4f"]

# ── load struct ─────────────────────────────────────────────────────────
d = json.loads(STRUCT.read_text())
bpm = d["bpm"]
sections = d["sections"]
kicks = [e["t"] for e in d["events"]["kick"]]
snares = [e["t"] for e in d["events"]["snare"]]
total = d["totalSec"]

# ── load wav → mono float ───────────────────────────────────────────────
with wave.open(str(WAV), "rb") as w:
    sr = w.getframerate()
    ch = w.getnchannels()
    frames = w.readframes(w.getnframes())
audio = np.frombuffer(frames, dtype=np.int16).astype(np.float32) / 32768.0
if ch > 1:
    audio = audio.reshape(-1, ch).mean(axis=1)
dur = len(audio) / sr

# ── figure ──────────────────────────────────────────────────────────────
fig = plt.figure(figsize=(16, 11), dpi=150, facecolor=BG)
gs = fig.add_gridspec(4, 1, height_ratios=[0.5, 2.3, 1.0, 1.1],
                      hspace=0.32, left=0.06, right=0.97, top=0.95, bottom=0.06)


def style(ax):
    ax.set_facecolor(PANEL)
    for s in ax.spines.values():
        s.set_color(GRID)
    ax.tick_params(colors=DIM, labelsize=8)


def section_marks(ax, y_label=None):
    for i, s in enumerate(sections):
        c = SECCOL[i % len(SECCOL)]
        ax.axvline(s["startSec"], color=c, lw=1.0, alpha=0.55)
        if y_label is not None:
            ax.text(s["startSec"] + 0.6, y_label, s["name"], color=c,
                    fontsize=8.5, fontfamily="monospace", va="top", alpha=0.95)


# ── header ──────────────────────────────────────────────────────────────
ax0 = fig.add_subplot(gs[0])
ax0.axis("off")
ax0.text(0, 0.78, "hellsine", color=CITRUS, fontsize=40,
         fontfamily="monospace", fontweight="bold", va="top")
ax0.text(0.001, 0.10, "all-sine hardcore engine  ·  D natural minor  ·  "
         "John Williams leitmotif structure", color=INK, fontsize=12,
         fontfamily="monospace", va="top")
stats = (f"{bpm} BPM    {d['totalBars']} bars    "
         f"{int(dur // 60)}:{dur % 60:05.2f}    "
         f"{len(sections)} sections    {len(kicks)} kicks    "
         f"{len(snares)} snares    {sr // 1000} kHz")
ax0.text(1.0, 0.78, stats, color=DIM, fontsize=10.5, fontfamily="monospace",
         va="top", ha="right")
ax0.text(1.0, 0.30, "every voice a sum of sines — kicks, pads, brass, "
         "steam-noise + snare crack", color=DIM, fontsize=9.5,
         fontfamily="monospace", va="top", ha="right", style="italic")

# ── spectrogram ─────────────────────────────────────────────────────────
ax1 = fig.add_subplot(gs[1])
style(ax1)
ax1.specgram(audio, NFFT=2048, Fs=sr, noverlap=1024, cmap="magma",
             vmin=-120, vmax=-20)
ax1.set_ylim(0, 12000)
ax1.set_ylabel("frequency (Hz)", color=DIM, fontsize=9, fontfamily="monospace")
ax1.set_xlim(0, dur)
section_marks(ax1, y_label=11600)
ax1.set_title("spectrogram — discrete sine partials, no broadband hash",
              color=INK, fontsize=10, fontfamily="monospace", loc="left", pad=8)

# ── waveform ────────────────────────────────────────────────────────────
ax2 = fig.add_subplot(gs[2])
style(ax2)
step = max(1, len(audio) // 4000)
env = np.abs(audio[: len(audio) // step * step].reshape(-1, step)).max(axis=1)
t = np.linspace(0, dur, len(env))
ax2.fill_between(t, -env, env, color=CITRUS, lw=0, alpha=0.85)
ax2.set_ylim(-1.05, 1.05)
ax2.set_xlim(0, dur)
ax2.set_yticks([])
for i, s in enumerate(sections):
    end = s["endSec"] if "endSec" in s else (
        sections[i + 1]["startSec"] if i + 1 < len(sections) else dur)
    ax2.axvspan(s["startSec"], end, color=SECCOL[i % len(SECCOL)], alpha=0.07)
section_marks(ax2)
ax2.set_title("waveform envelope", color=INK, fontsize=10,
              fontfamily="monospace", loc="left", pad=6)

# ── drum onset grid ─────────────────────────────────────────────────────
ax3 = fig.add_subplot(gs[3])
style(ax3)
ax3.set_xlim(0, dur)
ax3.set_ylim(-0.6, 1.6)
ax3.set_yticks([0, 1])
ax3.set_yticklabels(["snare", "kick"], color=INK, fontsize=9,
                    fontfamily="monospace")
for tk in kicks:
    ax3.plot([tk, tk], [0.62, 1.38], color=KICKC, lw=1.4, alpha=0.9)
for ts in snares:
    ax3.plot([ts, ts], [-0.38, 0.38], color=SNAREC, lw=1.2, alpha=0.9)
section_marks(ax3, y_label=1.55)
ax3.set_xlabel("time (s)", color=DIM, fontsize=9, fontfamily="monospace")
ax3.set_title(f"onset grid — {len(kicks)} HOLE kicks (halftime) · "
              f"{len(snares)} backbeat snares",
              color=INK, fontsize=10, fontfamily="monospace", loc="left", pad=6)

OUT.parent.mkdir(parents=True, exist_ok=True)
fig.savefig(OUT, facecolor=BG)
print(f"→ {OUT}")
