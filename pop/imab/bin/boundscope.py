#!/usr/bin/env python3
# boundscope.py — the boundary-reading instrument: the RAW take (vocal
# stem) slowed to 60%, scrolling under a fixed needle with a fine time
# ruler (0.1 s ticks, numbers every second, in ORIGINAL-take seconds),
# the energy strip and the pyin pitch trace drawn along it. Pause
# anywhere, read the number at the needle, dictate the boundary.
#
#   pop/.venv/bin/python pop/imab/bin/boundscope.py [take_id]
#     → pop/imab/out/imab-boundscope.mp4

import math, os, subprocess, sys
import numpy as np
import librosa
from PIL import Image, ImageDraw, ImageFont

TAKE = sys.argv[1] if len(sys.argv) > 1 else "7311159624588070175"
HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(os.path.dirname(HERE), "out")
WORK = os.path.expanduser("~/.cache/ac/imab")
STEM = f"{WORK}/sep/htdemucs/whistlegraph-{TAKE}/vocals.wav"
SLOW = 0.6
W, H, FPS = 1920, 1080, 60
PXS = 260.0                     # px per ORIGINAL second — fine enough to read 50 ms
NEEDLE = 640
SYNC_MS = 50.0

y, sr = librosa.load(STEM, sr=22050, mono=True)
hop = 128
rms = librosa.feature.rms(y=y, frame_length=1024, hop_length=hop)[0]
tt = librosa.times_like(rms, sr=sr, hop_length=hop)
f0, v, vp = librosa.pyin(y, sr=sr, fmin=80, fmax=700, frame_length=1024, hop_length=hop)
voiced = v & (vp > 0.2) & np.isfinite(f0)
midi = np.where(voiced, 69 + 12 * np.log2(np.where(voiced, f0, 1) / 440.0), np.nan)
DUR = float(len(y)) / sr
NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

slow_wav = f"{WORK}/boundscope-audio.wav"
subprocess.run(["ffmpeg", "-hide_banner", "-loglevel", "error", "-y", "-i", STEM,
                "-af", f"atempo={SLOW}", "-ar", "48000", slow_wav], check=True)

BG, INK, PINK, BLUE = (255, 253, 246), (26, 26, 34), (188, 30, 104), (72, 100, 172)
F = lambda s: ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", s)
f_num, f_small, f_title = F(44), F(26), F(38)
FRAMES = int(math.ceil(DUR / SLOW * FPS))
PITCH_Y0, PITCH_Y1, LO, HI = 140, 560, 45, 75
WAVE_Y0, WAVE_Y1 = 600, 940

proc = subprocess.Popen(["ffmpeg", "-hide_banner", "-loglevel", "error", "-y",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
    "-i", slow_wav, "-map", "0:v", "-map", "1:a",
    "-c:v", "libx264", "-preset", "fast", "-crf", "19", "-c:a", "aac", "-b:a", "192k",
    "-shortest", os.path.join(OUT, "imab-boundscope.mp4")], stdin=subprocess.PIPE)

for i in range(FRAMES):
    tor = ((i + 0.5) / FPS - SYNC_MS / 1000.0) * SLOW   # ORIGINAL-take seconds
    img = Image.new("RGB", (W, H), BG)
    d = ImageDraw.Draw(img, "RGBA")
    x_of = lambda ts: NEEDLE + round((ts - tor) * PXS)
    t_lo = tor - NEEDLE / PXS
    t_hi = tor + (W - NEEDLE) / PXS
    # ruler: 0.1 s ticks, numbers at whole seconds
    for k in range(int(t_lo * 10) - 1, int(t_hi * 10) + 2):
        ts = k / 10.0
        if ts < 0 or ts > DUR: continue
        x = x_of(ts)
        big = k % 10 == 0
        d.line([x, 60, x, 110 if big else 88], fill=(*INK, 220 if big else 90), width=2 if big else 1)
        if big:
            d.text((x + 5, 58), f"{int(ts)}", font=f_num, fill=INK)
        elif k % 5 == 0:
            d.text((x + 3, 88), f"{ts:.1f}"[-2:], font=f_small, fill=(*INK, 130))
    # pitch trace
    for m in range(LO, HI + 1, 12):
        yy = PITCH_Y1 - (m - LO) / (HI - LO) * (PITCH_Y1 - PITCH_Y0)
        d.line([0, yy, W, yy], fill=(*INK, 40))
        d.text((6, yy - 26), NAMES[m % 12] + str(m // 12 - 1), font=f_small, fill=(*INK, 140))
    i0 = np.searchsorted(tt, max(0, t_lo)); i1 = np.searchsorted(tt, min(DUR, t_hi))
    for j in range(i0, i1):
        if not voiced[j]: continue
        x = x_of(tt[j])
        yy = PITCH_Y1 - (midi[j] - LO) / (HI - LO) * (PITCH_Y1 - PITCH_Y0)
        d.rectangle([x, yy - 3, x + 2, yy + 3], fill=(*PINK, 235))
    # energy waveform along time
    pk = np.percentile(rms, 97)
    for px_ in range(0, W, 2):
        ts = (px_ - NEEDLE) / PXS + tor
        j = int(ts / (hop / sr))
        if j < 0 or j >= len(rms): continue
        amp = min(1.0, rms[j] / pk)
        mid_ = (WAVE_Y0 + WAVE_Y1) / 2
        h = amp * (WAVE_Y1 - WAVE_Y0) / 2
        d.rectangle([px_, mid_ - h, px_ + 2, mid_ + h],
                    fill=(*BLUE, 220) if px_ < NEEDLE else (*INK, 70))
    d.rectangle([NEEDLE - 2, 50, NEEDLE + 2, WAVE_Y1], fill=(*PINK, 240))
    d.text((30, H - 90), f"take {TAKE} · 0.6x · needle at {max(0, tor):6.2f} s (original time)",
           font=f_title, fill=INK)
    proc.stdin.write(img.tobytes())
proc.stdin.close(); proc.wait()
print(f"✓ {OUT}/imab-boundscope.mp4")
