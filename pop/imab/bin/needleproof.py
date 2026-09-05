#!/usr/bin/env python3
# needleproof.py — proves "the sound being heard is what's passing
# under the needle" (@jeffrey 2026-09-05). Renders through the SAME
# chassis + encoder path as lyricscroll: beat lines scroll under the
# fixed needle and FLASH as they cross; the audio carries clean 1 kHz
# ticks at the beats, stepped through offsets segment by segment
# (−50 … +50 ms). Watch in QT and say which segment LOCKS — that's
# your playback chain's true offset, bakeable as SYNC_MS. After
# rendering, the script decodes its own mp4 and verifies each
# segment's in-file offset, so the pipeline can never silently drift.
#
#   pop/.venv/bin/python pop/imab/bin/needleproof.py
#     → pop/imab/out/needleproof.mp4

import math, os, subprocess, sys
import numpy as np
import soundfile as sf
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
OUT = os.path.join(LANE, "out")
sys.path.insert(0, os.path.join(os.path.dirname(LANE), "lib"))
import lyricvideo as lv

W, H = 1920, 1080
FPS = int(os.environ.get("FPS", "30"))
BPM, SR = 124.0, 48000
SPB = 60.0 / BPM
OFFSETS = [-50, -25, 0, 25, 50]
SEG_BEATS = 8
TOTAL_BEATS = SEG_BEATS * len(OFFSETS)
DUR = TOTAL_BEATS * SPB
PXS = 384 / SPB          # fast scroll so 25 ms ≈ 20 px reads by eye
NEEDLE = W // 2
TH = lv.theme()

# ── audio: ticks at beat + segment offset ─────────────────────────────
aud = np.zeros(int(DUR * SR) + SR)
tick = (np.sin(2 * np.pi * 1000 * np.arange(int(0.02 * SR)) / SR)
        * np.hanning(int(0.02 * SR)))
for b in range(TOTAL_BEATS):
    off = OFFSETS[b // SEG_BEATS] / 1000.0
    i = int((b * SPB + off) * SR)
    if 0 <= i < len(aud) - len(tick):
        aud[i:i + len(tick)] += tick * 0.8
WAV = os.path.join(OUT, "needleproof-ticks.wav")
sf.write(WAV, aud[:int(DUR * SR)], SR)

f_big, f_lab, f_rul = lv.font(120), lv.font(44), lv.font(22)

def draw_frame(t):
    img = Image.new("RGB", (W, H), TH["CREAM"])
    d = ImageDraw.Draw(img, "RGBA")
    seg = min(len(OFFSETS) - 1, int(t / SPB / SEG_BEATS))
    # beat lines scrolling under the needle
    for b in range(TOTAL_BEATS + 1):
        x = NEEDLE + (b * SPB - t) * PXS
        if -60 < x < W + 60:
            d.line([x, 200, x, H - 200], fill=(*TH["INK"], 160), width=4)
            d.text((x + 8, 210), str(b), font=f_rul, fill=(*TH["INK"], 120))
    # ms ruler around the needle
    for ms in range(-100, 101, 25):
        x = NEEDLE + (ms / 1000.0) * PXS
        big = ms % 50 == 0
        d.line([x, H // 2 - (26 if big else 14), x, H // 2 + (26 if big else 14)],
               fill=(*TH["PINK_HOT"], 150), width=2)
        if big and ms:
            d.text((x - 20, H // 2 + 32), f"{ms:+d}", font=f_rul, fill=TH["PINK_HOT"])
    # flash when a beat line is under the needle (within half a frame)
    frac = (t / SPB) % 1.0
    if min(frac, 1 - frac) * SPB < 0.5 / FPS:
        d.ellipse([NEEDLE - 90, H // 2 - 90, NEEDLE + 90, H // 2 + 90],
                  fill=(*TH["PINK_HOT"], 210))
    d.line([NEEDLE, 120, NEEDLE, H - 120], fill=TH["PINK_HOT"], width=5)
    d.text((80, 60), f"AUDIO {OFFSETS[seg]:+d} ms", font=f_big, fill=TH["INK"])
    d.text((80, H - 140), "the tick should land exactly on the flash — "
           "say which segment LOCKS", font=f_lab, fill=(*TH["INK"], 200))
    return img

MP4 = os.path.join(OUT, "needleproof.mp4")
lv.render(MP4, WAV, draw_frame, start=0, end=DUR, w=W, h=H, fps=FPS)

# ── self-check: decode the mp4, measure each segment's in-file offset ─
raw = subprocess.run(["ffmpeg", "-hide_banner", "-loglevel", "error",
    "-i", MP4, "-f", "f32le", "-ac", "1", "-ar", str(SR), "-"],
    capture_output=True).stdout
y = np.frombuffer(raw, dtype=np.float32).astype(np.float64)
print("in-file audio-tick offsets vs the visual beat (should equal each label):")
for s, off in enumerate(OFFSETS):
    errs = []
    for b in range(s * SEG_BEATS + 1, (s + 1) * SEG_BEATS):
        c = int(b * SPB * SR)
        w = np.abs(y[c - SR // 10:c + SR // 10])
        if w.max() < 0.05:
            continue
        errs.append((np.argmax(w > w.max() * 0.5) - SR // 10) / SR * 1000)
    med = np.median(errs) if errs else float("nan")
    ok = abs(med - off) < 8
    print(f"  segment {off:+d} ms → measured {med:+6.1f} ms  {'✓' if ok else '✗ PIPELINE DRIFT'}")
