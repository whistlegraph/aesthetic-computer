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

import json
y, sr = librosa.load(STEM, sr=22050, mono=True)
hop = 128
rms = librosa.feature.rms(y=y, frame_length=1024, hop_length=hop)[0]
tt = librosa.times_like(rms, sr=sr, hop_length=hop)
DUR = float(len(y)) / sr
# mel spectrogram, pre-rendered at ruler resolution — words are readable:
# vowels = bright bands, sibilants = top splash, gaps = boundaries
S = librosa.feature.melspectrogram(y=y, sr=sr, n_mels=110, hop_length=hop, fmax=6000)
Sdb = librosa.power_to_db(S, ref=np.max)
Sn = np.clip((Sdb + 62) / 62, 0, 1)
SPEC_H = 380
spec_w = int(DUR * PXS)
cols = np.linspace(0, Sn.shape[1] - 1, spec_w).astype(int)
strip = Sn[::-1, cols]                                  # low freq at bottom
rows = np.linspace(0, strip.shape[0] - 1, SPEC_H).astype(int)
strip = strip[rows]
ink = np.array([26, 26, 34]); paper = np.array([255, 253, 246]); hot = np.array([188, 30, 104])
rgb = (paper[None, None] * (1 - strip[..., None]) +
       (ink[None, None] * (1 - strip[..., None] * 0.35) + hot[None, None] * (strip[..., None] * 0.35)) * strip[..., None])
SPEC_IMG = Image.fromarray(rgb.astype(np.uint8), "RGB")
# current boundary guesses, to be corrected by ear
BOUNDS = []
bp = f"{WORK}/bounds-{TAKE}.json"
if os.path.exists(bp):
    for w in json.load(open(bp))["words"]:
        for sy in w.get("sylls", [{"label": w["text"], "fromMs": w["fromMs"], "toMs": w["toMs"]}]):
            BOUNDS.append((sy["label"], sy["fromMs"] / 1000, sy["toMs"] / 1000))

slow_wav = f"{WORK}/boundscope-audio.wav"
subprocess.run(["ffmpeg", "-hide_banner", "-loglevel", "error", "-y", "-i", STEM,
                "-af", f"atempo={SLOW}", "-ar", "48000", slow_wav], check=True)

BG, INK, PINK, BLUE = (255, 253, 246), (26, 26, 34), (188, 30, 104), (72, 100, 172)
F = lambda s: ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", s)
f_num, f_small, f_title = F(44), F(26), F(38)
FRAMES = int(math.ceil(DUR / SLOW * FPS))
SPEC_Y0 = 190
WAVE_Y0, WAVE_Y1 = 620, 940

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
    # spectrogram window, same time axis as the ruler
    sx0 = int(max(0, t_lo) * PXS); sx1 = int(min(DUR, t_hi) * PXS)
    if sx1 > sx0:
        crop = SPEC_IMG.crop((sx0, 0, sx1, SPEC_H))
        img.paste(crop, (x_of(sx0 / PXS), SPEC_Y0))
    # boundary guesses: brackets + labels riding the spectrogram
    for (lab, b0, b1) in BOUNDS:
        if b1 < t_lo or b0 > t_hi: continue
        xa, xb = x_of(b0), x_of(b1)
        d.line([xa, SPEC_Y0 - 14, xa, SPEC_Y0 + SPEC_H + 14], fill=(*BLUE, 200), width=2)
        d.text((xa + 4, SPEC_Y0 - 44), lab, font=f_small, fill=(*BLUE, 255))
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
