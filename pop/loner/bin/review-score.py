#!/usr/bin/env python3
# review-score.py — lonerclub (v4pid) final review score mp4.
# Full-track waveform, section bands, seam/flourish/wub markers, sweeping
# playhead + timecode, YWFT type. Frames piped raw BGRA into ffmpeg.
import numpy as np, subprocess, sys
from PIL import Image, ImageDraw, ImageFont

SR = 8000
W, H, FPS = 1920, 1080, 30
MP3 = "pop/loner/out/lonerclub-v4pid.mp3"
OUT = "pop/loner/out/lonerclub-v4pid-review-score.mp4"
FONT_B = "/Users/jas/aesthetic-computer/slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf"
FONT_R = "/Users/jas/aesthetic-computer/slab/menuband/Sources/MenuBand/Resources/ywft-processing-regular.ttf"

raw = subprocess.run(["ffmpeg", "-v", "error", "-i", MP3, "-ac", "2",
                      "-ar", str(SR), "-f", "f32le", "-"],
                     capture_output=True).stdout
pcm = np.frombuffer(raw, np.float32).reshape(-1, 2)
DUR = len(pcm) / SR
NF = int(DUR * FPS)

SECTIONS = [
    (0.0,   31.83, "sitting open",  (64, 190, 180)),
    (31.83, 63.30, "big pass + wub", (235, 120, 60)),
    (63.30, 92.60, "wind-down",     (150, 120, 200)),
    (92.60, DUR,   "ring",          (110, 110, 110)),
]
MARKS = [
    (24.46, "tiiiime up"), (28.39, "passsss dn"),
    (31.83, "SEAM"),
    (55.94, "tiiiime up"), (59.87, "passsss dn"),
]
WUB = (31.83, 63.30)

# waveform columns
PAD, WAVE_TOP, WAVE_H = 80, 300, 560
cols = W - 2 * PAD
per = len(pcm) // cols
pk = np.array([np.abs(pcm[i*per:(i+1)*per]).max(axis=0) for i in range(cols)])
rms = np.array([np.sqrt((pcm[i*per:(i+1)*per]**2).mean(axis=0)) for i in range(cols)])

f_title = ImageFont.truetype(FONT_B, 56)
f_med = ImageFont.truetype(FONT_B, 30)
f_small = ImageFont.truetype(FONT_R, 24)
f_tc = ImageFont.truetype(FONT_B, 72)

def x_of(t): return PAD + int(t / DUR * cols)

base = Image.new("RGB", (W, H), (12, 11, 14))
d = ImageDraw.Draw(base)
d.text((PAD, 60), "lonerclub (v4pid)", font=f_title, fill=(240, 238, 232))
d.text((PAD, 130), "Whistlegraph Dot Org / pixsies / 1:34 / -9.7 LUFS / -1.5 dBTP / LRA 2.3 / wax-FM master",
       font=f_small, fill=(150, 148, 145))
# section band
BAND_Y, BAND_H = 200, 54
for t0, t1, name, c in SECTIONS:
    d.rectangle([x_of(t0), BAND_Y, x_of(t1), BAND_Y + BAND_H], fill=tuple(int(v*0.35) for v in c))
    d.text((x_of(t0) + 10, BAND_Y + 12), name, font=f_med, fill=c)
# wub underline
d.rectangle([x_of(WUB[0]), BAND_Y + BAND_H + 6, x_of(WUB[1]), BAND_Y + BAND_H + 14], fill=(235, 120, 60))
d.text((x_of(WUB[0]) + 130, BAND_Y + BAND_H + 18), "wub sub, kick-ducked, 4-6 Hz", font=f_small, fill=(200, 110, 60))
# waveform (grey base)
mid = WAVE_TOP + WAVE_H // 2
for i in range(cols):
    lh = int(pk[i, 0] * (WAVE_H // 2 - 6)); rh = int(pk[i, 1] * (WAVE_H // 2 - 6))
    lr = int(rms[i, 0] * (WAVE_H // 2 - 6)); rr = int(rms[i, 1] * (WAVE_H // 2 - 6))
    x = PAD + i
    d.line([x, mid - lh, x, mid + rh], fill=(70, 70, 76))
    d.line([x, mid - lr, x, mid + rr], fill=(105, 105, 112))
# markers
for k, (t, label) in enumerate(MARKS):
    x = x_of(t)
    col = (255, 80, 80) if label == "SEAM" else (255, 210, 90)
    d.line([x, WAVE_TOP - 24, x, WAVE_TOP + WAVE_H + 8], fill=col, width=2)
    dy = WAVE_TOP - 22 + (k % 2) * 30
    if label == "SEAM": dy = WAVE_TOP + WAVE_H - 26
    d.text((x + 6, dy), label, font=f_small, fill=col)
# time ruler
for s in range(0, int(DUR) + 1, 10):
    x = x_of(s)
    d.line([x, WAVE_TOP + WAVE_H + 24, x, WAVE_TOP + WAVE_H + 40], fill=(90, 90, 95))
    d.text((x - 20, WAVE_TOP + WAVE_H + 46), f"{s//60}:{s%60:02d}", font=f_small, fill=(120, 118, 115))
base_np = np.array(base)

# per-column tint for played region (teal-tinted waveform copy)
played = base_np.copy()
wave_region = (slice(WAVE_TOP, WAVE_TOP + WAVE_H), slice(PAD, PAD + cols))

ff = subprocess.Popen(["ffmpeg", "-y", "-v", "error",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
    "-i", MP3, "-map", "0:v", "-map", "1:a",
    "-c:v", "libx264", "-preset", "veryfast", "-crf", "18", "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "256k", "-shortest", OUT], stdin=subprocess.PIPE)

tint = np.array([90, 220, 205], dtype=np.float64)
for f in range(NF):
    t = f / FPS
    frame = base_np.copy()
    xp = x_of(t)
    # tint the played waveform region
    reg = frame[WAVE_TOP:WAVE_TOP + WAVE_H, PAD:xp].astype(np.float64)
    grey = reg.mean(axis=2, keepdims=True)
    lit = np.clip(grey / 110.0, 0, 1)
    frame[WAVE_TOP:WAVE_TOP + WAVE_H, PAD:xp] = np.clip(
        reg * 0.4 + lit * tint, 0, 255).astype(np.uint8)
    # playhead
    frame[WAVE_TOP - 30:WAVE_TOP + WAVE_H + 20, max(PAD, xp - 1):xp + 2] = (255, 245, 230)
    img = Image.fromarray(frame)
    dd = ImageDraw.Draw(img)
    tc = f"{int(t)//60}:{int(t)%60:02d}.{int((t%1)*10)}"
    dd.text((W - 360, 60), tc, font=f_tc, fill=(240, 238, 232))
    ff.stdin.write(img.tobytes())
    if f % 300 == 0: print(f"  frame {f}/{NF}", file=sys.stderr)
ff.stdin.close(); ff.wait()
print(OUT)
