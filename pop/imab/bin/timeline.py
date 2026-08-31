#!/usr/bin/env python3
# timeline.py — the imab kick+vocal study, in the loner timeline's image:
# a scrolling piano roll where every word of the hook is a block at its
# chart slot and sung pitch, bars numbered, beats gridded and tinted,
# kicks ticking along the floor, a fixed playhead with the roll sliding
# under it. Paper palette by day, the unlit room after dark
# (SCORE_THEME=light|dark overrides). Framesync learnings carried over:
# 60 fps, centre-of-interval sampling, rounded scroll at 96 px/beat.
#
#   pop/.venv/bin/python pop/imab/bin/timeline.py
#     → pop/imab/out/imab-study-timeline.mp4

import datetime, json, math, os, subprocess
import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
OUT = os.path.join(LANE, "out")
WORK = os.path.expanduser("~/.cache/ac/imab")

W, H, FPS = 1920, 1080, 60
BPM = 124.0
SPB = 60.0 / BPM
PXB = 96
PXS = PXB / SPB
PLAYHEAD_X = 560
PLACE = 2 * 4 * SPB                      # vocal enters after a 2-bar count-in
SYNC_MS = float(os.environ.get("SYNC_MS", "25"))

THEME = os.environ.get("SCORE_THEME") or ("light" if 7 <= datetime.datetime.now().hour < 19 else "dark")
if THEME == "light":
    CREAM, INK, PINK, BLUE = (255, 253, 246), (26, 26, 34), (255, 82, 156), (72, 100, 172)
    PINK_HOT, BLOCK, GLOW = (188, 30, 104), (255, 166, 202, 96), (255, 82, 156, 70)
    KICK = (72, 100, 172, 70)
    BEAT_TINT = [(255, 82, 156, 30), (72, 100, 172, 14), (26, 26, 34, 10), (72, 100, 172, 14)]
    WAVE = (26, 26, 34, 185)
else:
    CREAM, INK, PINK, BLUE = (14, 13, 20), (238, 234, 230), (255, 92, 162), (132, 158, 236)
    PINK_HOT, BLOCK, GLOW = (255, 176, 214), (255, 92, 162, 52), (255, 92, 162, 64)
    KICK = (132, 158, 236, 66)
    BEAT_TINT = [(255, 92, 162, 34), (132, 158, 236, 14), (238, 234, 230, 10), (132, 158, 236, 14)]
    WAVE = (238, 234, 230, 200)

targets = json.load(open(os.path.join(WORK, "holy-targets.json")))
NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
def to_midi(n):
    import re
    m = re.match(r"^([A-G]#?)(-?\d)$", n)
    return (int(m.group(2)) + 1) * 12 + NAMES.index(m.group(1))
words = [{"t": PLACE + t["t"], "dur": max(t["dur"], 0.2), "midi": to_midi(t["note"]),
          "label": t["label"].split("·")[0], "note": t["note"]} for t in targets]

AUD = os.path.join(WORK, ".line.wav")    # the lyricline study mix: click+kick+vox
dur = float(subprocess.run(["ffprobe", "-v", "quiet", "-show_entries", "format=duration",
                            "-of", "default=nw=1:nk=1", AUD], capture_output=True, text=True).stdout.strip())
FRAMES = int(math.ceil(dur * FPS))
TOTAL_BEATS = int(math.ceil(dur / SPB))

r = subprocess.run(["ffmpeg", "-v", "error", "-i", AUD, "-ac", "1", "-ar", "8000", "-f", "f32le", "-"],
                   capture_output=True)
samples = np.frombuffer(r.stdout, np.float32)

F = lambda s: ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", s)
f_title, f_bar, f_word, f_note = F(38), F(28), F(36), F(22)

ROLL_Y0, ROLL_Y1 = 150, 700
LO, HI = 58, 74                          # D4-ish window around the melody C4–C5
def y_of(midi):
    return ROLL_Y1 - (midi - LO) / (HI - LO) * (ROLL_Y1 - ROLL_Y0)
KICK_Y0, KICK_Y1 = 730, 800
WAVE_Y0, WAVE_Y1 = 830, 1040

proc = subprocess.Popen(["ffmpeg", "-hide_banner", "-loglevel", "error", "-y",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
    "-i", AUD, "-map", "0:v", "-map", "1:a",
    "-c:v", "libx264", "-preset", "fast", "-crf", "19", "-c:a", "aac", "-b:a", "192k",
    "-shortest", os.path.join(OUT, "imab-study-timeline.mp4")], stdin=subprocess.PIPE)

for i in range(FRAMES):
    t = (i + 0.5) / FPS - SYNC_MS / 1000.0
    img = Image.new("RGB", (W, H), CREAM)
    d = ImageDraw.Draw(img, "RGBA")
    x_of = lambda te: PLAYHEAD_X + round((te - t) * PXS)
    # beat columns, tinted per position in bar; bar numbers
    b0 = int(max(0, (t - PLAYHEAD_X / PXS) / SPB) - 1)
    b1 = int((t + (W - PLAYHEAD_X) / PXS) / SPB) + 1
    for b in range(max(0, b0), min(TOTAL_BEATS, b1)):
        x = x_of(b * SPB)
        d.rectangle([x, ROLL_Y0, x + PXB, ROLL_Y1], fill=BEAT_TINT[b % 4])
        d.line([x, ROLL_Y0, x, KICK_Y1], fill=(*INK, 90 if b % 4 == 0 else 30), width=2 if b % 4 == 0 else 1)
        if b % 4 == 0:
            d.text((x + 8, ROLL_Y0 - 36), f"{b // 4 + 1}", font=f_bar, fill=(*INK, 200))
    # word blocks at sung pitch
    for w in words:
        x0, x1 = x_of(w["t"]), x_of(w["t"] + w["dur"])
        if x1 < -50 or x0 > W + 50: continue
        y = y_of(w["midi"])
        active = w["t"] <= t <= w["t"] + w["dur"]
        d.rectangle([x0, y - 26, x1, y + 26], fill=GLOW if active else BLOCK,
                    outline=(*PINK_HOT, 255) if active else (*PINK_HOT, 140), width=2)
        d.text((x0 + 6, y - 62), w["label"], font=f_word, fill=PINK_HOT if not active else INK)
        d.text((x0 + 6, y + 30), w["note"], font=f_note, fill=(*INK, 160))
    # kicks on the floor (from the count-in on, every beat)
    for b in range(0, TOTAL_BEATS):
        x = x_of(b * SPB)
        if x < -20 or x > W + 20: continue
        d.rectangle([x + 2, KICK_Y0, x + 26, KICK_Y1], fill=KICK)
    # waveform, filled to the playhead
    n0 = int(max(0, (t - PLAYHEAD_X / PXS)) * 8000)
    span = int((W / PXS) * 8000)
    for px_ in range(0, W, 2):
        a = n0 + int(px_ / W * span); b_ = a + max(1, span // (W // 2))
        seg = samples[a:b_]
        if not len(seg): continue
        amp = float(np.sqrt((seg * seg).mean())) * 4
        h = min(1.0, amp) * (WAVE_Y1 - WAVE_Y0) / 2
        mid = (WAVE_Y0 + WAVE_Y1) / 2
        col = WAVE if px_ < PLAYHEAD_X else (*INK, 60)
        d.rectangle([px_, mid - h, px_ + 2, mid + h], fill=col)
    # playhead + header
    d.rectangle([PLAYHEAD_X - 2, ROLL_Y0 - 44, PLAYHEAD_X + 2, WAVE_Y1], fill=(*PINK, 235))
    bar_now = int(t / SPB / 4) + 1
    d.text((36, 30), "imab · kick + vocal study · 124", font=f_title, fill=INK)
    d.text((W - 320, 30), f"BAR {max(1, bar_now)}", font=f_title, fill=PINK_HOT)
    proc.stdin.write(img.tobytes())
    if i % 300 == 0: print(f"  {i}/{FRAMES}")
proc.stdin.close(); proc.wait()
print(f"✓ {OUT}/imab-study-timeline.mp4")
