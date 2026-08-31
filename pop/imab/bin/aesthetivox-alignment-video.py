#!/usr/bin/env python3
# aesthetivox-alignment-video.py — THE FEATURE, named (@jeffrey
# 2026-08-31): the aesthetivox alignment video. Every aesthetivox vocal
# gets one of these — the scrutiny instrument for utterance-on-grid
# alignment. Born as loner's timeline.py, refined on imab:
# a scrolling piano roll where every word of the hook is a block at its
# chart slot and sung pitch, bars numbered, beats gridded and tinted,
# kicks ticking along the floor, a fixed playhead with the roll sliding
# under it. Paper palette by day, the unlit room after dark
# (SCORE_THEME=light|dark overrides). Framesync learnings carried over:
# 60 fps, centre-of-interval sampling, rounded scroll at 96 px/beat.
#
#   pop/.venv/bin/python pop/imab/bin/timeline.py
#     → pop/imab/out/imab-aesthetivox-alignment.mp4

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
SYNC_MS = float(os.environ.get("SYNC_MS", "50"))   # measured on synccal: 50 ms locks on this display

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
# one block PER SYLLABLE, each at its own pitch, labeled with the
# syllable actually sung (not the whole word repeated)
SYL_TEXT = {"butterfly": ["but", "ter", "fly"], "flapping": ["flap", "ping"],
            "costume": ["cos", "tume"]}
words = []
for t in targets:
    base = t["label"].split("·")[0]
    k = int(t["label"].split("·")[1]) - 1 if "·" in t["label"] else 0
    label = SYL_TEXT.get(base, [base])[k] if base in SYL_TEXT else base
    words.append({"t": PLACE + t["t"], "dur": max(t["dur"], 0.2), "midi": to_midi(t["note"]),
                  "label": label, "note": t["note"]})

AUD = os.path.join(WORK, ".line.wav")    # the lyricline study mix: click+kick+vox
dur = float(subprocess.run(["ffprobe", "-v", "quiet", "-show_entries", "format=duration",
                            "-of", "default=nw=1:nk=1", AUD], capture_output=True, text=True).stdout.strip())
FRAMES = int(math.ceil(dur * FPS))
TOTAL_BEATS = int(math.ceil(dur / SPB))

r = subprocess.run(["ffmpeg", "-v", "error", "-i", AUD, "-ac", "1", "-ar", "8000", "-f", "f32le", "-"],
                   capture_output=True)
samples = np.frombuffer(r.stdout, np.float32)
rv = subprocess.run(["ffmpeg", "-v", "error", "-i", os.path.join(OUT, "imab-holyvox.wav"),
                     "-ac", "1", "-ar", "8000", "-f", "f32le", "-"], capture_output=True)
voxsamp = np.frombuffer(rv.stdout, np.float32)      # the vocal alone, for per-clip envelopes

F = lambda s: ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", s)
f_title, f_bar, f_word, f_note = F(38), F(28), F(36), F(22)

ROLL_Y0, ROLL_Y1 = 150, 720
LO, HI = 58, 74                          # piano-roll window around C4–C5
ROWH = (ROLL_Y1 - ROLL_Y0) / (HI - LO + 1)
def y_of(midi):
    return ROLL_Y1 - (midi - LO + 0.5) * ROWH
BLACK_PC = {1, 3, 6, 8, 10}
KICK_Y0, KICK_Y1 = 745, 810
WAVE_Y0, WAVE_Y1 = 835, 1045

proc = subprocess.Popen(["ffmpeg", "-hide_banner", "-loglevel", "error", "-y",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
    "-i", AUD, "-map", "0:v", "-map", "1:a",
    "-c:v", "libx264", "-preset", "fast", "-crf", "19", "-c:a", "aac", "-b:a", "192k",
    "-shortest", os.path.join(OUT, "imab-aesthetivox-alignment.mp4")], stdin=subprocess.PIPE)

for i in range(FRAMES):
    t = (i + 0.5) / FPS - SYNC_MS / 1000.0
    img = Image.new("RGB", (W, H), CREAM)
    d = ImageDraw.Draw(img, "RGBA")
    x_of = lambda te: PLAYHEAD_X + round((te - t) * PXS)
    # piano-roll rows: black-key rows tinted, C rows ruled, names at left
    for m in range(LO, HI + 1):
        ry = y_of(m)
        if (m % 12) in BLACK_PC:
            d.rectangle([0, ry - ROWH / 2, W, ry + ROWH / 2], fill=(*INK, 12))
        if m % 12 == 0:
            d.line([0, ry + ROWH / 2, W, ry + ROWH / 2], fill=(*INK, 60), width=1)
        d.text((8, ry - 12), NAMES[m % 12] + str(m // 12 - 1), font=f_note,
               fill=(*INK, 170 if m % 12 == 0 else 80))
    # beat columns, tinted per position in bar; bar numbers
    b0 = int(max(0, (t - PLAYHEAD_X / PXS) / SPB) - 1)
    b1 = int((t + (W - PLAYHEAD_X) / PXS) / SPB) + 1
    for b in range(max(0, b0), min(TOTAL_BEATS, b1)):
        x = x_of(b * SPB)
        d.rectangle([x, ROLL_Y0, x + PXB, ROLL_Y1], fill=BEAT_TINT[b % 4])
        d.line([x, ROLL_Y0, x, KICK_Y1], fill=(*INK, 90 if b % 4 == 0 else 30), width=2 if b % 4 == 0 else 1)
        if b % 4 == 0:
            d.text((x + 8, ROLL_Y0 - 36), f"{b // 4 + 1}", font=f_bar, fill=(*INK, 200))
    # syllable blocks — piano-roll notes with their sung text
    for w in words:
        x0, x1 = x_of(w["t"]), x_of(w["t"] + w["dur"])
        if x1 < -50 or x0 > W + 50: continue
        y = y_of(w["midi"])
        active = w["t"] <= t <= w["t"] + w["dur"]
        hh = ROWH / 2 - 1
        d.rectangle([x0, y - hh, x1, y + hh], fill=GLOW if active else BLOCK,
                    outline=(*PINK_HOT, 255) if active else (*PINK_HOT, 140), width=2)
        # the clip's own waveform, printed inside its block (vocal-stem audio)
        v0 = (w["t"] - PLACE) * 8000
        vspan = w["dur"] * 8000
        for bx in range(int(x0) + 2, int(x1) - 1, 2):
            fa = int(v0 + (bx - x0) / max(1, x1 - x0) * vspan)
            fb = fa + max(2, int(vspan / max(1, x1 - x0) * 2))
            seg = voxsamp[max(0, fa):max(0, fb)]
            if not len(seg): continue
            amp = min(1.0, float(np.sqrt((seg * seg).mean())) * 7)
            wh = amp * (hh - 3)
            d.rectangle([bx, y - wh, bx + 2, y + wh], fill=(*BLUE, 230 if active else 150))
        d.text((x0 + 5, y - hh - 30), w["label"], font=f_word, fill=INK if active else PINK_HOT)
    # kicks on the floor (from the count-in on, every beat)
    for b in range(0, TOTAL_BEATS):
        x = x_of(b * SPB)
        if x < -20 or x > W + 20: continue
        d.rectangle([x + 2, KICK_Y0, x + 26, KICK_Y1], fill=KICK)
    # the waveform rides the bars: same time→x mapping as the grid
    spx = 8000 / PXS                               # audio samples per pixel
    for px_ in range(0, W, 2):
        ts = (px_ - PLAYHEAD_X) / PXS + t
        a = int(ts * 8000)
        if a < 0 or a >= len(samples): continue
        seg = samples[a:a + max(2, int(spx * 2))]
        if not len(seg): continue
        amp = float(np.sqrt((seg * seg).mean())) * 4
        h = min(1.0, amp) * (WAVE_Y1 - WAVE_Y0) / 2
        mid = (WAVE_Y0 + WAVE_Y1) / 2
        col = WAVE if px_ < PLAYHEAD_X else (*INK, 70)
        d.rectangle([px_, mid - h, px_ + 2, mid + h], fill=col)
    # playhead + header
    d.rectangle([PLAYHEAD_X - 2, ROLL_Y0 - 44, PLAYHEAD_X + 2, WAVE_Y1], fill=(*PINK, 235))
    bar_now = int(t / SPB / 4) + 1
    d.text((36, 30), "imab · kick + vocal study · 124", font=f_title, fill=INK)
    d.text((W - 320, 30), f"BAR {max(1, bar_now)}", font=f_title, fill=PINK_HOT)
    proc.stdin.write(img.tobytes())
    if i % 300 == 0: print(f"  {i}/{FRAMES}")
proc.stdin.close(); proc.wait()
print(f"✓ {OUT}/imab-aesthetivox-alignment.mp4")
