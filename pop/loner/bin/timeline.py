# timeline.py — the labeled graphic timeline of the kick+vocals study.
#
# @jeffrey: "can u make an mp4 / graphic timeline where beats and bars
# and utterances are coded and labeled so we can start to scrutinize
# things a bit better". A scrolling piano roll of the MINIMAL render:
# every word of the unbroken take as a block at its chart slot and sung
# pitch, bars numbered, beats gridded, kicks ticking along the floor,
# a fixed playhead with the roll sliding under it in sync with
# out/loner-kickvox.mp3. Paper palette (cream, ink, margin pink).
#
#   pop/.venv/bin/python pop/loner/bin/timeline.py
#     → out/loner-kickvox-timeline.mp4

import json, math, os, subprocess, wave
import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
OUT = os.path.join(LANE, "out")

W, H, FPS = 1920, 1080, 60
BPM = 122.0
SPB = 60.0 / BPM
PXB = 96                      # px per beat — wider blocks AND finer sync
PLAYHEAD_X = 560
BH = 30                       # block half-height

# FRAMESYNC. @jeffrey: "the video feels a bit off from the audio · can we
# try and ensure an excellent framesync". The WAV is beat-accurate (its
# kicks land within 2 ms of the grid), so the error was all on the video
# side, and both sources pushed the same way — the picture lagged.
#   · frames were sampled at t = i/FPS, the START of the interval each
#     frame is displayed over, so the image was on average half a frame
#     stale (16.7 ms at 30). Sampling the CENTRE removes that bias.
#   · the scroll floored to whole pixels, which always scrolls too
#     little, so content arrived late by up to one pixel (8.8 ms at the
#     old 56 px/beat). Rounding halves it and 96 px/beat halves it again.
# 60 fps + centre sampling + rounded scroll at 96 px/beat: worst case
# ±8.3 ms of frame quantisation and ±5.2 ms of pixel, with zero bias.

CREAM = (255, 253, 246)
INK = (26, 26, 34)
FAINT = (26, 26, 34, 26)
SOFT = (26, 26, 34, 64)
PINK = (255, 102, 168)
PINK_SOFT = (255, 166, 202)
BLUE = (92, 118, 180)

chart = json.load(open(os.path.join(LANE, "vox4", ".chart.json")))["w-whole-line"]
LINE_BEATS = chart["beats"]
LINE_BARS = math.ceil(LINE_BEATS / 4.0)
PASSES = [8.0]                               # ONE pass — the study, not a loop
KICK_BEATS = int((2 + LINE_BARS + 1) * 4)
TOTAL_BEATS = (2 + LINE_BARS + 2) * 4

# mux straight from the render WAV — the mp3's LAME/AAC codec-delay
# padding shifted the audio ~50 ms late against the graphics
AUD = os.path.join(OUT, "loner-kickvox-full.wav")
dur = float(subprocess.run(["ffprobe", "-v", "quiet", "-show_entries", "format=duration",
                            "-of", "default=nw=1:nk=1", AUD],
                           capture_output=True, text=True).stdout.strip())
FRAMES = int(math.ceil(dur * FPS))

F = lambda s: ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", s)
f_title, f_bar, f_word, f_note, f_tiny = F(40), F(28), F(34), F(22), F(22)
M = lambda s: ImageFont.truetype("/System/Library/Fonts/Menlo.ttc", s)
f_tc, f_tc_small = M(38), M(22)

# ── the actual sung waveform, so the eye can audit the trim ───────────
# @jeffrey: "check the length of the actual waveforms in the utterances,
# not just ur trim etc — map / render those waveforms directly into the
# clips". The lead render (vox4/w-whole-line.wav) IS what the study
# plays; chart beat b lives at leadIn + b·SPB seconds in that file.
with wave.open(os.path.join(LANE, "vox4", "w-whole-line.wav"), "rb") as wf:
    VFS = wf.getframerate()
    VOX = np.frombuffer(wf.readframes(wf.getnframes()),
                        dtype=np.int16).astype(np.float64) / 32768.0
VOX_PEAK = np.max(np.abs(VOX)) or 1.0
LEAD_IN = chart["leadIn"]

# ── vowel vs consonant, in colour ─────────────────────────────────────
# @jeffrey: "make sure for each sample we know when the vowel /
# consonant / voicing starts · and lets mark that in colour in the mp4".
# halo3 emits the voiced runs of each phrase, in beats on the render's
# own timeline. Voiced = the sung vowel (drawn in the ink of the roll);
# everything else inside a word is consonant or breath (drawn in blue),
# and a blue tick marks the vowel onset — the frame the warp puts ON the
# beat, with the consonant running 1:1 ahead of it.
VOICED = chart.get("voiced", [])


def is_voiced(beat):
    for a, b in VOICED:
        if a <= beat < b:
            return True
        if a > beat:
            break
    return False


def vowel_onset(b0, b1):
    """First voiced beat inside a word's slot, if it starts on a consonant."""
    for a, b in VOICED:
        if b0 - 0.5 <= a < b1:
            return a
    return None


def vox_env(b0, b1, npx):
    """Per-column peak AND rms of the lead render between chart beats.
    Two traces instead of one: the peak outline says how loud the word
    gets, the rms core says how much of the column is actually sound —
    a decaying tail reads as a hollow outline, a solid vowel as a filled
    body. At 96 px/beat that is a column every 5 ms."""
    s0 = int((LEAD_IN + b0 * SPB) * VFS)
    s1 = int((LEAD_IN + b1 * SPB) * VFS)
    seg = VOX[max(0, s0):max(0, s1)]
    if len(seg) == 0 or npx <= 0:
        z = np.zeros(max(1, npx))
        return z, z
    e = np.linspace(0, len(seg), npx + 1).astype(int)
    pk = np.array([np.abs(seg[a:b]).max() if b > a else 0.0
                   for a, b in zip(e[:-1], e[1:])]) / VOX_PEAK
    rm = np.array([np.sqrt((seg[a:b] ** 2).mean()) if b > a else 0.0
                   for a, b in zip(e[:-1], e[1:])]) / VOX_PEAK
    return pk, rm

CHROM = ["A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A"]
def note_name(st):
    return f"{CHROM[st % 12]}{3 + (st + 10) // 12}"

STS = sorted({n["st"] for n in chart["notes"]})
ROLL_TOP, ROLL_BOT = 190, 900
def y_of(st):
    lo, hi = min(STS) - 1, max(STS) + 1
    return ROLL_BOT - (st - lo) / (hi - lo) * (ROLL_BOT - ROLL_TOP)

def label_of(t):
    t = t.replace("·a", "").replace("·b", "")
    return {"myself": "my" , }.get(t, t).lower()
# the split halves get their sung syllables back
def word_text(t):
    if t.endswith("·a"): return {"myself·a": "my"}.get(t, t[:-2].lower())
    if t.endswith("·b"): return {"myself·b": "self"}.get(t, t[:-2].lower())
    return t.lower()

# ── the static roll strip (whole song wide), scrolled per frame ───────
STRIP_W = int(TOTAL_BEATS * PXB) + W
strip = Image.new("RGB", (STRIP_W, H), CREAM)
d = ImageDraw.Draw(strip, "RGBA")
X = lambda beat: int(round(beat * PXB)) + PLAYHEAD_X   # beat → strip x

# beat + bar grid
for b in range(0, TOTAL_BEATS + 1):
    x = X(b)
    if b % 4 == 0:
        d.line([(x, ROLL_TOP - 40), (x, ROLL_BOT + 90)], fill=SOFT, width=2)
        d.text((x + 8, ROLL_TOP - 38), f"bar {b // 4 - 2}" if b >= 8 else
               ("count-in" if b == 0 else ""), font=f_bar, fill=INK)
    else:
        d.line([(x, ROLL_TOP), (x, ROLL_BOT)], fill=FAINT, width=1)

# pitch guide rows
for st in STS:
    y = y_of(st)
    d.line([(X(0), y), (X(TOTAL_BEATS), y)], fill=FAINT, width=1)
    d.text((X(0) - 84, y - 12), note_name(st), font=f_note, fill=SOFT)

# kick lane
KY0, KY1 = ROLL_BOT + 34, ROLL_BOT + 78
for b in range(KICK_BEATS):
    x = X(b)
    d.rounded_rectangle([x + 4, KY0, x + PXB - 8, KY1], 6, fill=(92, 118, 180, 70))
d.text((X(0) - 84, KY0 + 8), "kick", font=f_note, fill=BLUE)

# word blocks, both passes
BLOCKS = []   # (x0, x1, y0, y1, t_start_s, t_end_s)
for pb in PASSES:
    for n in chart["notes"]:
        b0, b1 = pb + n["beat"], pb + n["beat"] + n["dur"]
        x0, x1 = X(b0), X(b1)
        y = y_of(n["st"])
        y0, y1 = y - BH, y + BH
        d.rounded_rectangle([x0 + 2, y0, x1 - 3, y1], 10,
                            fill=(255, 166, 202, 90), outline=INK, width=2)
        # the real audio inside the clip: per-column peak of the lead
        # render, normalized to the whole take — dead air is visible
        npx = max(1, x1 - x0 - 8)
        env, rms = vox_env(n["beat"], n["beat"] + n["dur"], npx)
        AMP = BH - 4
        for j, (a, r) in enumerate(zip(env, rms)):
            xw = x0 + 4 + j
            vb = n["beat"] + n["dur"] * (j + 0.5) / npx
            voiced_here = is_voiced(vb)
            ah = a * AMP
            if ah >= 0.4:                      # peak outline, translucent
                col = (26, 26, 34, 70) if voiced_here else (92, 118, 180, 150)
                d.line([(xw, y - ah), (xw, y + ah)], fill=col, width=1)
            rh = r * AMP * 1.6                 # rms core, solid
            if rh >= 0.4:
                col = (26, 26, 34, 170) if voiced_here else (92, 118, 180, 235)
                d.line([(xw, y - rh), (xw, y + rh)], fill=col, width=1)
        vo = vowel_onset(n["beat"], n["beat"] + n["dur"])
        if vo is not None and vo - n["beat"] > 0.02:   # a real consonant runway
            xv = X(pb + vo)                            # pb: this pass's offset
            d.line([(xv, y0 - 5), (xv, y1 + 5)], fill=BLUE, width=3)
        txt = word_text(n["t"])
        tw = d.textlength(txt, font=f_word)
        wide = (x1 - x0) > tw + 20
        d.text((x0 + (10 if wide else 4), y - 17), txt, font=f_word, fill=INK)
        if (x1 - x0) > tw + 70:
            d.text((x0 + 14 + tw + 8, y - 10), note_name(n["st"]), font=f_note, fill=SOFT)
        BLOCKS.append((x0, x1, y0, y1, b0 * SPB, b1 * SPB))

strip_np = np.asarray(strip, dtype=np.uint8)

# ── frames → ffmpeg rawvideo pipe ─────────────────────────────────────
out_mp4 = os.path.join(OUT, "loner-kickvox-timeline.mp4")
ff = subprocess.Popen([
    "ffmpeg", "-y", "-v", "error",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
    "-i", AUD, "-af", "volume=-0.6dB", "-c:v", "libx264", "-preset", "fast", "-crf", "18",
    "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "256k", "-shortest", out_mp4,
], stdin=subprocess.PIPE)

hdr = Image.new("RGB", (W, ROLL_TOP - 60), CREAM)
dh = ImageDraw.Draw(hdr)
dh.text((40, 26), "loner — kick + vocals study", font=f_title, fill=INK)
sub = ("122 BPM · A# minor @ 237 Hz · the unbroken take, charted   —   "
       "ink = sung vowel · blue = consonant / breath · blue tick = vowel onset")
dh.text((40, 84), sub, font=f_tiny, fill=BLUE)
hdr_np = np.asarray(hdr, dtype=np.uint8)

for i in range(FRAMES):
    t = (i + 0.5) / FPS            # centre of the displayed interval
    beat = t / SPB
    px = int(round(beat * PXB))              # strip scroll offset, rounded
    fr = strip_np[:, px:px + W].copy()
    if fr.shape[1] < W:
        pad = np.full((H, W - fr.shape[1], 3), CREAM, dtype=np.uint8)
        fr = np.concatenate([fr, pad], axis=1)
    img = Image.fromarray(fr)
    d2 = ImageDraw.Draw(img, "RGBA")
    # header
    img.paste(Image.fromarray(hdr_np), (0, 0))
    d2.text((40, 26), "loner — kick + vocals study", font=f_title, fill=INK)
    d2.text((40, 84), sub, font=f_tiny, fill=BLUE)
    # active word glow
    for (x0, x1, y0, y1, t0, t1) in BLOCKS:
        if t0 <= t < t1:
            lx0, lx1 = x0 - px, x1 - px
            if lx1 > 0 and lx0 < W:
                # outline-only: the waveform inside stays scrutinizable
                d2.rounded_rectangle([lx0 + 2, y0, lx1 - 3, y1], 10,
                                     fill=(255, 102, 168, 40), outline=PINK, width=4)
    # kick flash
    kb = int(beat)
    if kb < KICK_BEATS and (beat - kb) < 0.22:
        x = X(kb) - px
        d2.rounded_rectangle([x + 4, KY0, x + PXB - 8, KY1], 6, fill=PINK)
    # live timecode — song clock + musical address, updating every frame
    tc = f"{int(t // 60):02d}:{t % 60:05.2f}"
    bar_no = int(beat) // 4 - 2
    addr = (f"bar {bar_no} · beat {beat % 4 + 1:3.1f}" if beat >= 8
            else f"count-in · beat {beat % 8 + 1:3.1f}")
    d2.text((W - 40 - d2.textlength(tc, font=f_tc), 26), tc, font=f_tc, fill=INK)
    d2.text((W - 40 - d2.textlength(addr, font=f_tc_small), 78), addr,
            font=f_tc_small, fill=BLUE)
    # playhead
    d2.line([(PLAYHEAD_X, ROLL_TOP - 44), (PLAYHEAD_X, KY1 + 8)], fill=PINK, width=4)
    d2.polygon([(PLAYHEAD_X - 10, ROLL_TOP - 56), (PLAYHEAD_X + 10, ROLL_TOP - 56),
                (PLAYHEAD_X, ROLL_TOP - 40)], fill=PINK)
    # progress
    d2.rectangle([0, H - 10, int(W * t / dur), H], fill=PINK_SOFT)
    ff.stdin.write(np.asarray(img, dtype=np.uint8).tobytes())

ff.stdin.close()
ff.wait()
print(f"✓ {out_mp4} · {FRAMES} frames · {dur:.1f}s")
