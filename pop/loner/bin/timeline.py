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

import json, math, os, subprocess
import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
OUT = os.path.join(LANE, "out")

W, H, FPS = 1920, 1080, 30
BPM = 122.0
SPB = 60.0 / BPM
PXB = 56                      # px per beat
PLAYHEAD_X = 560

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
PASSES = [8.0, 8.0 + LINE_BARS * 4]          # study: line at bar 2 and bar 2+lineBars
KICK_BEATS = int((2 + 2 * LINE_BARS + 1) * 4)
TOTAL_BEATS = (2 + 2 * LINE_BARS + 1 + 3) * 4

MP3 = os.path.join(OUT, "loner-kickvox.mp3")
dur = float(subprocess.run(["ffprobe", "-v", "quiet", "-show_entries", "format=duration",
                            "-of", "default=nw=1:nk=1", MP3],
                           capture_output=True, text=True).stdout.strip())
FRAMES = int(math.ceil(dur * FPS))

F = lambda s: ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", s)
f_title, f_bar, f_word, f_note, f_tiny = F(40), F(26), F(30), F(20), F(22)

CHROM = ["A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A"]
def note_name(st):
    return f"{CHROM[st % 12]}{3 + (st + 10) // 12}"

STS = sorted({n["st"] for n in chart["notes"]})
ROLL_TOP, ROLL_BOT = 200, 830
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
X = lambda beat: int(beat * PXB) + PLAYHEAD_X   # beat → strip x (playhead at song x)

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
        y0, y1 = y - 24, y + 24
        d.rounded_rectangle([x0 + 2, y0, x1 - 3, y1], 10,
                            fill=(255, 166, 202, 90), outline=INK, width=2)
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
    "-i", MP3, "-c:v", "libx264", "-preset", "fast", "-crf", "18",
    "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "256k", "-shortest", out_mp4,
], stdin=subprocess.PIPE)

hdr = Image.new("RGB", (W, ROLL_TOP - 60), CREAM)
dh = ImageDraw.Draw(hdr)
dh.text((40, 26), "loner — kick + vocals study", font=f_title, fill=INK)
sub = "122 BPM · A# minor @ 237 Hz · the unbroken take, charted"
dh.text((40, 84), sub, font=f_tiny, fill=BLUE)
hdr_np = np.asarray(hdr, dtype=np.uint8)

for i in range(FRAMES):
    t = i / FPS
    beat = t / SPB
    px = int(beat * PXB)                     # strip scroll offset
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
                d2.rounded_rectangle([lx0 + 2, y0, lx1 - 3, y1], 10,
                                     fill=(255, 102, 168, 120), outline=PINK, width=3)
    # kick flash
    kb = int(beat)
    if kb < KICK_BEATS and (beat - kb) < 0.22:
        x = X(kb) - px
        d2.rounded_rectangle([x + 4, KY0, x + PXB - 8, KY1], 6, fill=PINK)
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
