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

# ── the loner theme: lights off ───────────────────────────────────────
# @jeffrey: "can we start rendering this in a loner theme / dark mode
# theme now the mp4 please?" The song is someone curled up in a dark
# room, so the study stops looking like a sheet of paper: near-black with
# a blue cast, warm white for her voice, and the margin pink kept as the
# only hot colour — playhead, downbeat, the word being sung.
CREAM = (14, 13, 20)               # the room, lights off
INK = (238, 234, 230)              # warm white — her voice, the type
FAINT = (238, 234, 230, 26)
SOFT = (238, 234, 230, 92)
PINK = (255, 92, 162)
PINK_SOFT = (128, 46, 84)
BLUE = (132, 158, 236)             # lifted for dark ground
PINK_HOT = (255, 176, 214)         # word labels, so they clear the waveform

chart = json.load(open(os.path.join(LANE, "vox4", ".chart.json")))["w-whole-line"]
LINE_BEATS = chart["beats"]
LINE_BARS = math.ceil(LINE_BEATS / 4.0)
PASSES = [0.0]                               # ONE pass, and no count-in:
KICK_BEATS = int((LINE_BARS + 1) * 4)        # the first word IS the downbeat
TOTAL_BEATS = (LINE_BARS + 2) * 4
STRIP_PAD = 2.0                              # beats of strip before beat 0,
                                             # so the pickup has somewhere to live

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
f_tc, f_tc_small, f_beat = M(38), M(22), M(20)
f_lyric = F(30)

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

# @jeffrey: "the words we write on the waveforms don't have enough
# contrast anymore · they should have drop shadows / be coloured". The
# labels sit directly on the waveform, which is the same warm white, so
# they were disappearing into it. Every label is now knocked out with a
# dark halo first — cheap, and it works over waveform, tint or ground.
def shadowed(draw, xy, txt, font, fill, halo=(6, 5, 10, 235), r=2):
    x, y = xy
    for dx in (-r, 0, r):
        for dy in (-r, 0, r):
            if dx or dy:
                draw.text((x + dx, y + dy), txt, font=font, fill=halo)
    draw.text((x, y), txt, font=font, fill=fill)


CHROM = ["A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A"]
def note_name(st):
    return f"{CHROM[st % 12]}{3 + (st + 10) // 12}"

STS = sorted({n["st"] for n in chart["notes"]})
ROLL_TOP, ROLL_BOT = 210, 850
def y_of(st):
    lo, hi = min(STS) - 1, max(STS) + 1
    return ROLL_BOT - (st - lo) / (hi - lo) * (ROLL_BOT - ROLL_TOP)

def label_of(t):
    t = t.replace("·a", "").replace("·b", "")
    return {"myself": "my" , }.get(t, t).lower()
# the split halves get their sung syllables back
SYLL = {"myself·a": "my", "myself·b": "self",
        "sitting·a": "sit", "sitting·b": "ting"}
def word_text(t):
    if t.endswith(("·a", "·b")):
        return SYLL.get(t, t[:-2].lower())
    return t.lower()

# ── the static roll strip (whole song wide), scrolled per frame ───────
STRIP_W = int((TOTAL_BEATS + STRIP_PAD) * PXB) + W
strip = Image.new("RGB", (STRIP_W, H), CREAM)
d = ImageDraw.Draw(strip, "RGBA")
X = lambda beat: int(round((beat + STRIP_PAD) * PXB)) + PLAYHEAD_X  # beat → strip x

# beat + bar grid — @jeffrey: "can we add numbers for each beat in each
# bar in the output · and colour code the columns so we have stronger
# identifiers". Each beat column gets its own tint, cycling 1-2-3-4, so
# the downbeat reads pink and the others step through the paper palette;
# every column is numbered under the bar label. Now a note can be named
# out loud — "bar 3 beat 3" — and found by eye without counting.
BEAT_TINT = [(255, 92, 162, 34),       # 1 — the downbeat
             (132, 158, 236, 14),      # 2
             (238, 234, 230, 10),      # 3
             (132, 158, 236, 14)]      # 4
for b in range(0, TOTAL_BEATS + 1):
    x, xn = X(b), X(b + 1)
    d.rectangle([x, ROLL_TOP, xn - 1, ROLL_BOT], fill=BEAT_TINT[b % 4])
    if b % 4 == 0:
        d.line([(x, ROLL_TOP - 60), (x, ROLL_BOT + 90)], fill=SOFT, width=3)
        d.text((x + 8, ROLL_TOP - 58), f"bar {b // 4}", font=f_bar, fill=INK)
    else:
        d.line([(x, ROLL_TOP), (x, ROLL_BOT)], fill=FAINT, width=1)
    d.text((x + 8, ROLL_TOP - 26), str(b % 4 + 1), font=f_beat,
           fill=PINK if b % 4 == 0 else SOFT)

# pitch guide rows
for st in STS:
    y = y_of(st)
    d.line([(X(0), y), (X(TOTAL_BEATS), y)], fill=FAINT, width=1)
    d.text((X(0) - 84, y - 12), note_name(st), font=f_note, fill=SOFT)

# kick lane
KY0, KY1 = ROLL_BOT + 24, ROLL_BOT + 62
for b in range(KICK_BEATS):
    x = X(b)
    d.rounded_rectangle([x, KY0, x + PXB - 4, KY1], 6, fill=(132, 158, 236, 60))
d.text((X(0) - 84, KY0 + 8), "kick", font=f_note, fill=BLUE)

# word blocks, both passes
BLOCKS = []   # (x0, x1, y0, y1, t_start_s, t_end_s)
for pb in PASSES:
    for n in chart["notes"]:
        b0, b1 = pb + n["beat"], pb + n["beat"] + n["dur"]
        x0, x1 = X(b0), X(b1)
        y = y_of(n["st"])
        y0, y1 = y - BH, y + BH
        # NO INSET: a block's left edge IS its beat. The old +2/+4
        # cosmetic padding drew every word — and the waveform inside
        # it — up to 4 px right of where it sounds, which at 96 px a
        # beat is 20 ms of visible lateness on every single note.
        d.rounded_rectangle([x0, y0, x1 - 1, y1], 10,
                            fill=(255, 92, 162, 46), outline=INK, width=2)
        # the real audio inside the clip: per-column peak of the lead
        # render, normalized to the whole take — dead air is visible
        npx = max(1, x1 - x0)
        env, rms = vox_env(n["beat"], n["beat"] + n["dur"], npx)
        AMP = BH - 4
        for j, (a, r) in enumerate(zip(env, rms)):
            xw = x0 + j
            vb = n["beat"] + n["dur"] * (j + 0.5) / npx
            voiced_here = is_voiced(vb)
            ah = a * AMP
            if ah >= 0.4:                      # peak outline, translucent
                col = (238, 234, 230, 80) if voiced_here else (132, 158, 236, 160)
                d.line([(xw, y - ah), (xw, y + ah)], fill=col, width=1)
            rh = r * AMP * 1.6                 # rms core, solid
            if rh >= 0.4:
                col = (238, 234, 230, 200) if voiced_here else (132, 158, 236, 245)
                d.line([(xw, y - rh), (xw, y + rh)], fill=col, width=1)
        vo = vowel_onset(n["beat"], n["beat"] + n["dur"])
        if vo is not None and vo - n["beat"] > 0.02:   # a real consonant runway
            xv = X(pb + vo)                            # pb: this pass's offset
            d.line([(xv, y0 - 5), (xv, y1 + 5)], fill=BLUE, width=3)
        # @jeffrey: "the notes should be notated over or after the clips,
        # not overlapping · and the words maybe should centre in the clip".
        # The note name used to sit INSIDE the block, on top of the very
        # waveform the block exists to show. It goes above the clip now,
        # the way a chord symbol sits over a staff, and the word centres.
        txt = word_text(n["t"])
        tw = d.textlength(txt, font=f_word)
        lab = (x0 + max(4, (x1 - x0 - tw) / 2), y - 17)
        shadowed(d, lab, txt, f_word, PINK_HOT)
        shadowed(d, (x0 + 2, y0 - 27), note_name(n["st"]), f_note,
                 (238, 234, 230, 200), r=1)
        # media time, not beat time: t=0 is the pickup, beat 0 is LEAD_IN in
        BLOCKS.append((x0, x1, y0, y1, LEAD_IN + b0 * SPB, LEAD_IN + b1 * SPB,
                       txt, lab))

strip_np = np.asarray(strip, dtype=np.uint8)

# ── frames → ffmpeg rawvideo pipe ─────────────────────────────────────
out_mp4 = os.path.join(OUT, "loner-kickvox-timeline.mp4")
ff = subprocess.Popen([
    "ffmpeg", "-y", "-v", "error",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
    "-i", AUD, "-af", "volume=-0.6dB", "-c:v", "libx264", "-preset", "fast", "-crf", "18",
    "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "256k", "-shortest", out_mp4,
], stdin=subprocess.PIPE)

# ── the whistlegraph, in the corner ───────────────────────────────────
# @jeffrey: "can we also print the whistlegraph in the corner of the
# video too?" The whole song is one sung sentence, so the sentence IS
# the graph — printed along the bottom with the word being sung lit, so
# you can always see where in the lyric the playhead is.
LYRIC = [(word_text(n["t"]), LEAD_IN + n["beat"] * SPB,
          LEAD_IN + (n["beat"] + n["dur"]) * SPB) for n in chart["notes"]]
LYRIC_Y = H - 52
CREDIT = "lonr — “Loner” · Camille Klein (@cksuperstore) · the emo whistlegraph"

hdr = Image.new("RGB", (W, ROLL_TOP - 60), CREAM)
dh = ImageDraw.Draw(hdr)
dh.text((40, 26), "loner — kick + vocals study", font=f_title, fill=INK)
sub = ("122 BPM · A# minor @ 237 Hz · the unbroken take, charted   —   "
       "ink = sung vowel · blue = consonant / breath · blue tick = vowel onset")
dh.text((40, 84), sub, font=f_tiny, fill=BLUE)
hdr_np = np.asarray(hdr, dtype=np.uint8)

for i in range(FRAMES):
    t = (i + 0.5) / FPS            # centre of the displayed interval
    beat = (t - LEAD_IN) / SPB               # t=0 is the pickup, not beat 0
    px = int(round((beat + STRIP_PAD) * PXB))   # strip scroll offset, rounded
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
    for (x0, x1, y0, y1, t0, t1, txt, lab) in BLOCKS:
        if t0 <= t < t1:
            lx0, lx1 = x0 - px, x1 - px
            if lx1 > 0 and lx0 < W:
                # outline-only: the waveform inside stays scrutinizable
                d2.rounded_rectangle([lx0, y0, lx1 - 1, y1], 10,
                                     fill=(255, 92, 162, 58), outline=PINK, width=4)
                # the highlight paints over the label baked into the strip,
                # so the word being sung — the one you most want to read —
                # was the faintest on screen. Put it back on top.
                shadowed(d2, (lab[0] - px, lab[1]), txt, f_word, (255, 255, 255))
    # kick flash
    kb = int(math.floor(beat))
    if 0 <= kb < KICK_BEATS and (beat - kb) < 0.22:
        x = X(kb) - px
        d2.rounded_rectangle([x, KY0, x + PXB - 4, KY1], 6, fill=PINK)
    # live timecode — song clock + musical address, updating every frame
    tc = f"{int(t // 60):02d}:{t % 60:05.2f}"
    addr = (f"bar {int(beat) // 4} · beat {beat % 4 + 1:3.1f}" if beat >= 0
            else "pickup")
    d2.text((W - 40 - d2.textlength(tc, font=f_tc), 26), tc, font=f_tc, fill=INK)
    d2.text((W - 40 - d2.textlength(addr, font=f_tc_small), 78), addr,
            font=f_tc_small, fill=BLUE)
    # playhead
    d2.line([(PLAYHEAD_X, ROLL_TOP - 44), (PLAYHEAD_X, KY1 + 8)], fill=PINK, width=4)
    d2.polygon([(PLAYHEAD_X - 10, ROLL_TOP - 56), (PLAYHEAD_X + 10, ROLL_TOP - 56),
                (PLAYHEAD_X, ROLL_TOP - 40)], fill=PINK)
    # the whistlegraph: the whole lyric, with the sung word lit
    lx = 40
    for (wtxt, wt0, wt1) in LYRIC:
        on = wt0 <= t < wt1
        shadowed(d2, (lx, LYRIC_Y), wtxt, f_lyric,
                 PINK if on else (238, 234, 230, 110), r=1)
        lx += d2.textlength(wtxt, font=f_lyric) + 11
    shadowed(d2, (40, LYRIC_Y - 30), CREDIT, f_tiny, (238, 234, 230, 90), r=1)
    # progress
    d2.rectangle([0, H - 10, int(W * t / dur), H], fill=PINK_SOFT)
    ff.stdin.write(np.asarray(img, dtype=np.uint8).tobytes())

ff.stdin.close()
ff.wait()
print(f"✓ {out_mp4} · {FRAMES} frames · {dur:.1f}s")
