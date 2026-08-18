# timeline.py — the labeled graphic timeline of the press+words study.
#
# A scrolling piano roll of the MINIMAL render: every unit of the unbroken
# chant as a block at its chart slot and sung pitch, bars numbered, beats
# gridded and colour-coded, the press ticking along the floor, a fixed
# playhead with the roll sliding under it in sync with the render WAV.
# Paper palette (cream, ink, margin pink) by day; the factory floor after
# dark. The point is to be able to SEE a bad boundary — the real waveform
# is drawn inside each block, so dead air, a clipped attack or a syllable
# sitting in the wrong slot is visible without listening.
#
#   python3 pop/factory/bin/timeline.py
#     → out/factory-kickvox-timeline.mp4

import datetime, json, math, multiprocessing, os, subprocess, wave
import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
OUT = os.path.join(LANE, "out")
PHRASE = "f-whole-poem"

W, H, FPS = 1920, 2160, 60
BPM = 100.0
SPB = 60.0 / BPM
PXB = 96                      # px per beat — wide blocks AND fine sync
PLAYHEAD_X = 560
BH = 56                       # block half-height

# FRAMESYNC. The WAV is beat-exact (the study mix's presses measure 0.0 ms
# off the grid), so any felt lag is on the video side, and both sources
# push the same way — the picture arrives late.
#   · sampling a frame at t = i/FPS is the START of the interval it is
#     displayed over, so the image is on average half a frame stale.
#     Sampling the CENTRE removes that bias.
#   · flooring the scroll to whole pixels always scrolls too little, so
#     content arrives late by up to a pixel.
# 60 fps + centre sampling + sub-pixel scroll at 96 px/beat: ±8.3 ms of
# frame quantisation and no pixel bias at all.

# ── the theme: it follows the clock ───────────────────────────────────
# A score video is read, not watched, so it wants to match the room.
# Daylight gets the archive's paper; after dark it becomes the factory
# floor with the lights off. Override with SCORE_THEME=light|dark.
THEME = os.environ.get("SCORE_THEME") or (
    "light" if 7 <= datetime.datetime.now().hour < 19 else "dark")

if THEME == "light":
    CREAM = (255, 253, 246)
    INK = (26, 26, 34)
    FAINT = (26, 26, 34, 26)
    SOFT = (26, 26, 34, 74)
    PINK = (255, 82, 156)
    PINK_SOFT = (255, 176, 210)
    BLUE = (72, 100, 172)
    PINK_HOT = (188, 30, 104)
    HALO = (255, 253, 246, 232)
    LABEL_ON = (26, 26, 34)
    BLOCK_FILL = (255, 166, 202, 86)
    GLOW_FILL = (255, 82, 156, 62)
    WAVE_PK, WAVE_RMS = (26, 26, 34, 74), (26, 26, 34, 185)
    CONS_PK, CONS_RMS = (72, 100, 172, 150), (72, 100, 172, 235)
    KICK_FILL = (72, 100, 172, 62)
    LYRIC_OFF = (26, 26, 34, 104)
    LINE_TINT = (72, 100, 172, 16)
    BEAT_TINT = [(255, 82, 156, 30), (72, 100, 172, 14),
                 (26, 26, 34, 10), (72, 100, 172, 14)]
else:
    CREAM = (13, 14, 18)
    INK = (236, 234, 228)
    FAINT = (236, 234, 228, 26)
    SOFT = (236, 234, 228, 92)
    PINK = (255, 92, 162)
    PINK_SOFT = (128, 46, 84)
    BLUE = (132, 158, 236)
    PINK_HOT = (255, 176, 214)
    HALO = (6, 6, 9, 235)
    LABEL_ON = (255, 255, 255)
    BLOCK_FILL = (255, 92, 162, 46)
    GLOW_FILL = (255, 92, 162, 58)
    WAVE_PK, WAVE_RMS = (236, 234, 228, 80), (236, 234, 228, 200)
    CONS_PK, CONS_RMS = (132, 158, 236, 160), (132, 158, 236, 245)
    KICK_FILL = (132, 158, 236, 60)
    LYRIC_OFF = (236, 234, 228, 110)
    LINE_TINT = (132, 158, 236, 20)
    BEAT_TINT = [(255, 92, 162, 34), (132, 158, 236, 14),
                 (236, 234, 228, 10), (132, 158, 236, 14)]

chart = json.load(open(os.path.join(LANE, "vox3", ".chart.json")))[PHRASE]
LINE_BEATS = chart["beats"]
LINE_BARS = math.ceil(LINE_BEATS / 4.0)
PASSES = [0.0]                               # ONE pass, and no count-in
BARS_MAX = 16
KICK_BEATS = int(min(LINE_BARS + 1, BARS_MAX) * 4)
TOTAL_BEATS = min(LINE_BARS + 2, BARS_MAX) * 4
SYNC_MS = float(os.environ.get("SYNC_MS", "45"))   # display-latency lead
STRIP_PAD = 2.0                              # beats of strip before beat 0

# mux straight from the render WAV — an mp3/AAC round trip prepends codec
# delay, which reads as the whole video being 40–50 ms out
AUD = os.path.join(OUT, "factory-kickvox-full.wav")
dur = float(subprocess.run(["ffprobe", "-v", "quiet", "-show_entries", "format=duration",
                            "-of", "default=nw=1:nk=1", AUD],
                           capture_output=True, text=True).stdout.strip())
FRAMES = int(math.ceil(dur * FPS))
# The roll must reach the end of the AUDIO, not just the end of the chart:
# the engine leaves two seconds for the bird to ring, and without this the
# last of it scrolled off a cream cliff with no grid under it.
TOTAL_BEATS = max(TOTAL_BEATS, int(math.ceil(dur / SPB)))

F = lambda s: ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", s)
f_title, f_bar, f_word, f_note, f_tiny = F(40), F(28), F(32), F(22), F(22)
M = lambda s: ImageFont.truetype("/System/Library/Fonts/Menlo.ttc", s)
f_tc, f_tc_small, f_beat = M(38), M(22), M(20)
f_lyric = F(30)

# ── the actual sung waveform, so the eye can audit the trim ───────────
# The lead render (vox3/f-whole-poem.wav) IS what the study plays; chart
# beat b lives at leadIn + b·SPB seconds inside it.
with wave.open(os.path.join(LANE, "vox3", f"{PHRASE}.wav"), "rb") as wf:
    VFS = wf.getframerate()
    VOX = np.frombuffer(wf.readframes(wf.getnframes()),
                        dtype=np.int16).astype(np.float64) / 32768.0
VOX_PEAK = np.max(np.abs(VOX)) or 1.0
LEAD_IN = chart["leadIn"]

# ── vowel vs consonant, in colour ─────────────────────────────────────
# chart.py emits the voiced runs of the phrase, in beats on the render's
# own timeline. Voiced = the sung vowel (ink); everything else inside a
# word is consonant or breath (blue), and a blue tick marks the vowel
# onset — the frame the warp puts ON the beat, with the consonant running
# 1:1 ahead of it.
VOICED = chart.get("voiced", [])


def is_voiced(beat):
    for a, b in VOICED:
        if a <= beat < b:
            return True
        if a > beat:
            break
    return False


def vowel_onset(b0, b1):
    for a, b in VOICED:
        if b0 - 0.5 <= a < b1:
            return a
    return None


def vox_env(b0, b1, npx):
    """Per-column peak AND rms of the lead render between chart beats.
    The peak outline says how loud the word gets; the rms core says how
    much of the column is actually sound — a decaying tail reads as a
    hollow outline, a solid vowel as a filled body. At 96 px a beat that
    is a column every 6 ms."""
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


# Labels sit directly on the waveform, which is the same warm white, so
# they disappear into it. Every label is knocked out with a halo first —
# cheap, and it works over waveform, tint or ground.
def shadowed(draw, xy, txt, font, fill, halo=None, r=2):
    halo = HALO if halo is None else halo
    x, y = xy
    for dx in (-r, 0, r):
        for dy in (-r, 0, r):
            if dx or dy:
                draw.text((x + dx, y + dy), txt, font=font, fill=halo)
    draw.text((x, y), txt, font=font, fill=fill)


# Note names in HER key: 0 is D, because the tonic is her D and reading
# the roll in D is reading it in the poem's own key.
CHROM = ["D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B", "C", "C#"]


def note_name(st):
    return f"{CHROM[st % 12]}{3 + (st + 2) // 12}"


STS = sorted({n["st"] for n in chart["notes"]})
ROLL_TOP, ROLL_BOT = 300, 1900


def y_of(st):
    lo, hi = min(STS) - 1, max(STS) + 1
    return ROLL_BOT - (st - lo) / (hi - lo) * (ROLL_BOT - ROLL_TOP)


def word_text(t):
    return t.lower()


# the three lines of the poem, for the section bands across the roll
LINES = [(0.0, 8.0, "factory / cookie cutter / personalities"),
         (8.0, 18.0, "we must break free from the states that we're in"),
         (20.0, LINE_BEATS, "spinning away, I hear a bird")]

# ── the static roll strip (whole study wide), scrolled per frame ──────
STRIP_W = int((TOTAL_BEATS + STRIP_PAD) * PXB) + W
strip = Image.new("RGB", (STRIP_W, H), CREAM)
d = ImageDraw.Draw(strip, "RGBA")
X = lambda beat: int(round((beat + STRIP_PAD) * PXB)) + PLAYHEAD_X

# the three lines of the poem as bands behind the roll — the record's form
for (a, b, txt) in LINES:
    d.rectangle([X(a), ROLL_TOP - 8, X(b) - 1, ROLL_BOT + 8], fill=LINE_TINT)
    d.text((X(a) + 10, ROLL_BOT + 130), txt, font=f_tiny, fill=SOFT)

# beat + bar grid. Each beat column gets its own tint, cycling 1-2-3-4, so
# the downbeat reads pink and the others step through the palette; every
# column is numbered under the bar label, so a note can be named out loud
# — "bar 3 beat 3" — and found by eye without counting.
for b in range(-1, TOTAL_BEATS + 1):
    x, xn = X(b), X(b + 1)
    d.rectangle([x, ROLL_TOP, xn - 1, ROLL_BOT], fill=BEAT_TINT[b % 4])
    if b % 4 == 0:
        d.line([(x, ROLL_TOP - 60), (x, ROLL_BOT + 90)], fill=SOFT, width=3)
        d.text((x + 8, ROLL_TOP - 58), f"bar {b // 4}", font=f_bar, fill=INK)
    elif b < 0:
        d.line([(x, ROLL_TOP - 60), (x, ROLL_BOT + 90)], fill=FAINT, width=2)
        d.text((x + 8, ROLL_TOP - 58), "pickup", font=f_bar, fill=SOFT)
    else:
        d.line([(x, ROLL_TOP), (x, ROLL_BOT)], fill=FAINT, width=1)
    d.text((x + 8, ROLL_TOP - 26), str(b % 4 + 1), font=f_beat,
           fill=PINK if b % 4 == 0 else SOFT)

# pitch guide rows
for st in STS:
    y = y_of(st)
    d.line([(X(0), y), (X(TOTAL_BEATS), y)], fill=FAINT, width=1)
    d.text((X(0) - 84, y - 12), note_name(st), font=f_note, fill=SOFT)

# the press lane. It starts at beat 0, not at the pickup: the engine puts
# no press on the pickup (her /f/ has it to itself), and a drawn block
# where nothing strikes is the graphic lying about the mix.
KY0, KY1 = ROLL_BOT + 40, ROLL_BOT + 104
for b in range(0, KICK_BEATS):
    x = X(b)
    d.rounded_rectangle([x, KY0, x + PXB - 4, KY1], 6, fill=KICK_FILL)
d.text((X(0) - 90, KY0 + 8), "press", font=f_note, fill=BLUE)

# word blocks
BLOCKS = []
for pb in PASSES:
    for n in chart["notes"]:
        b0, b1 = pb + n["beat"], pb + n["beat"] + n["dur"]
        x0, x1 = X(b0), X(b1)
        y = y_of(n["st"])
        y0, y1 = y - BH, y + BH
        # NO INSET: a block's left edge IS its beat. A cosmetic +2/+4 pad
        # would draw every word — and the waveform inside it — right of
        # where it sounds, which at 96 px a beat is 20 ms of visible
        # lateness on every single note.
        d.rounded_rectangle([x0, y0, x1 - 1, y1], 10,
                            fill=BLOCK_FILL, outline=INK, width=2)
        npx = max(1, x1 - x0)
        env, rms = vox_env(n["beat"], n["beat"] + n["dur"], npx)
        AMP = BH - 4
        for j, (a, r) in enumerate(zip(env, rms)):
            xw = x0 + j
            vb = n["beat"] + n["dur"] * (j + 0.5) / npx
            voiced_here = is_voiced(vb)
            ah = a * AMP
            if ah >= 0.4:
                d.line([(xw, y - ah), (xw, y + ah)],
                       fill=WAVE_PK if voiced_here else CONS_PK, width=1)
            rh = r * AMP * 1.6
            if rh >= 0.4:
                d.line([(xw, y - rh), (xw, y + rh)],
                       fill=WAVE_RMS if voiced_here else CONS_RMS, width=1)
        vo = vowel_onset(n["beat"], n["beat"] + n["dur"])
        if vo is not None and vo - n["beat"] > 0.02:
            xv = X(pb + vo)
            d.line([(xv, y0 - 5), (xv, y1 + 5)], fill=BLUE, width=3)
        # The note name sits ABOVE the clip, the way a chord symbol sits
        # over a staff, so it never covers the waveform the block exists
        # to show; the word centres in the clip.
        txt = word_text(n["t"])
        tw = d.textlength(txt, font=f_word)
        lab = (x0 + max(4, (x1 - x0 - tw) / 2), y - 16)
        shadowed(d, lab, txt, f_word, PINK_HOT)
        shadowed(d, (x0 + 2, y0 - 27), note_name(n["st"]), f_note, INK, r=1)
        # media time, not beat time: t=0 is the pickup, beat 0 is LEAD_IN in
        BLOCKS.append((x0, x1, y0, y1, LEAD_IN + b0 * SPB, LEAD_IN + b1 * SPB,
                       txt, lab))

strip_np = np.asarray(strip, dtype=np.uint8)

out_mp4 = os.path.join(OUT, "factory-kickvox-timeline.mp4")
SEGDIR = os.path.join(OUT, ".segments")


def encode_segment(job):
    """Render a contiguous run of frames and encode it to its own file.

    Handing frames back to the parent meant piping gigabytes of raw RGB
    through IPC, and the parent could only write it one core's worth at a
    time — so adding workers stopped helping. Each worker owns an ffmpeg
    now; nothing crosses a process boundary but a filename, and the ENCODE
    parallelises along with the drawing.
    """
    k, a, b = job
    seg = os.path.join(SEGDIR, f"seg{k:03d}.mp4")
    p = subprocess.Popen(
        ["ffmpeg", "-y", "-v", "error", "-f", "rawvideo", "-pix_fmt", "rgb24",
         "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-", "-c:v", "libx264",
         "-preset", "veryfast", "-crf", "19", "-pix_fmt", "yuv420p",
         "-threads", "1", seg], stdin=subprocess.PIPE)
    for i in range(a, b):
        p.stdin.write(render_frame(i))
    p.stdin.close()
    p.wait()
    return seg


# ── the whistlegraph, in the corner ───────────────────────────────────
# The whole record is one sung poem, so the poem IS the graph — printed
# along the bottom with the word being sung lit, so you can always see
# where in the lyric the playhead is.
LYRIC = [(word_text(n["t"]), LEAD_IN + n["beat"] * SPB,
          LEAD_IN + (n["beat"] + n["dur"]) * SPB) for n in chart["notes"]]
LYRIC_Y = H - 64
# No emoji in the credit. The work's title carries two (🏭 and 🎄) and
# Helvetica has neither, so they rendered as tofu boxes — a glyph that
# says nothing, twice, in the one line that names the source.
CREDIT = ("fact — “factory cookie-cutter personalities” · "
          "Whistlegraph, 2021-02-04 · take a, the unbroken chant")

# Pre-rendered ONCE. Drawing this per frame meant every word through
# shadowed(), which is nine text passes each — hundreds of draw calls a
# frame, for a strip that only ever changes which single word is lit.
LYR_X, _lx = [], 40
_lyr = Image.new("RGB", (W, 96), CREAM)
_ld = ImageDraw.Draw(_lyr, "RGBA")
_ld.text((0, 0), CREDIT, font=f_tiny, fill=SOFT)
for n in chart["notes"]:
    _w = word_text(n["t"])
    LYR_X.append(_lx)
    _ld.text((_lx - 40, 30), _w, font=f_lyric, fill=LYRIC_OFF)
    _lx += _ld.textlength(_w, font=f_lyric) + 11
LYR_NP = np.asarray(_lyr, dtype=np.uint8)

hdr = Image.new("RGB", (W, ROLL_TOP - 60), CREAM)
dh = ImageDraw.Draw(hdr)
dh.text((40, 26), "factory — press + words study", font=f_title, fill=INK)
sub = ("100 BPM · D minor @ 148.73 Hz (her D) · the unbroken chant, charted   —   "
       "ink = sung vowel · blue = consonant / breath · blue tick = vowel onset")
dh.text((40, 84), sub, font=f_tiny, fill=BLUE)
hdr_np = np.asarray(hdr, dtype=np.uint8)


def render_frame(i):
    # SYNC_MS — display latency compensation, not a file fix. The file is
    # exact: both streams start at PTS 0 and the study mix's presses
    # measure 0.0 ms off the beat grid. The remaining offset is downstream
    # — an LCD and a compositor and a player buffer put the picture on the
    # retina one to three frames after the sound reaches the ear, so sound
    # always arrives first. Drawing the state of (t + SYNC_MS) at media
    # time t hands the picture that head start back. SYNC_MS=0 shows the
    # raw file.
    t = (i + 0.5) / FPS + SYNC_MS / 1000.0
    beat = (t - LEAD_IN) / SPB
    # SUB-PIXEL SCROLL. Rounding the scroll to a whole pixel leaves up to
    # half a pixel of error — 2.6 ms at 96 px a beat — on every frame, and
    # it is the only part of the geometry that is removable: the rest is
    # the 60 fps frame grid itself. Crop at the floor and blend the two
    # neighbouring columns by the fraction.
    fx = (beat + STRIP_PAD) * PXB
    px = int(math.floor(fx))
    frac = fx - px
    seg = strip_np[:, px:px + W + 1]
    if seg.shape[1] < W + 1:
        pad = np.full((H, W + 1 - seg.shape[1], 3), CREAM, dtype=np.uint8)
        seg = np.concatenate([seg, pad], axis=1)
    # fixed-point blend: int16 is half the memory traffic of float32, and
    # this loop is bandwidth-bound
    f8 = int(frac * 256.0 + 0.5)
    a16 = seg[:, :W].astype(np.int16)
    fr = a16 if f8 == 0 else (
        a16 + (((seg[:, 1:W + 1].astype(np.int16) - a16) * f8) >> 8))
    img = Image.fromarray(fr.astype(np.uint8))
    d2 = ImageDraw.Draw(img, "RGBA")
    img.paste(Image.fromarray(hdr_np), (0, 0))
    for (x0, x1, y0, y1, t0, t1, txt, lab) in BLOCKS:
        if t0 <= t < t1:
            lx0, lx1 = x0 - px, x1 - px
            if lx1 > 0 and lx0 < W:
                # outline-only: the waveform inside stays scrutinizable
                d2.rounded_rectangle([lx0, y0, lx1 - 1, y1], 10,
                                     fill=GLOW_FILL, outline=PINK, width=4)
                # the highlight paints over the label baked into the strip,
                # so the word being sung — the one you most want to read —
                # was the faintest on screen. Put it back on top.
                shadowed(d2, (lab[0] - px, lab[1]), txt, f_word, LABEL_ON)
    kb = int(math.floor(beat))
    if 0 <= kb < KICK_BEATS and (beat - kb) < 0.22:
        x = X(kb) - px
        d2.rounded_rectangle([x, KY0, x + PXB - 4, KY1], 6, fill=PINK)
    tc = f"{int(t // 60):02d}:{t % 60:05.2f}"
    addr = (f"bar {int(beat) // 4} · beat {beat % 4 + 1:3.1f}" if beat >= 0
            else "pickup")
    d2.text((W - 40 - d2.textlength(tc, font=f_tc), 26), tc, font=f_tc, fill=INK)
    d2.text((W - 40 - d2.textlength(addr, font=f_tc_small), 78), addr,
            font=f_tc_small, fill=BLUE)
    d2.rectangle([PLAYHEAD_X - 1, ROLL_TOP - 44, PLAYHEAD_X, KY1 + 8], fill=PINK)
    d2.polygon([(PLAYHEAD_X - 10, ROLL_TOP - 56), (PLAYHEAD_X + 10, ROLL_TOP - 56),
                (PLAYHEAD_X, ROLL_TOP - 40)], fill=PINK)
    img.paste(Image.fromarray(LYR_NP), (40, LYRIC_Y - 30))
    for k, (wtxt, wt0, wt1) in enumerate(LYRIC):
        if wt0 <= t < wt1:
            d2.text((LYR_X[k], LYRIC_Y), wtxt, font=f_lyric, fill=PINK)
            break
    d2.rectangle([0, H - 10, int(W * t / dur), H], fill=PINK_SOFT)
    return np.asarray(img, dtype=np.uint8).tobytes()


# PARALLEL FRAMES. Every frame is independent — it reads the shared strip
# and writes bytes — so the only reason this was serial was that it was
# written as a loop.
if __name__ == "__main__":
    # fork, not spawn. macOS defaults to spawn, which re-imports this
    # module in every worker — and importing it BUILDS THE WHOLE STRIP, so
    # eight workers would each redraw the study before rendering a frame.
    # Forking shares the finished strip copy-on-write.
    try:
        multiprocessing.set_start_method("fork", force=True)
    except RuntimeError:
        pass
    workers = max(1, min(os.cpu_count() or 4, 8))
    os.makedirs(SEGDIR, exist_ok=True)
    for f in os.listdir(SEGDIR):
        os.remove(os.path.join(SEGDIR, f))
    edges = [round(FRAMES * k / workers) for k in range(workers + 1)]
    jobs = [(k, edges[k], edges[k + 1]) for k in range(workers)
            if edges[k + 1] > edges[k]]
    with multiprocessing.Pool(len(jobs)) as pool:
        segs = pool.map(encode_segment, jobs)
    lst = os.path.join(SEGDIR, "segs.txt")
    with open(lst, "w") as fh:
        for sgm in segs:
            fh.write(f"file '{os.path.basename(sgm)}'\n")
    subprocess.run(["ffmpeg", "-y", "-v", "error", "-f", "concat", "-safe", "0",
                    "-i", lst, "-i", AUD, "-af", "volume=-0.6dB",
                    "-c:v", "copy", "-c:a", "aac", "-b:a", "256k",
                    "-shortest", out_mp4], check=True)
    print(f"✓ {out_mp4} · {FRAMES} frames · {dur:.1f}s")
