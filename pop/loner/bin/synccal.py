# synccal.py — find the display-latency offset by eye instead of by guess.
#
# @jeffrey: "the audio still comes sooner than visuals · still feels off".
# The study file measures exact (both streams at PTS 0, the kick at
# 0.0010 s in the WAV and 0.0010 s decoded back out of the MP4, roll
# geometry +3.0 ms mean), so what is left is the display pipeline — and
# how many milliseconds THAT is depends on the screen and the player, not
# on us. Guessing at it one render at a time is slow and inconclusive.
#
# So: a click track, a big shape that flashes on the beat, and the
# compensation stepping through a set of values every four bars with the
# number printed large. Watch it once, say which section locks, and that
# number becomes SYNC_MS for every score video.
#
#   python3 pop/loner/bin/synccal.py        → out/synccal.mp4
#
# The click is deliberately dry and percussive: a transient is the only
# thing the eye and ear can compare precisely.

import math, multiprocessing, os, subprocess, wave
import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
OUT = os.path.join(LANE, "out")
SEG = os.path.join(OUT, ".synccal")

W, H, FPS = 1280, 720, 60
BPM, SR = 122.0, 48000
SPB = 60.0 / BPM
OFFSETS = [0, 25, 50, 75, 100, 125]      # ms of picture lead, per section
BARS_EACH = 3
BEATS = len(OFFSETS) * BARS_EACH * 4
DUR = BEATS * SPB + 1.0

BG, FG, HOT, DIM = (16, 15, 22), (238, 234, 230), (255, 92, 162), (120, 118, 130)
F = lambda s: ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", s)
f_big, f_lab, f_sm = F(230), F(54), F(30)


def click_track(path):
    """A dry click on every beat — transients only, nothing to smear."""
    n = int(DUR * SR)
    y = np.zeros(n)
    rng = np.random.default_rng(3)
    for b in range(BEATS):
        i0 = int(b * SPB * SR)
        m = int(0.035 * SR)
        u = np.arange(m) / SR
        env = np.exp(-u * 260)
        tick = (np.sin(2 * np.pi * 1800 * u) * 0.7 + rng.standard_normal(m) * 0.35)
        body = np.sin(2 * np.pi * (70 + 220 * np.exp(-u * 90)) * u) * np.exp(-u * 55)
        hit = (tick * env + body * 0.9) * (1.0 if b % 4 else 1.0)
        y[i0:i0 + m] += hit[:max(0, min(m, n - i0))]
    y /= max(1e-9, np.abs(y).max()) / 0.89
    with wave.open(path, "wb") as f:
        f.setnchannels(1); f.setsampwidth(2); f.setframerate(SR)
        f.writeframes((y * 32767).astype("<i2").tobytes())


def section_of(beat):
    k = int(beat) // (BARS_EACH * 4)
    return min(max(k, 0), len(OFFSETS) - 1)


def render_frame(i):
    t = (i + 0.5) / FPS
    sec = section_of(t / SPB)
    tt = t + OFFSETS[sec] / 1000.0          # the compensation under test
    beat = tt / SPB
    img = Image.new("RGB", (W, H), BG)
    d = ImageDraw.Draw(img)
    # the flash: full for 90 ms after each beat, so the eye has an edge
    frac = beat - math.floor(beat)
    lit = frac < (0.090 / SPB) and beat >= 0
    r = 150
    d.ellipse([W // 2 - r, 250 - r, W // 2 + r, 250 + r],
              fill=HOT if lit else (34, 32, 42))
    lab = f"{OFFSETS[sec]} ms"
    d.text((W // 2 - d.textlength(lab, font=f_big) / 2, 400), lab,
           font=f_big, fill=FG if lit else DIM)
    d.text((40, 30), "sync calibration — which section locks?",
           font=f_lab, fill=FG)
    d.text((40, H - 60),
           "each section holds one compensation for 3 bars · "
           "pick the one where flash and click are simultaneous",
           font=f_sm, fill=DIM)
    for k, o in enumerate(OFFSETS):        # a progress ladder
        x = 40 + k * 90
        d.rectangle([x, H - 110, x + 74, H - 92],
                    fill=HOT if k == sec else (44, 42, 54))
    return np.asarray(img, dtype=np.uint8).tobytes()


def encode(job):
    k, a, b = job
    seg = os.path.join(SEG, f"s{k:02d}.mp4")
    p = subprocess.Popen(
        ["ffmpeg", "-y", "-v", "error", "-f", "rawvideo", "-pix_fmt", "rgb24",
         "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-", "-c:v", "libx264",
         "-preset", "veryfast", "-crf", "20", "-pix_fmt", "yuv420p",
         "-threads", "1", seg], stdin=subprocess.PIPE)
    for i in range(a, b):
        p.stdin.write(render_frame(i))
    p.stdin.close(); p.wait()
    return seg


if __name__ == "__main__":
    try:
        multiprocessing.set_start_method("fork", force=True)
    except RuntimeError:
        pass
    os.makedirs(SEG, exist_ok=True)
    for f in os.listdir(SEG):
        os.remove(os.path.join(SEG, f))
    aud = os.path.join(SEG, "click.wav")
    click_track(aud)
    frames = int(DUR * FPS)
    workers = max(1, min(os.cpu_count() or 4, 8))
    edges = [round(frames * k / workers) for k in range(workers + 1)]
    jobs = [(k, edges[k], edges[k + 1]) for k in range(workers)
            if edges[k + 1] > edges[k]]
    with multiprocessing.Pool(len(jobs)) as pool:
        segs = pool.map(encode, jobs)
    lst = os.path.join(SEG, "l.txt")
    with open(lst, "w") as fh:
        for s in segs:
            fh.write(f"file '{os.path.basename(s)}'\n")
    out = os.path.join(OUT, "synccal.mp4")
    subprocess.run(["ffmpeg", "-y", "-v", "error", "-f", "concat", "-safe", "0",
                    "-i", lst, "-i", aud, "-c:v", "copy", "-c:a", "aac",
                    "-b:a", "192k", "-shortest", out], check=True)
    print(f"✓ {out} · {frames} frames · {DUR:.1f}s · sections {OFFSETS} ms")
