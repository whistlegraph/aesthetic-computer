#!/usr/bin/env python3
# review-score4.py — SCROLLING clip-timeline review video, mini-DAW style.
# Same as review-score3 except the bottom-right corner: instead of static
# score art, the ORIGINAL loner whistlegraph is REDRAWN stroke-by-stroke
# (per-pixel ink appearance-time map recovered from the source TikTok,
# wg-appear.npz) and unmasked in proportion to the lead-vocal activity of
# the track, so the drawing progresses only while the lyrics are sung and
# completes as the singing ends.
import math, subprocess, sys, time
import numpy as np
from PIL import Image, ImageDraw, ImageFont

sr = 8000
W, H, FPS = 1920, 1080, 30
PPS = 272                      # timeline pixels per second
S = "/private/tmp/claude-501/-Users-jas-aesthetic-computer/df296e24-513a-488b-b96b-31cb958c1bda/scratchpad"
MP3 = "/Users/jas/aesthetic-computer/pop/loner/out/lonerclub-v4pid.mp3"
OUT = "/Users/jas/aesthetic-computer/pop/loner/out/lonerclub-v4pid-review-score-light.mp4"
APPEAR = f"{S}/wg-appear.npz"       # from build-appear.py (source video 7100768279983181099)
VOXRAW = f"{S}/vox8k.raw"           # s16le mono 8k demucs vocal stem, 94.2s
FONT_B = "/Users/jas/aesthetic-computer/slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf"
FONT_R = "/Users/jas/aesthetic-computer/slab/menuband/Sources/MenuBand/Resources/ywft-processing-regular.ttf"

BPM = 122.0
BEAT = 60.0 / BPM
BAR = 4 * BEAT
EIGHTH = BEAT / 2
GRID0 = 0.3654

# ---------------------------------------------------------------- audio in
def load(path, raw=False, af=None):
    args = ["ffmpeg", "-v", "error"]
    if raw:
        args += ["-f", "f32le", "-ar", "48000", "-ac", "2"]
    args += ["-i", path]
    if af:
        args += ["-af", af]
    args += ["-ac", "1", "-ar", str(sr), "-f", "f32le", "-"]
    out = subprocess.run(args, capture_output=True)
    if out.returncode != 0:
        sys.exit(f"ffmpeg decode failed for {path}: {out.stderr.decode()[:400]}")
    return np.frombuffer(out.stdout, np.float32).copy()

mix = load(MP3)
DUR = len(mix) / sr
N = len(mix)
del mix

def fit(sig):
    if len(sig) < N:
        sig = np.concatenate([sig, np.zeros(N - len(sig), np.float32)])
    return sig[:N]

OTHER = f"{S}/sep4/htdemucs/v4pid-trim/other.wav"
print("decoding stems...", flush=True)
LANES = [
    ("lead vox",    fit(load(f"{S}/vocalsFX.wav")),                       (110, 220, 205)),
    ("drums",       fit(load(f"{S}/drums-cool.raw", raw=True)),           (235, 150, 80)),
    ("bass",        fit(load(f"{S}/sep4/htdemucs/v4pid-trim/bass.wav")),  (200, 120, 235)),
    ("bells",       fit(load(OTHER, af="highpass=f=2800")),               (150, 190, 240)),
    ("pluck", fit(load(OTHER, af="highpass=f=700,lowpass=f=2800")), (140, 150, 235)),
    ("pads",        fit(load(OTHER, af="lowpass=f=700")),                 (120, 140, 190)),
    ("ahh arps",    fit(load(f"{S}/stem-flourish.raw", raw=True)),        (255, 210, 90)),
    ("wub sub",     fit(load(f"{S}/stem-wub.raw", raw=True)),             (235, 100, 60)),
    ("stamp",       fit(load(f"{S}/stem-stamp.raw", raw=True)),           (200, 150, 160)),
]

# ---------------------------------------------------------------- clip gate
def clips_of(sig):
    hop = int(0.05 * sr)
    nfr = len(sig) // hop
    fr = sig[: nfr * hop].reshape(nfr, hop)
    r = np.sqrt((fr ** 2).mean(axis=1))
    if r.max() <= 0:
        return []
    act = r > r.max() * 0.11
    # bridge silent gaps shorter than 0.15s (3 hops)
    i = 0
    while i < nfr:
        if not act[i]:
            j = i
            while j < nfr and not act[j]:
                j += 1
            if 0 < i and j < nfr and (j - i) < 3:
                act[i:j] = True
            i = j
        else:
            i += 1
    # contiguous runs -> regions in seconds
    regions = []
    i = 0
    while i < nfr:
        if act[i]:
            j = i
            while j < nfr and act[j]:
                j += 1
            t0, t1 = i * 0.05, j * 0.05
            if t1 - t0 >= 0.15:
                regions.append([t0, t1])
            i = j
        else:
            i += 1
    # merge gaps < 0.18s
    merged = []
    for reg in regions:
        if merged and reg[0] - merged[-1][1] < 0.18:
            merged[-1][1] = reg[1]
        else:
            merged.append(reg)
    # snap edges to the 8th-note grid
    def snap8(t):
        return min(DUR, max(0.0, GRID0 + round((t - GRID0) / EIGHTH) * EIGHTH))
    snapped = []
    for t0, t1 in merged:
        a, b = snap8(t0), snap8(t1)
        if b - a < EIGHTH / 2:
            b = min(DUR, a + EIGHTH)
        if snapped and a <= snapped[-1][1] + 1e-6:
            snapped[-1][1] = max(snapped[-1][1], b)
        else:
            snapped.append([a, b])
    # split anything longer than 4 bars at global 4-bar boundaries
    clips = []
    for t0, t1 in snapped:
        if t1 - t0 <= 4 * BAR + 0.05:
            clips.append((t0, t1))
            continue
        cur = t0
        k = math.ceil((t0 - GRID0) / (4 * BAR) - 1e-9)
        while True:
            b = GRID0 + k * 4 * BAR
            k += 1
            if b <= cur + 0.3:
                continue
            if b >= t1 - 0.3:
                break
            clips.append((cur, b))
            cur = b
        clips.append((cur, t1))
    return clips

# ---------------------------------------------------------------- layout
BG = (246, 244, 240)
GUT = 150                       # fixed left gutter
SCROLL_W = W - GUT              # 1770 px of scrolling timeline
PLAY_X = GUT + SCROLL_W // 2    # 1035, fixed playhead
LANE_H, LANE_GAP = 61, 6
NLANE = len(LANES)
LBL_BAND = 20                   # marker-label band at top of the strip
RULER_H = 28
STRIP_TOP = 100                 # y of strip in the frame
LANES_TOP = STRIP_TOP + LBL_BAND               # 120
LANES_BOT = LANES_TOP + NLANE * LANE_H + (NLANE - 1) * LANE_GAP   # 717
STRIP_H = LBL_BAND + (LANES_BOT - LANES_TOP) + RULER_H            # 645
STRIP_BOT = STRIP_TOP + STRIP_H

SWm = int(math.ceil(DUR * PPS))            # music extent in strip px
PAD_L = PLAY_X - GUT                        # 885
PAD_R = W - PLAY_X                          # 885
STRIP_W = SWm + PAD_L + PAD_R

SECTIONS = [
    (0.0,   31.83, "sitting open", (64, 190, 180)),
    (31.83, 63.30, "big pass",     (235, 120, 60)),
    (63.30, 92.60, "wind-down",    (150, 120, 200)),
    (92.60, DUR,   "stamp+ring",   (150, 150, 155)),
]
MARKS = [(23.86, "arp up-dn"), (27.06, "arp"), (31.83, "SEAM"),
         (55.44, "arp"), (59.37, "arp"), (93.60, "stamp")]

f_title = ImageFont.truetype(FONT_B, 48)
f_lbl   = ImageFont.truetype(FONT_B, 25)
f_tiny  = ImageFont.truetype(FONT_R, 17)
f_mark  = ImageFont.truetype(FONT_R, 20)
f_bar   = ImageFont.truetype(FONT_R, 20)
f_info  = ImageFont.truetype(FONT_R, 22)
f_tc    = ImageFont.truetype(FONT_B, 56)
f_kara  = ImageFont.truetype(FONT_B, 84)

def sx(t):                      # strip x for time t
    return PAD_L + int(round(t * PPS))

# ---------------------------------------------------------------- strip
print("rendering timeline strip...", flush=True)
strip = Image.new("RGB", (STRIP_W, STRIP_H), BG)
sd = ImageDraw.Draw(strip)
x0m, x1m = sx(0), sx(DUR)

# lane row backgrounds over the music extent only
lane_y = []
lane_clips = []
for li in range(NLANE):
    y0 = LBL_BAND + li * (LANE_H + LANE_GAP)
    lane_y.append(y0)
    sd.rectangle([x0m, y0, x1m, y0 + LANE_H - 1], fill=(234, 231, 226))

# beat grid: bar lines dim, heavier every 4 bars; ruler ticks + bar numbers
ry = STRIP_H - RULER_H
k = 0
while True:
    bt = GRID0 + k * BAR
    if bt >= DUR:
        break
    x = sx(bt)
    heavy = (k % 4 == 0)
    gcol = (206, 203, 197) if heavy else (222, 219, 213)
    sd.line([x, LBL_BAND, x, ry - 1], fill=gcol)
    tick = (105, 103, 99) if heavy else (160, 157, 152)
    sd.line([x, ry, x, ry + (12 if heavy else 8)], fill=tick, width=2 if heavy else 1)
    sd.text((x + 4, ry + 8), str(k + 1), font=f_bar,
            fill=(88, 86, 83) if heavy else (150, 147, 142))
    k += 1

# clips with in-block waveforms
ncols = SWm
for li, (name, sig, col) in enumerate(LANES):
    y0 = lane_y[li]
    mid = y0 + LANE_H // 2
    per = N // ncols            # 50 samples per strip column
    e = np.sqrt((sig[: per * ncols].reshape(ncols, per) ** 2).mean(axis=1))
    if e.max() > 0:
        e = (e / e.max()) ** 0.8
    fill = tuple(int(v * 0.25 + 191) for v in col)
    line = tuple(int(v * 0.58) for v in col)
    wave = tuple(int(v * 0.60) for v in col)
    lane_clips.append(clips_of(sig))
    for (t0, t1) in lane_clips[-1]:
        xa, xb = sx(t0), sx(t1)
        sd.rounded_rectangle([xa, y0 + 3, xb, y0 + LANE_H - 3], radius=5,
                             fill=fill, outline=line, width=2)
        amax = LANE_H // 2 - 6
        for x in range(xa + 2, xb - 1):
            ci = x - PAD_L
            if 0 <= ci < ncols:
                h = int(e[ci] * amax)
                if h > 0:
                    sd.line([x, mid - h, x, mid + h], fill=wave)
    print(f"  lane {name}", flush=True)

# event markers scroll with the strip
for t, label in MARKS:
    x = sx(t)
    col = (200, 40, 40) if label == "SEAM" else (168, 118, 20)
    sd.line([x, 2, x, ry - 1], fill=col, width=2)
    sd.text((x + 5, 1), label, font=f_mark, fill=col)

strip_np = np.array(strip)
del strip, sd

# ---------------------------------------------------------------- chrome
chrome = Image.new("RGB", (W, H), BG)
cd = ImageDraw.Draw(chrome)
cd.text((40, 18), "lonerclub (v4pid)", font=f_title, fill=(28, 27, 25))

MM_X0, MM_X1, MM_Y0, MM_Y1 = 40, 1880, 62, 88
for t0, t1, name, col in SECTIONS:
    xa = MM_X0 + int(t0 / DUR * (MM_X1 - MM_X0))
    xb = MM_X0 + int(t1 / DUR * (MM_X1 - MM_X0))
    cd.rectangle([xa, MM_Y0, xb, MM_Y1], fill=tuple(int(v * 0.45 + 140) for v in col))
    cd.text((xa + 6, MM_Y0 + 5), name, font=f_tiny, fill=tuple(int(v * 0.52) for v in col))
cd.rectangle([MM_X0, MM_Y0, MM_X1, MM_Y1], outline=(150, 147, 142))

# gutter: lane row stubs + labels
for li, (name, sig, col) in enumerate(LANES):
    y0 = LANES_TOP + li * (LANE_H + LANE_GAP)
    cd.rectangle([10, y0, GUT - 6, y0 + LANE_H - 1], fill=(234, 231, 226))
    cd.text((18, y0 + LANE_H // 2), name, font=f_lbl, fill=tuple(int(v * 0.55) for v in col), anchor="lm")
cd.line([GUT - 2, STRIP_TOP, GUT - 2, STRIP_BOT], fill=(200, 197, 192))


chrome_np = np.array(chrome)
del chrome, cd

# ------------------------------------------------- corner: live whistlegraph
# Appearance-time map: appear[y,x] = source-video frame (12 fps) where that
# ink pixel was drawn (1e9 = never ink). Unmask in vocal-progress order.
wg = np.load(APPEAR)
appear = wg["appear"]                       # (698, 452) float32, video frames
wg_final = wg["final"]                      # clean final frame, gray float32
wg_paper = float(wg["paper"])
wg_mask = wg["final_mask"]
WGW, WGH = appear.shape[1], appear.shape[0]           # 452 x 698 working res
CW, CH = WGW // 2, WGH // 2                            # 226 x 349 card
CX, CY = W - 10 - CW, H - 10 - CH                      # bottom-right, clear of lanes
CARD_A = 0.85
I0 = 26.0                                   # first usable source frame
maxA = float(appear[wg_mask].max())
ink_a = np.clip((wg_paper - wg_final) / (wg_paper - 60.0), 0.0, 1.0)
ink_a[~wg_mask] = 0.0
PAPER_RGB = np.float32([246.0, 245.0, 241.0])
INK_RGB = np.float32([42.0, 42.0, 47.0])

# vocal-activity progress curve: rises only while the lead vocal is active
vox = np.fromfile(VOXRAW, np.int16).astype(np.float32) / 32768.0
vhop = int(0.05 * sr)
nh = len(vox) // vhop
vr = np.sqrt((vox[: nh * vhop].reshape(nh, vhop) ** 2).mean(axis=1))
vact = (vr > vr.max() * 0.08).astype(np.float32)
vact = np.convolve(vact, np.ones(5, np.float32) / 5, mode="same")   # light smooth
vt = (np.arange(nh) + 0.5) * 0.05
# WORD-ACCURATE reveal: chart-line beats (the sung grid of every pass)
# mapped to the harvest word windows of the original take, so each stroke
# lands exactly under the word it was drawn to. One whistlegraph per sung
# pass; earlier figures ghost back underneath.
BEAT = 60.0 / 122
PASS_T0 = [0.3654, 31.83, 63.30]
import json as _json
_WC = sorted(_json.load(open(f"{S}/wordclock.json")), key=lambda e: e["t0"])
_T0 = np.array([e["t0"] for e in _WC]); _T1 = np.array([e["t1"] for e in _WC])
_V0 = np.array([e["v0"] for e in _WC]); _V1 = np.array([e["v1"] for e in _WC])
_WORDS = [e["word"] for e in _WC]; _MARKS = [e["mark"] for e in _WC]
def _pass_of(t):
    for k in range(len(PASS_T0) - 1, -1, -1):
        if t >= PASS_T0[k]:
            return k
    return 0
def _entry_pass(i):
    return _pass_of(_T0[i] + 0.001)

def video_cut(t):
    """(pass index, source cut in 12fps frames), driven by the CALIBRATED
    word clock — each sung word advances the pen through its own strokes."""
    pi = _pass_of(t)
    i = int(np.searchsorted(_T0, t, side="right")) - 1
    if i < 0 or _entry_pass(i) != pi:
        return pi, 0.0
    if t < _T1[i]:
        frac = (t - _T0[i]) / (_T1[i] - _T0[i])
        return pi, (_V0[i] + frac * (_V1[i] - _V0[i])) * 12.0
    return pi, _V1[i] * 12.0

def word_now(t):
    i = int(np.searchsorted(_T0, t, side="right")) - 1
    if i >= 0 and t < _T1[i] and _entry_pass(i) == _pass_of(t):
        return (_WORDS[i], _MARKS[i])
    return None
    for wi, (b0, b1, _, _) in enumerate(WORDMAP):
        if lb < b1:
            return WORDLBL[wi]
    return None

wgo = np.load(f"{S}/wg-retrace.npz")
ORD = wgo["ordmap"]; PENC = wgo["pcoords"]
PT12 = wgo["ptimes"].astype(np.float64) * 12.0
OM = int(PENC.shape[0])

def rank_of(cut):
    return float(min(int(np.searchsorted(PT12, cut, side="right")), OM))

GHOST_A = 0.16
def card_at(pi, cut):
    """Card with pass pi's drawing REDRAWN along the pen path up to `cut`,
    over pale ghosts of the completed earlier passes."""
    k = rank_of(cut)
    a = np.where((ORD >= 0) & (ORD <= k), ink_a, 0.0).astype(np.float32)
    a = np.array(Image.fromarray((a * 255).astype(np.uint8)).resize(
        (CW, CH), Image.LANCZOS), np.float32)[..., None] / 255.0
    card = PAPER_RGB * np.ones((CH, CW, 3), np.float32)
    if pi > 0:
        g = np.array(Image.fromarray((ink_a * 255).astype(np.uint8)).resize(
            (CW, CH), Image.LANCZOS), np.float32)[..., None] / 255.0 * GHOST_A
        card = card * (1.0 - g) + INK_RGB * g
    fresh = ((ORD > k - 150) & (ORD <= k)).astype(np.float32) * ink_a
    fa = np.array(Image.fromarray((fresh * 255).astype(np.uint8)).resize(
        (CW, CH), Image.LANCZOS), np.float32)[..., None] / 255.0
    card = card * (1.0 - a) + INK_RGB * a
    card = card * (1.0 - fa) + np.float32([196.0, 44.0, 40.0]) * fa
    return card, k

# ---------------------------------------------------------------- frames
NF = int(DUR * FPS)
print(f"encoding {NF} frames...", flush=True)
ff = subprocess.Popen(["ffmpeg", "-y", "-v", "error",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
    "-i", MP3, "-map", "0:v", "-map", "1:a",
    "-c:v", "libx264", "-preset", "veryfast", "-crf", "18", "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "256k", "-shortest", OUT], stdin=subprocess.PIPE)

GLOW = int(0.15 * PPS)          # +-0.15s around the playhead
t_start = time.time()
for f in range(NF):
    t = f / FPS
    frame = chrome_np.copy()
    off = int(round(t * PPS))
    frame[STRIP_TOP:STRIP_BOT, GUT:W] = strip_np[:, off:off + SCROLL_W]
    # ACTIVE CLIPS deepen while they play
    active_rects = []
    for li in range(NLANE):
        for (t0, t1) in lane_clips[li]:
            if t0 <= t < t1:
                xa = max(GUT, GUT + sx(t0) - off)
                xb = min(W, GUT + sx(t1) - off)
                if xb > xa:
                    y0f = LANES_TOP + li * (LANE_H + LANE_GAP)
                    reg = frame[y0f:y0f + LANE_H, xa:xb]
                    frame[y0f:y0f + LANE_H, xa:xb] = np.clip(reg.astype(np.int16) * 84 // 100, 0, 255).astype(np.uint8)
                    active_rects.append((xa, y0f, xb, li))
                break
    # playhead + minimap position
    frame[STRIP_TOP:STRIP_BOT, PLAY_X - 1:PLAY_X + 1] = (28, 27, 25)
    mx = MM_X0 + int(t / DUR * (MM_X1 - MM_X0))
    frame[MM_Y0:MM_Y1 + 1, mx:mx + 2] = (28, 27, 25)
    # corner whistlegraph card, unmasked by vocal progress
    pi, cutf = video_cut(t)
    card, k = card_at(pi, cutf)
    # the pen rides the END of the path — smooth, since the path is chained
    _, cutf_prev = video_cut(max(0.0, t - 0.1))
    if cutf - cutf_prev > 0.02 and k < OM - 1:
        py, px = PENC[int(k)]
        cyx, cxx = int(py) // 2, int(px) // 2
        y0d, y1d = max(0, cyx - 3), min(CH, cyx + 4)
        x0d, x1d = max(0, cxx - 3), min(CW, cxx + 4)
        card[y0d:y1d, x0d:x1d] = (200.0, 36.0, 32.0)
    reg = frame[CY:CY + CH, CX:CX + CW].astype(np.float32)
    frame[CY:CY + CH, CX:CX + CW] = \
        (reg * (1.0 - CARD_A) + card * CARD_A).astype(np.uint8)
    img = Image.fromarray(frame)
    dact = ImageDraw.Draw(img)
    for (xa, y0f, xb, li) in active_rects:
        dact.rectangle([xa, y0f + 1, xb - 1, y0f + LANE_H - 2],
                       outline=tuple(int(v * 0.55) for v in LANES[li][2]), width=3)
    ImageDraw.Draw(img).rectangle([CX, CY, CX + CW - 1, CY + CH - 1],
                                  outline=(182, 179, 174))
    wn = word_now(t)
    if wn is not None:
        dd2 = ImageDraw.Draw(img)
        dd2.text((60, CY + 66), wn[0], font=f_kara, fill=(28, 27, 25), anchor="lm")
    if wn is not None:
        dd2.text((64, CY + 170), wn[1], font=f_title, fill=(110, 108, 104), anchor="lm")
    ImageDraw.Draw(img).text((W - 40, 14),
        f"{int(t) // 60}:{int(t) % 60:02d}.{int((t * 10) % 10)}",
        font=f_tc, fill=(28, 27, 25), anchor="ra")
    ff.stdin.write(img.tobytes())
    if f % 300 == 0:
        print(f"  frame {f}/{NF}  ({time.time() - t_start:.0f}s)", flush=True)
ff.stdin.close()
ff.wait()
print(f"done in {time.time() - t_start:.0f}s -> {OUT}")
