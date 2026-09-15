#!/usr/bin/env python3
# review-amazing.py — SCROLLING clip-timeline review video for "amazing
# grace", mini-DAW style: the pop/cult/viz/review-score.py treatment
# (itself from pop/loner/viz/review-score4.py) carried to this lane
# without the cult's tempo warp, release seams or elastic pass, because
# this record has none — the master is the render.
#
# Per-stem lanes as colored clip/waveform blocks scroll under a fixed
# playhead; a cell/act ruler runs beneath them; the sung words are drawn
# twice — as pitch-placed blocks in the score lanes and as big karaoke
# text (vowel-stretched, letters lighting one by one) at the moment they
# are sung. The act card lists the hymn's four lines + intro + amen with
# the current one lit; the house loudness meter runs live along the top.
#
# Truth sources (all written by the bake):
#   · out/amazing-grace/amazing-grace-score.json — tokens + the sung notes
#     (bin/amazing-score.mjs, from the sing-amazing.mjs receipt); its
#     cells mirror c/amazinhym.c CELLS, so every block below is the
#     arrangement the engine actually rendered.
#   · out/amazing-grace/vox.wav, bed.wav, stamp.wav — TRUE stems from the
#     bake (the master is bed + vox(+cathedral) + stamp, then the cut).
#     The bed is band-split into sub-lanes, labelled with their split.
#
#   pop/.venv/bin/python pop/big-pictures/viz/review-amazing.py
#   --audio PATH     render against a different master
#   --out PATH       override the output mp4 path
#   --light          paper theme
#   --from S --to S  render a window (for previews)
#   --preview        15 fps
#   --lufs-target N  (default −11.5)   --tp-ceiling N (default −2.0)
import json, math, os, subprocess, sys, time
import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.abspath(os.path.join(HERE, ".."))
REPO = os.path.abspath(os.path.join(LANE, "..", ".."))
OUTD = os.path.join(LANE, "out", "amazing-grace")
sys.path.insert(0, os.path.join(REPO, "pop", "viz"))
from loudness_meter import analyze_loudness, draw_loudness_meter

def argval(flag, default=None):
    if flag in sys.argv:
        i = sys.argv.index(flag)
        if i + 1 < len(sys.argv):
            return sys.argv[i + 1]
    return default

LIGHT = "--light" in sys.argv
PREVIEW = "--preview" in sys.argv
sr = 8000
W, H = 2560, 1920
FPS = 15 if PREVIEW else 30
PPS = 200                       # px per second → ~170 px per beat at 70

SCORE = json.load(open(os.path.join(OUTD, "amazing-grace-score.json")))
MP3 = argval("--audio", SCORE["audio"])
OUT = argval("--out", os.path.join(OUTD, "amazing-grace-review-2560x1920.mp4"))
LUFS_TARGET = float(argval("--lufs-target", "-11.5"))
TP_CEILING = float(argval("--tp-ceiling", "-2.0"))
DUR = float(SCORE["dur"])
T_FROM = max(0.0, float(argval("--from", "0")))
T_TO = min(DUR, float(argval("--to", str(DUR))))
BPM = 70.0
SPB = 60.0 / BPM
INTRO = 6.0                     # where the pickup "a-" lands (bake-amazing.sh)
def tb(beat):                   # beat → seconds; beat 0 is the pickup
    return INTRO + beat * SPB

FONT_B = f"{REPO}/slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf"
FONT_R = f"{REPO}/slab/menuband/Sources/MenuBand/Resources/ywft-processing-regular.ttf"

# ---------------------------------------------------------------- audio
def load(path, af=None):
    cmd = ["ffmpeg", "-v", "error", "-i", path]
    if af:
        cmd += ["-af", af]
    cmd += ["-ac", "1", "-ar", str(sr), "-f", "f32le", "-"]
    raw = subprocess.run(cmd, capture_output=True, check=True).stdout
    return np.frombuffer(raw, dtype=np.float32).astype(np.float64)

N = int(round(DUR * sr))
def fit(sig):
    if len(sig) >= N:
        return sig[:N]
    return np.concatenate([sig, np.zeros(N - len(sig))])

VOX_DELAY = f"adelay={int(INTRO * 1000)}|{int(INTRO * 1000)}"
print("loading stems...", flush=True)
STEM_VOX = fit(load(os.path.join(OUTD, "vox.wav"), af=VOX_DELAY))
BED = os.path.join(OUTD, "bed.wav")
STEM_STAMP = fit(load(os.path.join(OUTD, "stamp.wav"), af="adelay=65800|65800"))

# ---------------------------------------------------------------- score
WHO_COL = {"jeffrey": (255, 211, 82), None: (200, 200, 205)}
TOKENS = []
for tk in SCORE["tokens"]:
    span = tk["rails"]["jeffrey"]
    TOKENS.append({"word": tk["word"], "t0": float(span["t0"]), "t1": float(span["t1"]),
                   "who": "jeffrey"})
lanes_by = {ln["name"]: ln for ln in SCORE["lanes"]}
NOTES = [{"t0": e["t0"], "t1": e["t1"], "word": e["word"], "midi": e["midi"], "who": "jeffrey",
          "voice": "lead"} for e in lanes_by["vocal"]["events"]]
NOTE_NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
def note_name(m):
    return f"{NOTE_NAMES[m % 12]}{m // 12 - 1}"
MELODY = [{**e, "word": note_name(e["midi"]), "voice": "instr", "who": None} for e in NOTES]

# the engine's cells — c/amazinhym.c CELLS, verbatim (beat, len, chord, line)
CELLS = [(1, 4, "G", 1), (5, 4, "G", 0), (9, 4, "G", 0), (13, 6, "C", 0),
         (19, 4, "G", 1), (23, 4, "G", 0), (27, 6, "D", 0),
         (33, 4, "G", 1), (37, 4, "G", 0), (41, 4, "C", 0), (45, 6, "C", 0),
         (51, 4, "G", 1), (55, 4, "D", 0), (59, 5, "G", 0), (64, 4, "C", 0)]
CHORD_ROOT = {"G": 43, "C": 48, "D": 50}
CHORDS = [{"t0": tb(b), "t1": tb(b + ln), "word": ch, "midi": CHORD_ROOT[ch] + 12,
           "voice": "instr", "who": None} for (b, ln, ch, _l) in CELLS]
CHORDS.append({"t0": tb(68), "t1": tb(68) + 4.0, "word": "G", "midi": 55, "voice": "instr", "who": None})
DRUMS, BELLS = [], []
def hit(lst, t, word, midi, dur=0.18):
    lst.append({"t0": t, "t1": t + dur, "word": word, "midi": midi, "voice": "instr", "who": None})
# intro cells (two, downbeat-aligned): kick + glock, hats on the second
intro_cells = int(math.floor((tb(1) + 0.001) / (4 * SPB)))
first = tb(1) - intro_cells * 4 * SPB
for k in range(intro_cells):
    t0 = first + k * 4 * SPB
    hit(DRUMS, t0, "kick", 40)
    hit(BELLS, t0, "glock", 62 + (12 if k % 2 else 0), 1.4)
    if k == intro_cells - 1:
        hit(DRUMS, t0 + 2 * SPB, "clap", 48)
for (b, ln, ch, line) in CELLS:
    t0 = tb(b)
    hit(DRUMS, t0, "kick", 40)
    hit(DRUMS, t0 + 2 * SPB, "clap", 48)
    if ln >= 6:
        hit(DRUMS, t0 + 4 * SPB, "kick", 40)
    if line:
        root = CHORD_ROOT[ch] + 12
        hit(BELLS, t0 + 0.5 * SPB, "kalimba", root + 7, 1.2)
        hit(BELLS, t0 + 1.5 * SPB, "kalimba", root + 12, 1.0)
        for a, iv in enumerate((0, 4, 7, 12)):
            hit(BELLS, t0 + a * 0.5 * SPB, "piano", root + iv, 1.0)
        hit(BELLS, t0, "glock", root + 7, 1.4)
hit(DRUMS, tb(68), "kick", 40)
hit(BELLS, tb(68), "glock", 62, 2.4)
hit(BELLS, tb(68), "glock", 67, 3.2)
STAMP_EV = [{"t0": 65.8, "t1": 67.3, "word": "aesthetic computer", "midi": 60, "voice": "instr", "who": None}]

# ---------------------------------------------------------------- vowel stretch
VOWSET = set("aeiou")
NO_STRETCH = {"a"}
FUNC_WORDS = {"a", "the"}
STRETCH_BASE, STRETCH_RATE, STRETCH_CAP = 0.95, 0.42, 9

def vowel_groups(w):
    gs, i = [], 0
    while i < len(w):
        if w[i] in VOWSET:
            j = i
            while j < len(w) and (w[j] in VOWSET or (w[j] == "y" and j > i)):
                j += 1
            gs.append((i, j))
            i = j
        else:
            i += 1
    return gs

def stretch_part(part, extra):
    low = part.lower()
    gs = vowel_groups(low)
    if len(gs) > 1:
        gs = [g for g in gs if not (g[1] - g[0] == 1 and g[0] == len(low) - 1 and low[g[0]] == "e")]
    if not gs or extra <= 0:
        return part
    a, b = max(gs, key=lambda g: g[1] - g[0])
    letters = [k for k in range(a, b) if low[k] in VOWSET]
    if not letters:
        return part
    reps = {k: 0 for k in letters}
    for n in range(extra):
        reps[letters[n % len(letters)]] += 1
    return "".join(ch * (1 + reps.get(k, 0)) for k, ch in enumerate(part))

def stretch_word(word, dur):
    if word in NO_STRETCH:
        return word
    extra = min(STRETCH_CAP, int((dur - STRETCH_BASE) / STRETCH_RATE))
    if extra <= 0:
        return word
    parts = word.split(" ")
    idx = [i for i, p in enumerate(parts) if p.lower() not in FUNC_WORDS] or list(range(len(parts)))
    n = len(idx)
    for j, pi in enumerate(idx):
        parts[pi] = stretch_part(parts[pi], extra // n + (1 if j < extra % n else 0))
    return " ".join(parts)

def stretch_on_note(word, notes):
    """Multi-note words hold the vowel of their LONGEST note ("-maaa-"),
    not the first vowel."""
    extra = min(STRETCH_CAP, int((max(n["t1"] - n["t0"] for n in notes) - STRETCH_BASE) / STRETCH_RATE))
    if extra <= 0:
        return word
    gs = vowel_groups(word.lower())
    k = max(range(len(notes)), key=lambda i: notes[i]["t1"] - notes[i]["t0"])
    if k >= len(gs):
        return stretch_part(word, extra)
    a, b = gs[k]
    return word[:a] + word[a:b][0] * extra + word[a:b] + word[b:]

for tk in TOKENS:
    mine = [e for e in NOTES if tk["t0"] - 1e-3 <= e["t0"] < tk["t1"] + 1e-3]
    if len(mine) > 1:
        tk["disp"] = stretch_on_note(tk["word"], mine)
    else:
        tk["disp"] = stretch_word(tk["word"], tk["t1"] - tk["t0"])
print(f"vowel stretching: {sum(1 for tk in TOKENS if tk['disp'] != tk['word'])}/{len(TOKENS)} tokens elongated", flush=True)

def prog(tk, t):
    return max(0.0, min(1.0, (t - tk["t0"]) / max(1e-6, tk["t1"] - tk["t0"])))

# ---------------------------------------------------------------- lanes
ALANES = [   # (label, signal, color, clip-gate threshold)
    ("vox (sung)",  STEM_VOX,                              (255, 211, 82),  0.08),
    ("bed 2.5k+",   fit(load(BED, af="highpass=f=2500")),  (240, 175, 90),  0.045),
    ("bed 250-2.5k", fit(load(BED, af="highpass=f=250,lowpass=f=2500")), (150, 140, 220), 0.11),
    ("bass <250",   fit(load(BED, af="highpass=f=45,lowpass=f=250")), (200, 120, 235), 0.11),
    ("kick <150",   fit(load(BED, af="lowpass=f=150")),    (235, 110, 60),  0.11),
    ("ac stamp",    STEM_STAMP,                            (150, 150, 155), 0.08),
]
LANE_THRESH = {nm: th for (nm, _s, _c, th) in ALANES}
LANE_DEFS = [
    ("sung lead",    "ev", NOTES,    (255, 211, 82),  150),
    ("melody (sine)", "ev", MELODY,  (110, 220, 205), 110),
    ("chords · cells", "ev", CHORDS, (130, 180, 245), 90),
    ("bells · piano", "ev", BELLS,   (245, 180, 95),  100),
    ("kick · clap",  "ev", DRUMS,    (240, 125, 65),  70),
    ("stamp",        "ev", STAMP_EV, (150, 150, 155), 50),
] + [(nm, "au", sig, col, 76) for (nm, sig, col, _th) in ALANES]
NLANE = len(LANE_DEFS)

def clips_of(sig, thresh=0.11):
    hop = int(0.05 * sr)
    nfr = len(sig) // hop
    fr = sig[: nfr * hop].reshape(nfr, hop)
    r = np.sqrt((fr ** 2).mean(axis=1))
    if r.max() <= 0:
        return []
    act = r > r.max() * thresh
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
    regions, i = [], 0
    while i < nfr:
        if act[i]:
            j = i
            while j < nfr and act[j]:
                j += 1
            if (j - i) * 0.05 >= 0.15:
                regions.append([i * 0.05, j * 0.05])
            i = j
        else:
            i += 1
    merged = []
    for reg in regions:
        if merged and reg[0] - merged[-1][1] < 0.18:
            merged[-1][1] = reg[1]
        else:
            merged.append(reg)
    HALF = SPB / 2
    def snap(t):                         # snap edges to the 8th-note grid
        return min(DUR, max(0.0, INTRO + round((t - INTRO) / HALF) * HALF))
    snapped = []
    for t0, t1 in merged:
        a, b = snap(t0), snap(t1)
        if b - a < HALF / 2:
            b = min(DUR, a + HALF)
        if snapped and a <= snapped[-1][1] + 1e-6:
            snapped[-1][1] = max(snapped[-1][1], b)
        else:
            snapped.append([a, b])
    clips = []                           # split at cell boundaries so a held pad reads as cells
    cell_edges = [tb(b) for (b, _l, _c, _n) in CELLS] + [tb(68)]
    for t0, t1 in snapped:
        cur = t0
        for edge in cell_edges:
            if edge <= cur + 0.3:
                continue
            if edge >= t1 - 0.3:
                break
            clips.append((cur, edge))
            cur = edge
        clips.append((cur, t1))
    return clips

# ---------------------------------------------------------------- theme
def dim(col, k):
    return tuple(int(v * k) for v in col)

if LIGHT:
    BG = (246, 244, 240); LANE_BG = (234, 231, 226); INK = (28, 27, 25)
    PLAYHEAD = (28, 27, 25); MMCUR = (28, 27, 25)
    GRID_HVY, GRID_LT = (206, 203, 197), (222, 219, 213)
    TICK_HVY, TICK_LT = (105, 103, 99), (160, 157, 152)
    BNUM_HVY, BNUM_LT = (88, 86, 83), (150, 147, 142)
    MM_OUTLINE = (150, 147, 142); GUT_LINE = (200, 197, 192); FOOT = (110, 108, 104)
    MARK_COL = (168, 118, 20); CARD_BG, CARD_EDGE = (238, 235, 230), (200, 197, 192)
    CARD_HDR = (110, 108, 104); ROW_HL = (222, 218, 212); BAR_TRACK = (210, 207, 200)
else:
    BG = (12, 11, 14); LANE_BG = (17, 16, 20); INK = (240, 238, 232)
    PLAYHEAD = (255, 245, 230); MMCUR = (245, 243, 238)
    GRID_HVY, GRID_LT = (34, 33, 40), (24, 23, 28)
    TICK_HVY, TICK_LT = (130, 128, 134), (78, 76, 82)
    BNUM_HVY, BNUM_LT = (150, 148, 152), (104, 102, 108)
    MM_OUTLINE = (58, 58, 64); GUT_LINE = (44, 44, 50); FOOT = (120, 118, 124)
    MARK_COL = (255, 210, 90); CARD_BG, CARD_EDGE = (17, 16, 21), (44, 44, 50)
    CARD_HDR = (150, 148, 155); ROW_HL = (28, 27, 34); BAR_TRACK = (40, 40, 46)

def ink_of(col):   return dim(col, 0.55) if LIGHT else col
def mute_of(col):  return tuple(int(v * 0.45 + 120) for v in col) if LIGHT else dim(col, 0.55)
def clip_fill(col): return tuple(int(v * 0.25 + 191) for v in col) if LIGHT else dim(col, 0.22)
def clip_line(col): return dim(col, 0.58) if LIGHT else dim(col, 0.78)
def wave_of(col):  return dim(col, 0.52) if LIGHT else dim(col, 0.60)
def blk_fill(col): return tuple(int(v * 0.30 + 165) for v in col) if LIGHT else dim(col, 0.42)
def blk_line(col): return dim(col, 0.60) if LIGHT else dim(col, 0.95)
def mm_fill(col):  return tuple(int(v * 0.45 + 140) for v in col) if LIGHT else dim(col, 0.40)
def blend(c0, c1, u):
    return tuple(int(a + (b - a) * u) for a, b in zip(c0, c1))

# ---------------------------------------------------------------- layout
GUT = 205
SCROLL_W = W - GUT
PLAY_X = GUT + SCROLL_W // 2
LANE_GAP = 5
LBL_BAND = 26
RULER_H = 34
STRIP_TOP = 110
LANES_TOP = STRIP_TOP + LBL_BAND
lane_y, y = [], 0
for (_n, _k, _d, _c, hh) in LANE_DEFS:
    lane_y.append(y)
    y += hh + LANE_GAP
LANES_H = y - LANE_GAP
STRIP_H = LBL_BAND + LANES_H + RULER_H
STRIP_BOT = STRIP_TOP + STRIP_H
SWm = int(math.ceil(DUR * PPS))
PAD_L = PLAY_X - GUT
PAD_R = W - PLAY_X
STRIP_W = SWm + PAD_L + PAD_R

ACTS = [(float(a["t0"]), a["name"].upper(), tuple(a["color"])) for a in SCORE["acts"]]
ACTS = [(t0, ("I " if i == 1 else "II " if i == 2 else "III " if i == 3 else "IV " if i == 4 else "") + n, c)
        for i, (t0, n, c) in enumerate(ACTS)]
ACT_END = [a[0] for a in ACTS[1:]] + [DUR]
MARKS = [(tb(0), "pickup a-"), (tb(64), "amen IV (C)"), (tb(68), "amen I (G)"), (65.8, "ac stamp")]

f_title = ImageFont.truetype(FONT_B, 48)
f_lbl   = ImageFont.truetype(FONT_B, 24)
f_tiny  = ImageFont.truetype(FONT_R, 17)
f_mark  = ImageFont.truetype(FONT_R, 20)
f_bar   = ImageFont.truetype(FONT_R, 20)
f_act   = ImageFont.truetype(FONT_B, 25)
f_tc    = ImageFont.truetype(FONT_B, 56)

print(f"analyzing review loudness: {os.path.basename(MP3)}", flush=True)
LOUDNESS = analyze_loudness(MP3)

def sx(t):
    return PAD_L + int(round(t * PPS))

# ---------------------------------------------------------------- strip
print("rendering timeline strip...", flush=True)
strip = Image.new("RGB", (STRIP_W, STRIP_H), BG)
sd = ImageDraw.Draw(strip)
x0m, x1m = sx(0), sx(DUR)
for li in range(NLANE):
    y0 = LBL_BAND + lane_y[li]
    sd.rectangle([x0m, y0, x1m, y0 + LANE_DEFS[li][4] - 1], fill=LANE_BG)

# beat grid (faint) + the cell ruler: every cell is a bar here, heavy at
# the hymn's four line openings; the two intro cells are −2 and −1
ry = STRIP_H - RULER_H
beat = -8
while tb(beat) < DUR:
    x = sx(tb(beat))
    if x >= x0m:
        sd.line([x, LBL_BAND, x, ry - 1], fill=GRID_LT)
    beat += 1
cells_all = [(first + k * 4 * SPB, f"-{intro_cells - k}", False) for k in range(intro_cells)]
cells_all += [(tb(b), str(i + 1), bool(line)) for i, (b, _l, _c, line) in enumerate(CELLS)]
cells_all += [(tb(68), "amen", True)]
for (t, label, heavy) in cells_all:
    x = sx(t)
    sd.line([x, LBL_BAND, x, ry - 1], fill=GRID_HVY if heavy else GRID_LT, width=2 if heavy else 1)
    sd.line([x, ry, x, ry + (12 if heavy else 8)], fill=TICK_HVY if heavy else TICK_LT, width=2 if heavy else 1)
    sd.text((x + 4, ry + 8), label, font=f_bar, fill=BNUM_HVY if heavy else BNUM_LT)
for (t0, name, col) in ACTS:
    x = sx(max(0.0, t0))
    sd.line([x, 0, x, ry - 1], fill=dim(col, 0.62 if LIGHT else 0.85), width=3)
    sd.text((x + 7, 1), name, font=f_act, fill=ink_of(col))
for t, label in MARKS:
    x = sx(t)
    sd.line([x, LBL_BAND, x, ry - 1], fill=MARK_COL, width=2)
    sd.text((x + 5, LBL_BAND + 2), label, font=f_mark, fill=MARK_COL)

# score lanes: pitch-placed blocks
MIDI_LO, MIDI_HI = 36, 80
ev_rects = []
for li, (name, kind, data, col, hh) in enumerate(LANE_DEFS):
    if kind != "ev":
        continue
    y0 = LBL_BAND + lane_y[li]
    bh = 12 if hh < 100 else 14
    last_lbl_x = -1e9
    for e in data:
        xa, xb = sx(e["t0"]), sx(max(e["t1"], e["t0"] + 0.12))
        m = e["midi"] if e["midi"] is not None else (MIDI_LO + MIDI_HI) / 2
        yy = y0 + 4 + (MIDI_HI - m) / (MIDI_HI - MIDI_LO) * (hh - 8 - bh)
        c = col if e["voice"] == "instr" else WHO_COL.get(e["who"], WHO_COL[None])
        sd.rounded_rectangle([xa, yy, xb, yy + bh], radius=4, fill=blk_fill(c), outline=blk_line(c), width=1)
        ev_rects.append((li, e["t0"], e["t1"], int(yy), int(yy + bh), c))
        want = e["voice"] == "lead" or name in ("chords · cells", "melody (sine)", "stamp") or xa - last_lbl_x > 64
        if want:
            ly = max(y0 + 1, yy - 19) if yy - y0 > 20 else min(y0 + hh - 18, yy + bh + 2)
            sd.text((xa + 1, ly), e["word"], font=f_tiny, fill=blk_line(c))
            last_lbl_x = xa
print("  score lanes (words · notes · cells · hits)", flush=True)

# audio lanes: clips with in-block waveforms
ncols = SWm
lane_clips = [[] for _ in range(NLANE)]
for li, (name, kind, sig, col, hh) in enumerate(LANE_DEFS):
    if kind != "au":
        continue
    y0 = LBL_BAND + lane_y[li]
    mid = y0 + hh // 2
    per = N // ncols
    e = np.sqrt((sig[: per * ncols].reshape(ncols, per) ** 2).mean(axis=1))
    if e.max() > 0:
        e = (e / e.max()) ** 0.8
    fill, line, wave = clip_fill(col), clip_line(col), wave_of(col)
    lane_clips[li] = clips_of(sig, LANE_THRESH.get(name, 0.11))
    for (t0, t1) in lane_clips[li]:
        xa, xb = sx(t0), sx(t1)
        sd.rounded_rectangle([xa, y0 + 3, xb, y0 + hh - 3], radius=5, fill=fill, outline=line, width=2)
        amax = hh // 2 - 6
        for x in range(xa + 2, xb - 1):
            ci = x - PAD_L
            if 0 <= ci < ncols:
                hgt = int(e[ci] * amax)
                if hgt > 0:
                    sd.line([x, mid - hgt, x, mid + hgt], fill=wave)
    print(f"  lane {name}", flush=True)
strip_np = np.array(strip)
del strip, sd

# ---------------------------------------------------------------- chrome
TITLE = "amazing grace" + ("  release master" if "release" in os.path.basename(MP3) else "  " + os.path.basename(MP3))
chrome = Image.new("RGB", (W, H), BG)
cd = ImageDraw.Draw(chrome)
cd.text((40, 18), TITLE, font=f_title, fill=INK)
MM_X0, MM_X1, MM_Y0, MM_Y1 = 40, W - 40, 62, 88
for i, (t0, name, col) in enumerate(ACTS):
    xa = MM_X0 + int(max(0.0, t0) / DUR * (MM_X1 - MM_X0))
    xb = MM_X0 + int(min(DUR, ACT_END[i]) / DUR * (MM_X1 - MM_X0))
    cd.rectangle([xa, MM_Y0, xb, MM_Y1], fill=mm_fill(col))
    cd.text((xa + 6, MM_Y0 + 5), name, font=f_tiny, fill=ink_of(col))
cd.rectangle([MM_X0, MM_Y0, MM_X1, MM_Y1], outline=MM_OUTLINE)
for li, (name, kind, _d, col, hh) in enumerate(LANE_DEFS):
    y0 = LANES_TOP + lane_y[li]
    cd.rectangle([10, y0, GUT - 6, y0 + hh - 1], fill=LANE_BG)
    cd.text((18, y0 + hh // 2), name, font=f_lbl, fill=ink_of(col), anchor="lm")
cd.line([GUT - 2, STRIP_TOP, GUT - 2, STRIP_BOT], fill=GUT_LINE)
cd.text((40, H - 34),
        "score receipt: amazing-grace-score.json (sing-amazing.mjs / c/amazinhym.c CELLS) / true stems from the bake"
        " (bed band-split as labelled) / G major / 70 BPM / cells of 4 (6 on the holds), pickup a- at 6.0 s",
        font=f_tiny, fill=FOOT)

CARD_W, CARD_H = 560, 60 + 34 * len(ACTS)
CX, CY = W - 30 - CARD_W, H - 44 - CARD_H
ROW_H = 34
cd.rounded_rectangle([CX, CY, CX + CARD_W, CY + CARD_H], radius=10, fill=CARD_BG, outline=CARD_EDGE, width=2)
cd.text((CX + 18, CY + 10), "the hymn", font=f_lbl, fill=CARD_HDR)
for i, (t0, name, col) in enumerate(ACTS):
    cd.text((CX + 18, CY + 42 + i * ROW_H), name, font=f_act, fill=mute_of(col))
chrome_np = np.array(chrome)
del chrome, cd

# ---------------------------------------------------------------- karaoke
def act_index(t):
    ai = 0
    for i, (t0, _n, _c) in enumerate(ACTS):
        if t >= t0:
            ai = i
    return ai

MIN_SHOW = 0.35

def karaoke_fill(dd, x, y, disp, font, col, p, k=1.0):
    dd.text((x, y), disp, font=font, fill=blend(BG, col, 0.40 * k), anchor="lm")
    n = max(1, len(disp))
    kk = max(1, min(n, int(math.ceil(p * n))))
    dd.text((x, y), disp[:kk], font=font, fill=blend(BG, col, k), anchor="lm")
    return dd.textlength(disp[:kk], font=font)

# three rails: the sung words (vowel-stretched, phonics fill), the score's
# note names under them, and the cell chords — the same time axis as the
# strip above, so a held "meeeee" and its B3 block share an x
NOW_X = 700
SCR_X0, SCR_X1 = 220, CX - 16
SCR_Y0, SCR_Y1 = STRIP_BOT + 8, H - 42
RAIL_ITEMS = {
    "jeffrey": [(tk["disp"], tk["t0"], tk["t1"], tk) for tk in TOKENS],
    "notes":   [(note_name(e["midi"]), e["t0"], e["t1"], None) for e in NOTES],
    "chords":  [(f"{e['word']}", e["t0"], e["t1"], None) for e in CHORDS],
}
SCR_RAILS = ("jeffrey", "notes", "chords")
SCR_LABEL = {"jeffrey": "JEFFREY", "notes": "NOTES", "chords": "CHORDS"}
SCR_COL = {"jeffrey": WHO_COL["jeffrey"], "notes": (110, 220, 205), "chords": (130, 180, 245)}
SCR_SIZE = {"jeffrey": 84, "notes": 40, "chords": 40}
SCR_SHARE = {"jeffrey": 0.62, "notes": 0.19, "chords": 0.19}
LPPS = 190.0
SCR_LOOK = (SCR_X1 - NOW_X) / LPPS
SCR_FADE = 0.65
_meas = ImageDraw.Draw(Image.new("RGB", (8, 8)))
SCR_FONT_SIZES = (84, 72, 60, 48, 40, 34, 28, 24, 20)
SCR_FONT_BANK = {s: ImageFont.truetype(FONT_B, s) for s in SCR_FONT_SIZES}
SCR_MAXW = 900
rail_layout = {}
rail_band = {}
ytop = SCR_Y0
for rail in SCR_RAILS:
    h = (SCR_Y1 - SCR_Y0) * SCR_SHARE[rail]
    rail_band[rail] = (ytop, ytop + h)
    ytop += h
    items = sorted(RAIL_ITEMS[rail], key=lambda it: it[1])
    slot_until, layout = [], []
    for (disp, t0, t1, tk) in items:
        font = SCR_FONT_BANK[SCR_FONT_SIZES[-1]]
        for size in SCR_FONT_SIZES:
            if size > SCR_SIZE[rail]:
                continue
            if _meas.textlength(disp, font=SCR_FONT_BANK[size]) <= SCR_MAXW:
                font = SCR_FONT_BANK[size]
                break
        cell = _meas.textlength(disp, font=font) + 18
        slot = next((i for i, until in enumerate(slot_until) if t0 >= until - 1e-6), None)
        if slot is None:
            slot = len(slot_until)
            slot_until.append(0.0)
        slot_until[slot] = t0 + cell / LPPS
        layout.append((disp, t0, t1, tk, font, cell, slot))
    rail_layout[rail] = (layout, max(1, len(slot_until)))
print("lyric subtracks: " + ", ".join(f"{r}={rail_layout[r][1]}" for r in SCR_RAILS), flush=True)

def draw_scroller(img, dd, t):
    for rail in SCR_RAILS:
        band_top, band_bot = rail_band[rail]
        yc = (band_top + band_bot) / 2
        col = ink_of(SCR_COL[rail])
        dd.text((42, yc), SCR_LABEL[rail], font=f_tiny, fill=blend(BG, col, 0.78), anchor="lm")
        dd.line([SCR_X0 - 12, band_bot - 1, SCR_X1, band_bot - 1], fill=blend(BG, col, 0.16), width=1)
    dd.line([NOW_X, SCR_Y0, NOW_X, SCR_Y1], fill=blend(BG, PLAYHEAD, 0.55), width=2)
    for rail in SCR_RAILS:
        layout, slots = rail_layout[rail]
        band_top, band_bot = rail_band[rail]
        band_h = band_bot - band_top
        base = ink_of(SCR_COL[rail])
        for (disp, t0, t1, tk, font, cell, slot) in layout:
            x = NOW_X + (t0 - t) * LPPS
            t1v = max(t1, t0 + MIN_SHOW)
            if x > SCR_X1 or x + cell < SCR_X0 or t > t1v + SCR_FADE:
                continue
            active = t0 <= t < t1v
            if t < t0:
                u = max(0.0, 1.0 - (t0 - t) / SCR_LOOK)
                k = 0.30 + 0.45 * u
            elif active:
                k = 1.0
            else:
                k = 0.75 * (1.0 - min(1.0, (t - t1v) / SCR_FADE))
            k *= max(0.0, min(1.0, (SCR_X1 - x) / 130.0))
            slot_top = band_top + slot * band_h / slots
            slot_bot = band_top + (slot + 1) * band_h / slots
            yc = (slot_top + slot_bot) / 2
            x0 = max(SCR_X0, int(math.floor(x)))
            x1 = min(SCR_X1, int(math.ceil(x + cell)))
            if x1 <= x0:
                continue
            y0, y1 = int(math.floor(slot_top + 1)), int(math.ceil(slot_bot - 1))
            tile = img.crop((x0, y0, x1, y1))
            td = ImageDraw.Draw(tile)
            tx, ty = x - x0, yc - y0
            if active:
                p = max(0.0, min(1.0, (t - t0) / max(1e-6, t1 - t0)))
                lw = karaoke_fill(td, tx, ty, disp, font, base, p, k) if rail == "jeffrey" else 0
                if rail != "jeffrey":
                    td.text((tx, ty), disp, font=font, fill=blend(BG, base, k), anchor="lm")
                    lw = td.textlength(disp, font=font)
                uy = min(y1 - y0 - 4, int(ty + font.size / 2 + 1))
                td.rectangle([tx, uy, tx + max(2, lw), uy + 3], fill=base)
            else:
                td.text((tx, ty), disp, font=font, fill=blend(BG, base, k), anchor="lm")
            img.paste(tile, (x0, y0))

# ---------------------------------------------------------------- frames
NF = int(round((T_TO - T_FROM) * FPS))
print(f"encoding {NF} frames → {OUT}", flush=True)
ff = subprocess.Popen(["ffmpeg", "-y", "-v", "error",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
    "-ss", str(T_FROM), "-i", MP3, "-map", "0:v", "-map", "1:a",
    "-c:v", "libx264", "-preset", "veryfast", "-crf", "18", "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "256k", "-movflags", "+faststart", "-shortest", OUT],
    stdin=subprocess.PIPE)
t_start = time.time()
for f in range(NF):
    t = T_FROM + f / FPS
    frame = chrome_np.copy()
    off = int(round(t * PPS))
    frame[STRIP_TOP:STRIP_BOT, GUT:W] = strip_np[:, off:off + SCROLL_W]
    active_rects = []
    for li in range(NLANE):
        if LANE_DEFS[li][1] != "au":
            continue
        for (t0, t1) in lane_clips[li]:
            if t0 <= t < t1:
                xa = max(GUT, GUT + sx(t0) - off)
                xb = min(W, GUT + sx(t1) - off)
                if xb > xa:
                    y0f = LANES_TOP + lane_y[li]
                    hh = LANE_DEFS[li][4]
                    reg = frame[y0f:y0f + hh, xa:xb]
                    if LIGHT:
                        frame[y0f:y0f + hh, xa:xb] = np.clip(reg.astype(np.int16) * 84 // 100, 0, 255).astype(np.uint8)
                    else:
                        frame[y0f:y0f + hh, xa:xb] = np.clip(reg.astype(np.uint16) * 16 // 10, 0, 255).astype(np.uint8)
                    active_rects.append((xa, y0f, xb, hh, LANE_DEFS[li][3]))
                break
    frame[STRIP_TOP:STRIP_BOT, PLAY_X - 1:PLAY_X + 1] = PLAYHEAD
    mx = MM_X0 + int(t / DUR * (MM_X1 - MM_X0))
    frame[MM_Y0:MM_Y1 + 1, mx:mx + 2] = MMCUR
    img = Image.fromarray(frame)
    dd = ImageDraw.Draw(img)
    for (xa, y0f, xb, hh, col) in active_rects:
        dd.rectangle([xa, y0f + 1, xb - 1, y0f + hh - 2], outline=ink_of(col), width=3)
    for (li, t0, t1, yb0, yb1, col) in ev_rects:
        if t0 <= t < max(t1, t0 + 0.2):
            xa = max(GUT, GUT + sx(t0) - off)
            xb = min(W, GUT + sx(max(t1, t0 + 0.12)) - off)
            if xb > xa:
                dd.rectangle([xa, STRIP_TOP + yb0, xb, STRIP_TOP + yb1], outline=ink_of(col), width=2)
    draw_scroller(img, dd, t)
    ai = act_index(t)
    for i, (t0, name, col) in enumerate(ACTS):
        ry2 = CY + 42 + i * ROW_H
        if i == ai:
            dd.rectangle([CX + 10, ry2 - 3, CX + CARD_W - 10, ry2 + 27], fill=ROW_HL)
            dd.text((CX + 18, ry2), name, font=f_act, fill=ink_of(col))
            u = (t - max(0.0, t0)) / max(0.1, ACT_END[i] - max(0.0, t0))
            bx0, bx1 = CX + 18, CX + CARD_W - 22
            dd.rectangle([bx0, ry2 + 24, bx1, ry2 + 26], fill=BAR_TRACK)
            dd.rectangle([bx0, ry2 + 24, bx0 + int(u * (bx1 - bx0)), ry2 + 26], fill=ink_of(col))
    dd.text((W - 40, 14), f"{int(t) // 60}:{int(t) % 60:02d}.{int((t * 10) % 10)}", font=f_tc, fill=INK, anchor="ra")
    draw_loudness_meter(dd, LOUDNESS, t, (760, 10, W - 360, 56), f_tiny,
                        target_lufs=LUFS_TARGET, true_peak_ceiling=TP_CEILING,
                        colors={"background": CARD_BG, "outline": CARD_EDGE, "text": INK,
                                "muted": FOOT, "track": BAR_TRACK})
    ff.stdin.write(img.tobytes())
    if f % 300 == 0:
        print(f"  frame {f}/{NF}  ({time.time() - t_start:.0f}s)", flush=True)
ff.stdin.close()
ff.wait()
print(f"done in {time.time() - t_start:.0f}s -> {OUT}", flush=True)
