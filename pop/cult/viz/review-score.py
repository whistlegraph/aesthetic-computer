#!/usr/bin/env python3
# review-score.py — SCROLLING clip-timeline review video for the cult remix,
# mini-DAW style, adapted from pop/loner/viz/review-score4.py (the mature
# loner renderer). Per-stem lanes as colored clip/waveform blocks scroll
# under a fixed playhead; a bar/act ruler runs beneath them; the sung words
# are drawn twice — as pitch-placed blocks in the score lanes (the
# timeline.py treatment) and as big karaoke text at the moment they are
# sung, with WHO is singing (camille / alex / jeffrey) from the score
# receipt. The loner corner (stroke-redrawn whistlegraph) is replaced by an
# ACT RULER card: the nine-act narrative with the current act lit.
#
# Truth sources:
#   · out/cult-remix-v10.events.json — the score receipt (regenerate with
#     `node pop/cult/bin/render10.mjs`); word mapping ported from
#     bin/transcript.mjs. Receipt times are FULL-render seconds; the
#     shipped cut starts TRIM=15.95 s in (bar 8 of the render = 0:00).
#   · out/stems/v10-*.wav — TRUE per-bus stems from
#     `node pop/cult/bin/render10.mjs --stems` (vox / tube / music /
#     drums / signal). The music and drums buses are band-split into
#     sub-lanes (labelled with their split) — not demucs guesses.
#
#   pop/.venv/bin/python pop/cult/viz/review-score.py          # club cut
#   pop/.venv/bin/python pop/cult/viz/review-score.py --radio  # radio cut
#   pop/.venv/bin/python pop/cult/viz/review-score.py --light  # paper theme
#   --audio PATH   render against a different master (e.g. the release
#                  master out/cult-remix-final.mp3 — same trim/timeline,
#                  so the receipt aligns 1:1)
#   --out PATH     override the output mp4 path
#
# FINAL-video additions (@jeffrey's two asks):
#   · LYRICS SCROLLER — a teleprompter band along the bottom: upcoming
#     words/phrases approach a fixed now-line from the right, light up in
#     their singer's color while sung, then fade and sink away leftward.
#   · MULTI-VOICE KARAOKE — overlapping word events with different `who`
#     performers draw as a STACK of name-colored rows (camille / alex /
#     jeffrey top-to-bottom by register), so the dashStack's
#     three-people-one-pitch-28-ms-apart signature is visible, not
#     collapsed into one word.
#   · VOWEL STRETCHING + PHONICS FILL — held words elongate their stressed
#     vowel to match the sung duration ("daaaaaaaash", "ruuuuun reeeaaal
#     faaaast") in both the scroller and the stack, and the letters light
#     one by one (with a sweeping underline) in sync with the utterance.
import json, math, os, re, subprocess, sys, time
import numpy as np
from PIL import Image, ImageDraw, ImageFont

sr = 8000
W, H, FPS = 1920, 1080, 30
PPS = 260                       # timeline pixels per second (520 px / bar)

RADIO = "--radio" in sys.argv
LIGHT = "--light" in sys.argv
def argval(flag):
    if flag in sys.argv:
        i = sys.argv.index(flag)
        if i + 1 < len(sys.argv):
            return sys.argv[i + 1]
    return None
LANE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
OUTD = os.path.join(LANE_DIR, "out")
MP3 = os.path.join(OUTD, "cult-remix-radio.mp3" if RADIO else "cult-remix.mp3")
AUDIO_OVERRIDE = argval("--audio")
if AUDIO_OVERRIDE:
    MP3 = os.path.abspath(AUDIO_OVERRIDE)
OUT = os.path.join(OUTD, "cult-remix-radio-review-score.mp4" if RADIO
                   else "cult-remix-review-score.mp4")
if LIGHT:
    OUT = OUT.replace(".mp4", "-light.mp4")
OUT_OVERRIDE = argval("--out")
if OUT_OVERRIDE:
    OUT = os.path.abspath(OUT_OVERRIDE)
EVENTS_JSON = os.path.join(OUTD, "cult-remix-v10.events.json")
STEMS = os.path.join(OUTD, "stems")
FONT_B = "/Users/jas/aesthetic-computer/slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf"
FONT_R = "/Users/jas/aesthetic-computer/slab/menuband/Sources/MenuBand/Resources/ywft-processing-regular.ttf"

BPM = 120.0
BEAT = 60.0 / BPM               # 0.5 s
BAR = 4 * BEAT                  # 2.0 s
EIGHTH = BEAT / 2
TRIM = 15.95                    # shipped t = full-render t − TRIM
GRID0 = 8 * BAR - TRIM          # 0.05 — bar 8's downbeat in shipped time
BAR0 = 8                        # ruler numbers speak render bars (hook = 29)

# ---------------------------------------------------------------- audio in
def load(path, af=None, ss=None):
    args = ["ffmpeg", "-v", "error"]
    if ss is not None:
        args += ["-ss", str(ss)]
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

def stem(name, af=None):        # bus stem, trimmed to the shipped cut
    return fit(load(os.path.join(STEMS, f"v10-{name}.wav"), af=af, ss=TRIM))

# ---------------------------------------------------------------- receipt
receipt = json.load(open(EVENTS_JSON))
EVENTS = receipt["events"]
EXPLOSIONS = [{**e, "t": e["t"] - TRIM} for e in EVENTS
              if e.get("voice") == "spatial-explosion" and e["t"] >= TRIM]

WHO_COL = {"camille": (255, 140, 190), "alex": (150, 225, 130),
           "jeffrey": (130, 175, 255), None: (225, 222, 230)}

def words_of(name):             # ported from bin/transcript.mjs (v10.2 bank)
    if re.match(r"^runrealfast", name): return "run real fast"
    if re.match(r"^runitfast", name): return "run it fast"
    if re.match(r"^hideaway", name): return "hide away"
    if re.match(r"^iwannahide", name): return "i wanna hide"
    if re.match(r"^iwanna", name): return "i wanna"
    if re.match(r"^away", name): return "a-waaay"
    if re.match(r"^dotdotdash", name): return "dot dot dash"
    if name == "dotorg": return "whistlegraph dot org"
    if re.match(r"^(dot|voxdot)-", name): return "dot"
    if re.match(r"^alt-\d+-dot", name): return "dot"
    if re.match(r"^alt-\d+-cult", name): return "cult"
    if re.match(r"^alt-\d+-threeofus", name): return "the three of us"
    if re.match(r"^(cult|cultlong)-", name): return "cult"
    if re.match(r"^(dash|dashlong|bassdash|sos-dash)", name): return "dash"
    if name == "three-of-us": return "the three of us"
    return None

# word events in shipped time: (t0, t1, word, who, midi, voice)
PRIO = {"lead": 3, "cult": 3, "alt": 3, "sample": 3, "stretch": 3, "dash": 2,
        "bassdash": 2, "sosdash": 2, "dot": 1}
WORDS = []
for e in EVENTS:
    s = e.get("sample")
    if not isinstance(s, str):
        continue
    w = words_of(s)
    if w is None:
        continue
    t0 = e["t"] - TRIM
    if t0 < -0.3:
        continue
    dur = float(e.get("dur", 0.3)) or 0.3
    WORDS.append({"t0": max(0.0, t0), "t1": max(0.0, t0) + dur, "word": w,
                  "who": e.get("who"), "midi": e.get("midi"),
                  "voice": e["voice"], "prio": PRIO.get(e["voice"], 1)})
WORDS.sort(key=lambda e: e["t0"])
print(f"{len(WORDS)} word events from the receipt", flush=True)

# instrument banks (new in the current engine): lane clips, NOT karaoke.
# violin-secret = the act-IV violin desks; guitar-chug / guitar-wide = the
# dark flanged guitars; boing-b/d/g/e = the act-VII chord-change boings;
# accordion-secret = the 13.5 s musette swell under the act-IV turn, and
# accordion-b/d/g/e breathe on act VII's chord changes (drawn at their
# chord roots so the Bm·D·G·Em walk reads in-lane); waterhole = the
# opening watery-hole hit; plus the synth "guitar" voice (midi notes)
# that shadows the reply act.
def instr_of(name):
    if re.match(r"^violin", name): return "violin"
    if name == "guitar-chug": return "chug"
    if name == "guitar-wide": return "guitar wide"
    if re.match(r"^boing-", name): return "boing"
    if re.match(r"^accordion", name): return "accordion"
    if name == "waterhole": return "watery-hole"
    return None

INSTR_MIDI = {"violin": 64, "guitar wide": 56, "chug": 50, "boing": 46,
              "watery-hole": 60, "accordion": 62}
CHORD_MIDI = {"b": 59, "d": 62, "g": 67, "e": 64,   # accordion chord roots
              "secret": 55}
BOING_MIDI = {"b": 47, "d": 50, "g": 55, "e": 52}
INSTR = []
for e in EVENTS:
    s = e.get("sample")
    lbl = instr_of(s) if isinstance(s, str) else None
    if lbl is None and e["voice"].startswith("guitar"):
        lbl = "guitar"
    if lbl is None:
        continue
    t0 = e["t"] - TRIM
    if t0 < -0.3:
        continue
    dur = float(e.get("dur", 0.3)) or 0.3
    m = INSTR_MIDI.get(lbl, 56)
    if lbl == "accordion":
        m = CHORD_MIDI.get(s.split("-")[-1], m)
    elif lbl == "boing":
        m = BOING_MIDI.get(s.split("-")[-1], m)
    if e["voice"].startswith("guitar") and e.get("midi") is not None:
        m = min(69, max(43, e["midi"] + 19))    # spread the low notes in-lane
    if INSTR and INSTR[-1]["word"] == lbl and abs(INSTR[-1]["t0"] - max(0.0, t0)) < 0.12:
        continue                                # unison doubles draw once
    INSTR.append({"t0": max(0.0, t0), "t1": max(0.0, t0) + dur, "word": lbl,
                  "who": None, "midi": m, "voice": "instr", "prio": 0})
INSTR.sort(key=lambda e: e["t0"])
print(f"{len(INSTR)} instrument events (violin / guitars / boings / accordion / watery-hole)",
      flush=True)

# ---------------------------------------------------------------- lanes
# Two SCORE lanes drawn from the receipt (pitch-placed word blocks), then
# audio lanes from the true bus stems (music/drums band-split, labelled).
print("decoding stems...", flush=True)
EV_WORDS = [e for e in WORDS
            if e["voice"] in ("lead", "dot", "cult", "alt", "stretch", "material")]
KARAOKE = [e for e in WORDS if e["voice"] != "material"]   # grains are texture
EV_DASH = [e for e in WORDS if e["voice"] in ("dash", "bassdash", "sosdash")]

# ---------------------------------------------------------------- tokens
# Karaoke events fused into display TOKENS for the scroller and the
# multi-voice stack. Two fusion rules:
#   · simultaneity — same word starting within 140 ms (the dashStack's
#     28 ms offsets, the choir's 45 ms cults, jeffrey's sub octave) is ONE
#     token carrying the union of whos;
#   · melisma — the same word restated by the same voice while the first
#     is still ringing (runrealfast-hi + its -long melisma 0.8 s later)
#     extends the token instead of doubling it.
WHO_ORDER = {"camille": 0, "alex": 1, "jeffrey": 2}   # register, high→low
TOKENS = []
for e in sorted(KARAOKE, key=lambda e: e["t0"]):
    fused = False
    for tk in TOKENS[-10:]:
        if tk["word"] != e["word"]:
            continue
        simul = e["t0"] - tk["first"] <= 0.14
        melis = (e["voice"] == tk["voice"] and e["t0"] < tk["t1"] + 0.05
                 and e["t0"] - tk["first"] <= 1.2)
        if simul or melis:
            tk["t1"] = max(tk["t1"], e["t1"])
            if e["who"] and e["who"] not in tk["whos"]:
                tk["whos"].append(e["who"])
            tk["prio"] = max(tk["prio"], e["prio"])
            tk["alt"] = tk["alt"] or e["voice"] == "alt"
            tk["instances"].append(e)
            fused = True
            break
    if not fused:
        TOKENS.append({"word": e["word"], "t0": e["t0"], "t1": e["t1"],
                       "first": e["t0"], "whos": [e["who"]] if e["who"] else [],
                       "voice": e["voice"], "prio": e["prio"],
                       "alt": e["voice"] == "alt", "instances": [e]})
for tk in TOKENS:
    tk["whos"].sort(key=lambda w: WHO_ORDER.get(w, 9))
    # The early pitched dot triad predates performer metadata in the sample
    # names. Keep the receipt untouched, but show its three near-simultaneous
    # events as three instances on the three human rails.
    triad_dot = (tk["word"] == "dot" and not tk["whos"]
                 and len(tk["instances"]) >= 3
                 and tk["instances"][-1]["t0"] - tk["first"] <= 0.14)
    tk["display_whos"] = list(WHO_ORDER) if triad_dot else list(tk["whos"])
    tk["rail_spans"] = {}
    if triad_dot:
        # These source events are staggered by ~60 ms. Preserve each attack;
        # the old shared t0 is why the unison looked early on two rails.
        for who, inst in zip(tk["display_whos"], sorted(tk["instances"], key=lambda e: e["t0"])):
            tk["rail_spans"][who] = {"t0": inst["t0"], "t1": inst["t1"]}
    elif tk["display_whos"]:
        for who in tk["display_whos"]:
            own = [e for e in tk["instances"] if e["who"] == who]
            tk["rail_spans"][who] = {
                "t0": min(e["t0"] for e in own) if own else tk["t0"],
                "t1": max(e["t1"] for e in own) if own else tk["t1"],
            }
    else:
        tk["rail_spans"][None] = {"t0": tk["t0"], "t1": tk["t1"]}
print(f"{len(TOKENS)} lyric tokens "
      f"({sum(1 for tk in TOKENS if len(tk['whos']) > 1)} multi-voice)", flush=True)

# ---------------------------------------------------------------- vowels
# VOWEL STRETCHING — a held word elongates its stressed vowel to match the
# sung duration, so the mp4 reads the way the utterance sounds: "dash"
# held 1.5 s -> "daaaaaaaash"; the runrealfast melisma -> "ruuuuun
# reeeaaal faaaast". Baseline: a word held <= STRETCH_BASE s renders
# normally; past that, one repeated letter per STRETCH_RATE s of hold,
# capped at STRETCH_CAP total repeats so layout survives (an 8 s cult is
# still one readable cuuuuult). Duration source: the token's t1 - t0,
# which comes from the receipt event's `dur` (melisma fusion extends it).
# Multi-word tokens split the repeats across their words (function words
# skipped), and inside a chosen vowel GROUP ("ea" in real) the repeats
# spread over each vowel letter -> "reeeaaal", not "reeeeeal".
STRETCH_BASE = 0.35             # held this long or less: no elongation
STRETCH_RATE = 0.135            # one repeated letter per 135 ms of hold
STRETCH_CAP = 13                # total extra letters across the token
VOWSET = set("aeiou")
FUNC_WORDS = {"the", "of", "i", "it", "a"}   # never stretch these
NO_STRETCH = {"dot"}             # three dot events stay three dots, never "dooooot"

def vowel_groups(w):            # runs of vowels (y joins a group it follows)
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

def stretch_part(part, extra):  # elongate ONE word's main vowel group
    low = part.lower()
    gs = vowel_groups(low)
    if len(gs) > 1:             # a lone final silent-e never stretches
        gs = [g for g in gs if not (g[1] - g[0] == 1 and g[0] == len(low) - 1
                                    and low[g[0]] == "e")]
    if not gs or extra <= 0:
        return part
    a, b = max(gs, key=lambda g: g[1] - g[0])   # longest group; ties -> first
    letters = [k for k in range(a, b) if low[k] in VOWSET]
    if not letters:
        return part
    reps = {k: 0 for k in letters}
    for n in range(extra):      # spread repeats over the group's vowels
        reps[letters[n % len(letters)]] += 1
    return "".join(ch * (1 + reps.get(k, 0)) for k, ch in enumerate(part))

def stretch_word(word, dur):
    if word in NO_STRETCH:
        return word
    extra = min(STRETCH_CAP, int((dur - STRETCH_BASE) / STRETCH_RATE))
    if extra <= 0:
        return word
    parts = word.split(" ")
    idx = [i for i, p in enumerate(parts) if p.lower() not in FUNC_WORDS]
    if not idx:
        idx = list(range(len(parts)))
    n = len(idx)                # each word takes its share of the hold
    for j, pi in enumerate(idx):
        parts[pi] = stretch_part(parts[pi], extra // n + (1 if j < extra % n else 0))
    return " ".join(parts)

for tk in TOKENS:
    tk["disp"] = stretch_word(tk["word"], tk["t1"] - tk["t0"])
print(f"vowel stretching: {sum(1 for tk in TOKENS if tk['disp'] != tk['word'])}"
      f"/{len(TOKENS)} tokens elongated", flush=True)

def prog(tk, t, span=None):     # sung progress through one voice span, 0..1
    span = span or {"t0": tk["t0"], "t1": tk["t1"]}
    return max(0.0, min(1.0, (t - span["t0"]) /
                        max(1e-6, span["t1"] - span["t0"])))
ALANES = [   # (label, signal, color, clip-gate threshold)
    ("bed 250+",     stem("music", af="highpass=f=250"),  (150, 140, 220), 0.11),
    ("bass <250",    stem("music", af="lowpass=f=250"),   (200, 120, 235), 0.11),
    ("kick <150",    stem("drums", af="lowpass=f=150"),   (235, 110, 60),  0.11),
    # the kick's click transient dominates the highpassed drums lane, so the
    # gate opens lower here or the hats and skids never show
    ("perc+skids",   stem("drums", af="highpass=f=150"),  (240, 175, 90),  0.045),
    ("signal",       stem("signal"),                      (255, 215, 90),  0.08),
]
LANE_THRESH = {nm: th for (nm, _s, _c, th) in ALANES}
LANE_DEFS = [
    ("words", "ev", EV_WORDS, (110, 220, 205), 96),
    ("dashes", "ev", EV_DASH, (255, 170, 120), 78),
    ("instr", "ev", INSTR, (235, 130, 155), 52),
] + [(nm, "au", sig, col, 58) for (nm, sig, col, _th) in ALANES]
NLANE = len(LANE_DEFS)

# ---------------------------------------------------------------- clip gate
def clips_of(sig, thresh=0.11):
    hop = int(0.05 * sr)
    nfr = len(sig) // hop
    fr = sig[: nfr * hop].reshape(nfr, hop)
    r = np.sqrt((fr ** 2).mean(axis=1))
    if r.max() <= 0:
        return []
    act = r > r.max() * thresh
    i = 0
    while i < nfr:                       # bridge silent gaps < 0.15 s
        if not act[i]:
            j = i
            while j < nfr and not act[j]:
                j += 1
            if 0 < i and j < nfr and (j - i) < 3:
                act[i:j] = True
            i = j
        else:
            i += 1
    regions = []
    i = 0
    while i < nfr:                       # contiguous runs -> seconds
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
    merged = []
    for reg in regions:                  # merge gaps < 0.18 s
        if merged and reg[0] - merged[-1][1] < 0.18:
            merged[-1][1] = reg[1]
        else:
            merged.append(reg)
    def snap8(t):                        # snap edges to the 8th-note grid
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
    clips = []                           # split at global 4-bar boundaries
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

# ---------------------------------------------------------------- theme
# --light translates the palette the way loner's review-score4-light.py
# translated review-score4.py: warm paper ground, dark-ink text and
# playhead, lane colors deepened (v*0.55) wherever they were once drawn
# at full strength, clip bodies as pale tints of their lane color, and
# active clips DARKEN while they play instead of brightening.
def dim(col, k):
    return tuple(int(v * k) for v in col)

if LIGHT:
    BG = (246, 244, 240)
    LANE_BG = (234, 231, 226)
    INK = (28, 27, 25)          # title / karaoke / timecode ink
    PLAYHEAD = (28, 27, 25)
    MMCUR = (28, 27, 25)
    GRID_HVY, GRID_LT = (206, 203, 197), (222, 219, 213)
    TICK_HVY, TICK_LT = (105, 103, 99), (160, 157, 152)
    BNUM_HVY, BNUM_LT = (88, 86, 83), (150, 147, 142)
    MM_OUTLINE = (150, 147, 142)
    GUT_LINE = (200, 197, 192)
    FOOT = (110, 108, 104)
    MARK_COL = (168, 118, 20)
    CARD_BG, CARD_EDGE = (238, 235, 230), (200, 197, 192)
    CARD_HDR = (110, 108, 104)
    ROW_HL = (222, 218, 212)
    BAR_TRACK = (210, 207, 200)
    MUTE_GRAY = (120, 118, 114)
else:
    BG = (12, 11, 14)
    LANE_BG = (17, 16, 20)
    INK = (240, 238, 232)
    PLAYHEAD = (255, 245, 230)
    MMCUR = (245, 243, 238)
    GRID_HVY, GRID_LT = (34, 33, 40), (24, 23, 28)
    TICK_HVY, TICK_LT = (130, 128, 134), (78, 76, 82)
    BNUM_HVY, BNUM_LT = (150, 148, 152), (104, 102, 108)
    MM_OUTLINE = (58, 58, 64)
    GUT_LINE = (44, 44, 50)
    FOOT = (120, 118, 124)
    MARK_COL = (255, 210, 90)
    CARD_BG, CARD_EDGE = (17, 16, 21), (44, 44, 50)
    CARD_HDR = (150, 148, 155)
    ROW_HL = (28, 27, 34)
    BAR_TRACK = (40, 40, 46)
    MUTE_GRAY = (160, 158, 165)

def ink_of(col):                # a lane color as legible text/outline ink
    return dim(col, 0.55) if LIGHT else col
def mute_of(col):               # the same color, muted (inactive act rows)
    return tuple(int(v * 0.45 + 120) for v in col) if LIGHT else dim(col, 0.55)
def clip_fill(col):             # audio clip body
    return tuple(int(v * 0.25 + 191) for v in col) if LIGHT else dim(col, 0.22)
def clip_line(col):             # audio clip border
    return dim(col, 0.58) if LIGHT else dim(col, 0.78)
def wave_of(col):               # in-clip waveform
    return dim(col, 0.52) if LIGHT else dim(col, 0.60)
def blk_fill(col):              # score-lane word/instrument blocks
    return tuple(int(v * 0.30 + 165) for v in col) if LIGHT else dim(col, 0.42)
def blk_line(col):
    return dim(col, 0.60) if LIGHT else dim(col, 0.95)
def mm_fill(col):               # minimap act cells
    return tuple(int(v * 0.45 + 140) for v in col) if LIGHT else dim(col, 0.40)

# ---------------------------------------------------------------- layout
GUT = 165                       # fixed left gutter
SCROLL_W = W - GUT
PLAY_X = GUT + SCROLL_W // 2    # fixed playhead
LANE_GAP = 6
LBL_BAND = 22                   # act-name band at top of the strip
RULER_H = 30
STRIP_TOP = 100
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

# Acts in shipped time (render bar 8 = 0:00; receipt narrative − TRIM).
ACTS = [
    (0.00,   "II THREE VOICES",       (64, 190, 180)),
    (32.05,  "III THE MESSAGE",       (235, 150, 70)),
    (64.05,  "IV THE SECRET",         (205, 75, 85)),
    (80.05,  "V THE REPLY",           (120, 200, 120)),
    (112.05, "VI IT SPREADS",         (170, 130, 230)),
    (136.05, "VII THE WHOLE MESSAGE", (255, 210, 90)),
    (176.05, "VIII RECOGNITION",      (150, 160, 200)),
    (192.05, "IX CARRIER OFF",        (150, 150, 155)),
]
ACT_END = [a[0] for a in ACTS[1:]] + [DUR]
MARKS = [(0.05, "watery-hole"), (20.30, "whistlegraph.org"),
         (42.05, "bar 29: the sentence lands")]

f_title = ImageFont.truetype(FONT_B, 48)
f_lbl   = ImageFont.truetype(FONT_B, 24)
f_tiny  = ImageFont.truetype(FONT_R, 17)
f_mark  = ImageFont.truetype(FONT_R, 20)
f_bar   = ImageFont.truetype(FONT_R, 20)
f_act   = ImageFont.truetype(FONT_B, 25)
f_tc    = ImageFont.truetype(FONT_B, 56)
f_who   = ImageFont.truetype(FONT_B, 36)
# multi-voice stack: the row font shrinks as more voices sing at once;
# F_SIZES is also the step-down ladder for rows whose STRETCHED word
# (daaaaaaaash) would otherwise run under the act card
F_SIZES = [88, 64, 52, 42, 34, 28]
F_BANK = {s: ImageFont.truetype(FONT_B, s) for s in F_SIZES}
F_STACK = {1: F_BANK[88], 2: F_BANK[64], 3: F_BANK[52], 4: F_BANK[28]}
KAR_MAX_W = 1150                # stack text right edge stays off the act card
def fit_stack_font(dd, text, font):
    while dd.textlength(text, font=font) > KAR_MAX_W:
        smaller = [s for s in F_SIZES if s < font.size]
        if not smaller:
            break
        font = F_BANK[smaller[0]]
    return font
f_name  = ImageFont.truetype(FONT_B, 24)
f_scr   = ImageFont.truetype(FONT_B, 38)     # scroller: upcoming / past
f_scra  = ImageFont.truetype(FONT_B, 48)     # scroller: the word being sung

def sx(t):                      # strip x for time t
    return PAD_L + int(round(t * PPS))

# ---------------------------------------------------------------- strip
print("rendering timeline strip...", flush=True)
strip = Image.new("RGB", (STRIP_W, STRIP_H), BG)
sd = ImageDraw.Draw(strip)
x0m, x1m = sx(0), sx(DUR)

for li in range(NLANE):
    y0 = LBL_BAND + lane_y[li]
    sd.rectangle([x0m, y0, x1m, y0 + LANE_DEFS[li][4] - 1], fill=LANE_BG)

# beat grid + ruler (bar numbers speak RENDER bars: shipped 0:00 = bar 8)
ry = STRIP_H - RULER_H
k = 0
while True:
    bt = GRID0 + k * BAR
    if bt >= DUR:
        break
    x = sx(bt)
    heavy = (k % 4 == 0)
    gcol = GRID_HVY if heavy else GRID_LT
    sd.line([x, LBL_BAND, x, ry - 1], fill=gcol)
    tick = TICK_HVY if heavy else TICK_LT
    sd.line([x, ry, x, ry + (12 if heavy else 8)], fill=tick, width=2 if heavy else 1)
    sd.text((x + 4, ry + 8), str(k + BAR0), font=f_bar,
            fill=BNUM_HVY if heavy else BNUM_LT)
    k += 1

# act boundaries: heavy colored line + act name in the label band
for (t0, name, col) in ACTS:
    x = sx(max(0.0, t0))
    sd.line([x, 0, x, ry - 1], fill=dim(col, 0.62 if LIGHT else 0.85), width=3)
    sd.text((x + 7, 1), name, font=f_act, fill=ink_of(col))

# Event markers and the measure grid are timeline coordinates, not sound
# bodies. Freeze this layer before clips are drawn so the elastic pass can
# move clips across stationary bars instead of dragging the ruler with them.
for t, label in MARKS:
    x = sx(t)
    sd.line([x, LBL_BAND, x, ry - 1], fill=MARK_COL, width=2)
    sd.text((x + 5, LBL_BAND + 2), label, font=f_mark, fill=MARK_COL)
strip_base_np = np.array(strip)

# score lanes: pitch-placed word blocks from the receipt
MIDI_LO, MIDI_HI = 43, 69
ev_rects = []                   # (lane idx, t0, t1, y0, y1, col) for lighting
for li, (name, kind, data, col, hh) in enumerate(LANE_DEFS):
    if kind != "ev":
        continue
    y0 = LBL_BAND + lane_y[li]
    bh = 12
    last_lbl_x = -1e9
    for e in data:
        xa, xb = sx(e["t0"]), sx(max(e["t1"], e["t0"] + 0.12))
        m = e["midi"] if e["midi"] is not None else (MIDI_LO + MIDI_HI) / 2
        yy = y0 + 4 + (MIDI_HI - m) / (MIDI_HI - MIDI_LO) * (hh - 8 - bh)
        if e["voice"] != "instr" and e["who"] in WHO_ORDER:
            yy += (-5, 0, 5)[WHO_ORDER[e["who"]]]
        c = col if e["voice"] == "instr" else WHO_COL.get(e["who"], WHO_COL[None])
        sd.rounded_rectangle([xa, yy, xb, yy + bh], radius=4,
                             fill=blk_fill(c), outline=blk_line(c), width=1)
        ev_rects.append((li, e["t0"], e["t1"], int(yy), int(yy + bh), c))
        # word labels: every lead/cult/alt word; sparse for the dot/dash runs
        want = e["voice"] in ("lead", "cult", "alt", "instr") or xa - last_lbl_x > 64
        if want:
            ly = max(y0 + 1, yy - 19) if yy - y0 > 20 else min(y0 + hh - 18, yy + bh + 2)
            sd.text((xa + 1, ly), e["word"], font=f_tiny, fill=blk_line(c))
            last_lbl_x = xa
print("  score lanes (words · dashes)", flush=True)

# audio lanes: clips with in-block waveforms (loner treatment, verbatim)
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
    fill = clip_fill(col)
    line = clip_line(col)
    wave = wave_of(col)
    lane_clips[li] = clips_of(sig, LANE_THRESH.get(name, 0.11))
    for (t0, t1) in lane_clips[li]:
        xa, xb = sx(t0), sx(t1)
        sd.rounded_rectangle([xa, y0 + 3, xb, y0 + hh - 3], radius=5,
                             fill=fill, outline=line, width=2)
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
TITLE = "whistlegraph cult --- remix (v10)" + (
    "  radio cut" if RADIO else
    "  release master" if AUDIO_OVERRIDE and "final" in os.path.basename(MP3)
    else "  club cut")
chrome = Image.new("RGB", (W, H), BG)
cd = ImageDraw.Draw(chrome)
cd.text((40, 18), TITLE, font=f_title, fill=INK)

MM_X0, MM_X1, MM_Y0, MM_Y1 = 40, 1880, 62, 88
for i, (t0, name, col) in enumerate(ACTS):
    if t0 >= DUR:               # probe clips shorter than the record
        continue
    xa = MM_X0 + int(max(0.0, t0) / DUR * (MM_X1 - MM_X0))
    xb = MM_X0 + int(min(DUR, ACT_END[i]) / DUR * (MM_X1 - MM_X0))
    cd.rectangle([xa, MM_Y0, xb, MM_Y1], fill=mm_fill(col))
    cd.text((xa + 6, MM_Y0 + 5), name.split(" ")[0], font=f_tiny, fill=ink_of(col))
cd.rectangle([MM_X0, MM_Y0, MM_X1, MM_Y1], outline=MM_OUTLINE)

# gutter: lane row stubs + labels
for li, (name, kind, _d, col, hh) in enumerate(LANE_DEFS):
    y0 = LANES_TOP + lane_y[li]
    cd.rectangle([10, y0, GUT - 6, y0 + hh - 1], fill=LANE_BG)
    cd.text((18, y0 + hh // 2), name, font=f_lbl, fill=ink_of(col), anchor="lm")
cd.line([GUT - 2, STRIP_TOP, GUT - 2, STRIP_BOT], fill=GUT_LINE)
cd.text((40, H - 34), "score receipt: cult-remix-v10.events.json / lanes from per-bus stems"
        " (music + drums band-split as labelled) / B minor / 120 BPM",
        font=f_tiny, fill=FOOT)

# act ruler card, bottom-right (the loner whistlegraph corner's replacement)
CARD_W, CARD_H = 470, 322
CX, CY = W - 30 - CARD_W, H - 44 - CARD_H
ROW_H = 34
cd.rounded_rectangle([CX, CY, CX + CARD_W, CY + CARD_H], radius=10,
                     fill=CARD_BG, outline=CARD_EDGE, width=2)
cd.text((CX + 18, CY + 10), "acts", font=f_lbl, fill=CARD_HDR)
for i, (t0, name, col) in enumerate(ACTS):
    ry2 = CY + 42 + i * ROW_H
    cd.text((CX + 18, ry2), name, font=f_act, fill=mute_of(col))
chrome_np = np.array(chrome)
del chrome, cd

# ---------------------------------------------------------------- karaoke
def act_index(t):
    ai = 0
    for i, (t0, _n, _c) in enumerate(ACTS):
        if t >= t0:
            ai = i
    return ai

MIN_SHOW = 0.35                 # a dot stays readable

def blend(c0, c1, u):           # c0 → c1 as u goes 0 → 1
    return tuple(int(a + (b - a) * u) for a, b in zip(c0, c1))

def tok_col(tk):                # a token's base ink (single-who or neutral)
    if len(tk["display_whos"]) == 1:
        return ink_of(WHO_COL[tk["display_whos"][0]])
    return INK                  # unattributed words read in plain ink

def karaoke_fill(dd, x, y, disp, font, col, p, k=1.0):
    # PROGRESSIVE KARAOKE FILL — the whole (vowel-stretched) word sits
    # dimmed, and the letters sung so far light up one by one, so each
    # letter of daaaaaaaash brightens as its sound happens (the phonics
    # read-along). Returns the lit prefix's pixel width for underlines.
    dd.text((x, y), disp, font=font, fill=blend(BG, col, 0.40 * k), anchor="lm")
    n = max(1, len(disp))
    # Dot is one attacked syllable, not three phonics. Light the whole word
    # at its own receipt onset; only sustained words crawl letter-by-letter.
    kk = n if disp == "dot" else max(1, min(n, int(math.ceil(p * n))))
    dd.text((x, y), disp[:kk], font=font, fill=blend(BG, col, k), anchor="lm")
    return dd.textlength(disp[:kk], font=font)

# ── multi-voice stack (the big karaoke, one row per live voice) ──────
# Live tokens group by word; a word with several `who` performers expands
# to one name-colored row per person (camille/alex/jeffrey, register
# order). If the moment is too crowded, multi-voice words COMPRESS to a
# single chorus row — the word drawn once per performer, offset copies in
# each color, all names listed — so nobody is ever dropped.
KAR_TOP = STRIP_BOT + 14
def rows_at(t):
    candidates = []
    for tk in TOKENS:
        for who, span in tk["rail_spans"].items():
            if span["t0"] <= t < max(span["t1"], span["t0"] + MIN_SHOW):
                candidates.append({"word": tk["word"],
                                   "whos": [who] if who else [],
                                   "alt": tk["alt"], "tk": tk,
                                   "span": span, "prio": tk["prio"]})
    candidates.sort(key=lambda r: (-r["prio"], -r["span"]["t0"],
                                   WHO_ORDER.get(r["whos"][0], 9) if r["whos"] else 9))
    rows, seen = [], set()
    for row in candidates:
        key = (row["word"], row["whos"][0] if row["whos"] else None)
        if key in seen:
            continue
        seen.add(key)
        rows.append(row)
    rows.sort(key=lambda r: (WHO_ORDER.get(r["whos"][0], 9) if r["whos"] else 9,
                             -r["prio"]))
    return rows[:4]

# ── lyrics scroller (teleprompter band along the bottom) ─────────────
# Time maps to x exactly like the timeline above. Camille, Alex, Jeffrey,
# and unattributed/source words own fixed vertical rails, so three people
# saying "dot" are three instances—not one stretched "dooooot" or three
# offset drawings fighting for the same row.
NOW_X = 350
SCR_X0, SCR_X1 = 190, 1404      # labels own the left; act card owns the right
SCR_Y0, SCR_Y1 = H - 174, H - 42
SCR_RAILS = ("camille", "alex", "jeffrey", None)
SCR_ROW_Y = {"camille": H - 150, "alex": H - 118,
             "jeffrey": H - 86, None: H - 54}
SCR_LABEL = {"camille": "CAMILLE", "alex": "ALEX",
             "jeffrey": "JEFFREY", None: "WORDS"}
LPPS = 150.0
SCR_LOOK = (SCR_X1 - NOW_X) / LPPS      # ≈ 7.0 s of upcoming lyric
SCR_FADE = 1.6                          # seconds to fall away after t1

_meas = ImageDraw.Draw(Image.new("RGB", (8, 8)))
SCR_FONT_SIZES = (38, 34, 30, 26, 22, 19)
SCR_FONT_BANK = {s: ImageFont.truetype(FONT_B, s) for s in SCR_FONT_SIZES}
rail_items = {r: [] for r in SCR_RAILS}
for tk in TOKENS:
    tk["rails"] = list(tk["rail_spans"])
    tk["scr_font"] = {}
    for rail in tk["rails"]:
        rail_items[rail].append((tk, tk["rail_spans"][rail]))
for rail, items in rail_items.items():
    items.sort(key=lambda item: item[1]["t0"])
    for i, (tk, span) in enumerate(items):
        # Fit to the time cell before the next word on this SAME singer rail.
        # Font size is precomputed, so rows cannot flicker or collide.
        avail = ((items[i + 1][1]["t0"] - span["t0"]) * LPPS - 10
                 if i + 1 < len(items) else SCR_X1 - NOW_X)
        cell = max(8, avail)
        font = SCR_FONT_BANK[SCR_FONT_SIZES[-1]]
        for size in SCR_FONT_SIZES:
            candidate = SCR_FONT_BANK[size]
            if _meas.textlength(tk["disp"], font=candidate) <= cell:
                font = candidate
                break
        tk["scr_font"][rail] = font
        tk.setdefault("scr_cell", {})[rail] = cell

for tk in TOKENS:
    tk["w"] = max(_meas.textlength(tk["disp"], font=f)
                  for f in tk["scr_font"].values())

def draw_scroller(img, dd, t):
    for rail in SCR_RAILS:
        yc = SCR_ROW_Y[rail]
        col = ink_of(WHO_COL[rail])
        dd.text((42, yc), SCR_LABEL[rail], font=f_tiny,
                fill=blend(BG, col, 0.78), anchor="lm")
        dd.line([SCR_X0 - 12, yc + 15, SCR_X1, yc + 15],
                fill=blend(BG, col, 0.16), width=1)
    dd.line([NOW_X, SCR_Y0, NOW_X, SCR_Y1],
            fill=blend(BG, PLAYHEAD, 0.55), width=2)
    for tk in TOKENS:
        for rail in tk["rails"]:
            span = tk["rail_spans"][rail]
            x = NOW_X + (span["t0"] - t) * LPPS
            t1v = max(span["t1"], span["t0"] + MIN_SHOW)
            if x > SCR_X1 or x < SCR_X0 or t > t1v + SCR_FADE:
                continue
            active = span["t0"] <= t < t1v
            if t < span["t0"]:
                u = max(0.0, 1.0 - (span["t0"] - t) / SCR_LOOK)
                k = 0.30 + 0.45 * u
            elif active:
                k = 1.0
            else:
                past = min(1.0, (t - t1v) / SCR_FADE)
                k = 0.75 * (1.0 - past)
            k *= max(0.0, min(1.0, (SCR_X1 - x) / 130.0))
            p = prog(tk, t, span)
            yc = SCR_ROW_Y[rail]
            font = tk["scr_font"][rail]
            base = ink_of(WHO_COL[rail])
            # Draw into the exact time cell and paste it back. This hard clip
            # is what makes adjacent lyric tracks incapable of overlapping,
            # even when a melisma is still active at the next word's attack.
            x0 = max(SCR_X0, int(math.floor(x)))
            x1 = min(SCR_X1, int(math.ceil(x + tk["scr_cell"][rail])))
            if x1 <= x0:
                continue
            y0, y1 = int(yc - 22), int(yc + 23)
            tile = img.crop((x0, y0, x1, y1))
            td = ImageDraw.Draw(tile)
            tx, ty = x - x0, yc - y0
            if active:
                lw = karaoke_fill(td, tx, ty, tk["disp"], font, base, p, k)
            else:
                td.text((tx, ty), tk["disp"], font=font,
                        fill=blend(BG, base, k), anchor="lm")
                lw = 0
            if active:          # one swept underline on each performer rail
                uy = ty + font.size // 2 + 2
                td.rectangle([tx, uy, tx + max(2, lw), uy + 3], fill=base)
            img.paste(tile, (x0, y0))

# The same damped-spring receipt that spatializes the stems drives the DAW
# lanes. They separate as bodies, cross home, overshoot, and settle; during
# the impact, thin horizontal shards disagree briefly about displacement.
LANE_FORCE = (-0.42, 0.96, -0.86, -0.86, -0.86, 0.70, 0.70, -0.98)
def ease(u):
    u = max(0.0, min(1.0, u))
    return u * u * (3 - 2 * u)

def elastic_state(t):
    spring = 0.0
    fracture = 0.0
    for ex in EXPLOSIONS:
        age = t - ex["t"]
        dur = ex.get("dur", 4.0)
        if 0 <= age <= dur:
            tail = ease((dur - age) / 0.48)
            spring += (ex.get("strength", 1.0) * math.exp(-ex.get("damping", 0.6) * age)
                       * math.sin(2 * math.pi * ex.get("springHz", 1.0) * age) * tail)
            fracture = max(fracture, ex.get("glitch", 0.5) * math.exp(-2.7 * age)
                           * ease(age / 0.022) * tail)
    return spring, fracture

# ---------------------------------------------------------------- frames
NF = int(DUR * FPS)
print(f"encoding {NF} frames...", flush=True)
ff = subprocess.Popen(["ffmpeg", "-y", "-v", "error",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
    "-i", MP3, "-map", "0:v", "-map", "1:a",
    "-c:v", "libx264", "-preset", "veryfast", "-crf", "18", "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "256k", "-shortest", OUT], stdin=subprocess.PIPE)

t_start = time.time()
for f in range(NF):
    t = f / FPS
    frame = chrome_np.copy()
    off = int(round(t * PPS))
    frame[STRIP_TOP:STRIP_BOT, GUT:W] = strip_np[:, off:off + SCROLL_W]
    spring, fracture = elastic_state(t)
    lane_dx = [0] * NLANE
    if abs(spring) > 0.002 or fracture > 0.01:
        for li in range(NLANE):
            y0f = LANES_TOP + lane_y[li]
            hh = LANE_DEFS[li][4]
            sy0 = LBL_BAND + lane_y[li]
            src = strip_np[sy0:sy0 + hh, off:off + SCROLL_W]
            base = strip_base_np[sy0:sy0 + hh, off:off + SCROLL_W]
            moving = np.any(src != base, axis=2)
            dst = frame[y0f:y0f + hh, GUT:W]
            dst[:] = base
            dx = int(round(spring * LANE_FORCE[li] * (72 + li * 5)))
            lane_dx[li] = dx
            bands = 5 if fracture > 0.035 else 1
            for band in range(bands):
                ya = band * hh // bands
                yb = (band + 1) * hh // bands
                shard = int(round(fracture * (10 + li * 1.8) * (-1 if band & 1 else 1)))
                ddx = dx + shard
                if ddx >= 0:
                    if ddx < SCROLL_W:
                        s = src[ya:yb, :SCROLL_W - ddx]
                        m = moving[ya:yb, :SCROLL_W - ddx]
                        d = dst[ya:yb, ddx:]
                        d[m] = s[m]
                elif -ddx < SCROLL_W:
                    s = src[ya:yb, -ddx:]
                    m = moving[ya:yb, -ddx:]
                    d = dst[ya:yb, :SCROLL_W + ddx]
                    d[m] = s[m]
    # active audio clips light up while they play
    active_rects = []
    for li in range(NLANE):
        if LANE_DEFS[li][1] != "au":
            continue
        for (t0, t1) in lane_clips[li]:
            if t0 <= t < t1:
                xa = max(GUT, GUT + sx(t0) - off + lane_dx[li])
                xb = min(W, GUT + sx(t1) - off + lane_dx[li])
                if xb > xa:
                    y0f = LANES_TOP + lane_y[li]
                    hh = LANE_DEFS[li][4]
                    reg = frame[y0f:y0f + hh, xa:xb]
                    if LIGHT:   # active clips DEEPEN on paper
                        frame[y0f:y0f + hh, xa:xb] = np.clip(
                            reg.astype(np.int16) * 84 // 100, 0, 255).astype(np.uint8)
                    else:       # active clips light up
                        frame[y0f:y0f + hh, xa:xb] = np.clip(
                            reg.astype(np.uint16) * 16 // 10, 0, 255).astype(np.uint8)
                    active_rects.append((xa, y0f, xb, hh, LANE_DEFS[li][3]))
                break
    # playhead + minimap position
    frame[STRIP_TOP:STRIP_BOT, PLAY_X - 1:PLAY_X + 1] = PLAYHEAD
    mx = MM_X0 + int(t / DUR * (MM_X1 - MM_X0))
    frame[MM_Y0:MM_Y1 + 1, mx:mx + 2] = MMCUR

    img = Image.fromarray(frame)
    dd = ImageDraw.Draw(img)
    for (xa, y0f, xb, hh, col) in active_rects:
        dd.rectangle([xa, y0f + 1, xb - 1, y0f + hh - 2], outline=ink_of(col), width=3)
    # active word blocks light up in the score lanes
    for (li, t0, t1, yb0, yb1, col) in ev_rects:
        if t0 <= t < max(t1, t0 + 0.2):
            xa = max(GUT, GUT + sx(t0) - off + lane_dx[li])
            xb = min(W, GUT + sx(max(t1, t0 + 0.12)) - off + lane_dx[li])
            if xb > xa:
                dd.rectangle([xa, STRIP_TOP + yb0, xb, STRIP_TOP + yb1],
                             outline=ink_of(col), width=2)
    if abs(spring) > 0.025 or fracture > 0.04:
        cy = (STRIP_TOP + STRIP_BOT) // 2
        radius = int(34 + abs(spring) * 170 + fracture * 55)
        col = blend(BG, MARK_COL, min(0.72, 0.18 + abs(spring) * 0.42 + fracture * 0.25))
        dd.ellipse([PLAY_X - radius, cy - radius, PLAY_X + radius, cy + radius],
                   outline=col, width=max(1, int(2 + fracture * 5)))
    # karaoke: EVERY live voice as its own name-colored row — the
    # dashStack's three simultaneous people stack camille/alex/jeffrey
    # top-to-bottom by register instead of collapsing into one word
    rows = rows_at(t)
    if rows:
        font0 = F_STACK[len(rows)]
        rh = font0.size + 18
        y = KAR_TOP + rh // 2 + 4
        for row in rows:
            whos = row["whos"]
            rtk = row["tk"]
            disp = rtk["disp"]  # the vowel-stretched word
            p = prog(rtk, t, row["span"])  # this singer's receipt timing
            font = fit_stack_font(dd, disp, font0)
            uy = y + font.size // 2 + 5
            if len(whos) > 1:   # chorus row: every performer, one word
                lw = 0
                for i, who in enumerate(whos):
                    dd.text((66, y + (i - (len(whos) - 1) / 2) * 20), who,
                            font=f_name, fill=ink_of(WHO_COL[who]), anchor="lm")
                    lw = karaoke_fill(dd, 250 + i * 3,
                                      y + (i - (len(whos) - 1) / 2) * 10,
                                      disp, font, ink_of(WHO_COL[who]), p)
                dd.rectangle([250, uy + 10, 250 + max(2, lw), uy + 14],
                             fill=ink_of(WHO_COL[whos[0]]))
            elif whos:
                col = ink_of(WHO_COL[whos[0]])
                dd.text((66, y), whos[0], font=f_name, fill=col, anchor="lm")
                lw = karaoke_fill(dd, 250, y, disp, font, col, p)
                dd.rectangle([250, uy, 250 + max(2, lw), uy + 4], fill=col)
            else:
                if row["alt"]:
                    dd.text((66, y), "another cult", font=f_name,
                            fill=MUTE_GRAY, anchor="lm")
                lw = karaoke_fill(dd, 250, y, disp, font, INK, p)
                dd.rectangle([250, uy, 250 + max(2, lw), uy + 4], fill=INK)
            y += rh
    # upcoming-lyrics teleprompter
    draw_scroller(img, dd, t)
    # act card: current act lit, with a progress bar
    ai = act_index(t)
    for i, (t0, name, col) in enumerate(ACTS):
        ry2 = CY + 42 + i * ROW_H
        if i == ai:
            dd.rectangle([CX + 10, ry2 - 3, CX + CARD_W - 10, ry2 + 27],
                         fill=ROW_HL)
            dd.text((CX + 18, ry2), name, font=f_act, fill=ink_of(col))
            u = (t - max(0.0, t0)) / max(0.1, ACT_END[i] - max(0.0, t0))
            bx0, bx1 = CX + 18, CX + CARD_W - 22
            dd.rectangle([bx0, ry2 + 24, bx1, ry2 + 26], fill=BAR_TRACK)
            dd.rectangle([bx0, ry2 + 24, bx0 + int(u * (bx1 - bx0)), ry2 + 26],
                         fill=ink_of(col))
    dd.text((W - 40, 14),
            f"{int(t) // 60}:{int(t) % 60:02d}.{int((t * 10) % 10)}",
            font=f_tc, fill=INK, anchor="ra")
    ff.stdin.write(img.tobytes())
    if f % 300 == 0:
        print(f"  frame {f}/{NF}  ({time.time() - t_start:.0f}s)", flush=True)
ff.stdin.close()
ff.wait()
print(f"done in {time.time() - t_start:.0f}s -> {OUT}")
