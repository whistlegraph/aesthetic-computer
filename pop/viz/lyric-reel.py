#!/usr/bin/env python3
# lyric-reel.py — the Instagram lyric reel for whistlegraph pop tracks.
#
# A 1080x1920 portrait video: the record's tracks fly by behind (every lane
# of the score receipt and the bus stems as translucent blocks scrolling
# right-to-left), and the lyrics ride on top — one rail per singer, big
# type. Repeated-vowel runs retain readable world-space glyph widths even when
# that makes them several screens long: their attack stays score-locked, then
# the offscreen tail sustains the larger section at the shared scroll speed.
#
# Input is the JSON that pop/cult/viz/review-score.py --dump writes: tokens
# (word, singers, per-rail spans in shipped seconds), lanes (events or
# audio clips + envelope), the bar grid and the acts. Nothing here knows
# about the tempo warp or the release seams — that's the dump's job.
#
#   pop/.venv/bin/python pop/viz/lyric-reel.py pop/cult/out/wannadash-score.json
#       [--audio PATH] [--out PATH] [--palette violet|chalk]
#       [--still T[,T...]]   render single frames (PNG) instead of the video
#       [--preview]          15 fps
#       [--from T --to T]    render a review excerpt
#       [--sync-lag S]       delay score visuals against the release master
#       [--scroll-only]      one continuous lyric train; no editorial scenes
#       [--bare]             editorial cut: score data + full-frame lyrics,
#                            no title, labels, needle, or progress chrome
#       [--blur PX]          horizontal motion blur on the words as they fly
#                            (default 26 with --bare, 0 otherwise)
#       [--stamp SPECS]      extra sung tokens absent from the dump, e.g. the
#                            ident: "whistlegraph@0.05-1.05:jeffrey,org@..."
#                            (word@t0-t1[:rail], comma-separated)
#
# Safe zones (Instagram Reels UI): the top ~220 px carries the camera/audio
# chrome, the bottom ~420 px the caption and username, the right ~130 px
# the like/comment/share column. Lyrics stay inside x 60..950, y 260..1500;
# the flying tracks may go full-bleed because they are texture, not content.
import bisect, json, math, os, subprocess, sys, time
import numpy as np
from PIL import Image, ImageDraw, ImageFilter, ImageFont

def argval(flag, default=None):
    if flag in sys.argv:
        i = sys.argv.index(flag)
        if i + 1 < len(sys.argv):
            return sys.argv[i + 1]
    return default

SCORE = next((a for a in sys.argv[1:] if a.endswith(".json")), None)
if not SCORE:
    sys.exit("usage: lyric-reel.py <score.json> [--audio PATH] [--out PATH]")
S = json.load(open(SCORE))
LANE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(SCORE)))
AUDIO = argval("--audio", S.get("audio"))
OUT = argval("--out", os.path.join(LANE_DIR, "out", f"{S['title']}-lyric-reel.mp4"))
PALETTE = argval("--palette", "violet")
STILLS = [float(x) for x in argval("--still", "").split(",") if x]
FPS = 15 if "--preview" in sys.argv else 30
BARE = "--bare" in sys.argv
SCROLL_ONLY = "--scroll-only" in sys.argv
TUNNEL = "--tunnel" in sys.argv
BLUR = float(argval("--blur", "14" if BARE else "0"))
W, H = 1080, 1920
DUR = float(S["dur"])
START = max(0.0, float(argval("--from", "0")))
END = min(DUR, float(argval("--to", str(DUR))))
if END <= START:
    sys.exit("--to must be greater than --from")
# The release edit overlaps adjacent source regions with a 240 ms
# constant-power crossfade. The score dump is on the pre-overlap clock, so its
# envelopes and lyric attacks lead the shipped master by exactly that window.
# Keep one explicit master→score calibration and apply it to the whole visual
# composition; fixing only the lyric layer would pull cuts and track data apart.
SYNC_LAG = float(argval("--sync-lag", str(S.get("sync_lag", 0.24))))
INTRO_SPLICE = 4.05

def score_time(master_t):
    """Map shipped-master time to the score clock.
    The opening edit is sample-aligned. At its splice the body begins on the
    score clock 240 ms early, so hold the visual clock across that window and
    then continue with the measured body lag—never jump the picture backward."""
    if master_t <= INTRO_SPLICE:
        return master_t
    return max(INTRO_SPLICE, master_t - SYNC_LAG)

def master_time(score_t):
    return score_t if score_t <= INTRO_SPLICE else score_t + SYNC_LAG

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
FONT_B = f"{REPO}/slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf"
FONT_R = f"{REPO}/slab/menuband/Sources/MenuBand/Resources/ywft-processing-regular.ttf"
if not os.path.exists(FONT_B):
    REPO = os.getcwd()          # scratch/QA copy run from the repository root
    FONT_B = f"{REPO}/slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf"
    FONT_R = f"{REPO}/slab/menuband/Sources/MenuBand/Resources/ywft-processing-regular.ttf"

# ---------------------------------------------------------------- palette
# The cover is blacklight powder on near-black: electric blue, neon pink,
# lime, and orange. Keep that dark-room contrast in the reel; scene changes
# rotate through the cover's full powder range instead of one mid-violet wash.
PALETTES = {
    "violet": {
        "ground": (27, 18, 76),
        "ground2": (15, 9, 48),            # lane stripes
        "ink": (255, 255, 255),
        "dim": (181, 176, 236),            # upcoming / spent words
        "grid": (255, 255, 255),
        "who": {"camille": (255, 65, 165), "alex": (112, 255, 138),
                "jeffrey": (255, 126, 66), "None": (126, 120, 255)},
        "lane_mix": 0.42,                  # how much of a lane's own color shows
        "block_alpha": 0.62,
        "wave_alpha": 0.80,
    },
    "chalk": {
        "ground": (56, 118, 92),
        "ground2": (46, 104, 80),
        "ink": (250, 248, 240),
        "dim": (170, 200, 185),
        "grid": (250, 248, 240),
        "who": {"camille": (255, 140, 200), "alex": (210, 255, 150),
                "jeffrey": (255, 190, 100), "None": (250, 248, 240)},
        "lane_mix": 0.30,
        "block_alpha": 0.42,
        "wave_alpha": 0.55,
    },
}
P = PALETTES[PALETTE]
GROUND, INK, DIM = P["ground"], P["ink"], P["dim"]
WHO = {k: tuple(v) for k, v in P["who"].items()}
GLYPH_ACCENTS = (
    (255, 65, 165),   # powder pink
    (112, 255, 138),  # phosphor green
    (126, 120, 255),  # ultraviolet blue
    (255, 126, 66),   # orange ember
    (255, 215, 90),   # signal yellow
)

def blend(c0, c1, u):
    return tuple(int(round(a + (b - a) * u)) for a, b in zip(c0, c1))

# ---------------------------------------------------------------- layout
SAFE_X0, SAFE_X1, SAFE_Y0, SAFE_Y1 = 60, 950, 260, 1500
NOW_X = 600                     # the needle; sung letters trail left of it
RAILS = ["camille", "alex", "jeffrey", "None"]
RAIL_LABEL = {"camille": "CAMILLE", "alex": "ALEX", "jeffrey": "JEFFREY", "None": "ALL"}
RAIL_Y = {"camille": 560, "alex": 800, "jeffrey": 1040, "None": 1280}
RAIL_SIZE = 124                 # the type; smaller sizes only to dodge collisions
if BARE:
    # jeffrey is the MAIN voice: his line carries the record down the middle
    # and the chorus tokens (no single singer) fold into it; camille and
    # alex answer above and below in smaller type. The needle sits left so
    # the future train has room to recede toward the right-edge horizon.
    RAILS = ["camille", "jeffrey", "alex"]
    # condensed: ALL utterances share one line. jeffrey is the baseline;
    # camille rides SUPERscript, alex SUBscript — each word's offset is
    # fitted dynamically from whether it actually overlaps his line in
    # time (full clear when clashing, tucked close when alone).
    RAIL_Y = {"camille": 960, "jeffrey": 960, "alex": 960}
    # Full-screen lyric bodies. Lead glyphs occupy nearly half the frame's
    # width in height; the two answering voices remain large enough to own
    # their thirds of the composition.
    RSIZE = {"camille": 320, "jeffrey": 460, "alex": 320}
    NOW_X = W // 2              # the now-point is the CENTER: largest there
else:
    RSIZE = {r: RAIL_SIZE for r in RAILS}
SIZES = (124, 104, 88, 72, 56)
FONTS = {s: ImageFont.truetype(FONT_B, s) for s in set(SIZES) | set(RSIZE.values())}
f_title = ImageFont.truetype(FONT_B, 68)
f_sub = ImageFont.truetype(FONT_R, 30)
f_rail = ImageFont.truetype(FONT_B, 26)
_meas = ImageDraw.Draw(Image.new("RGB", (8, 8)))

# One repeated vowel per STRETCH_RATE seconds of hold (the house rule from
# review-score.py). The scroll rate follows from the font: a vowel's width
# per 135 ms, so stretched length and sung length are the same length.
STRETCH_RATE = 0.135
STRETCH_BASE = 0.35
STRETCH_CAP = 14
VOWEL_W = _meas.textlength("a", font=FONTS[RAIL_SIZE])
PPS = VOWEL_W / STRETCH_RATE * (1.65 if BARE else 1.0)
LOOK = (W - NOW_X) / PPS        # seconds of lyric visible ahead of the needle
FADE = 0.55                     # seconds a finished word lingers, fading
CHAR_TRAIL = 0.12               # fresh ember strokes after each spoken glyph
WORD_COOL = 0.70                # complete lit word cools as one readable body
FLASH_DECAY = 0.16              # longer white-to-color decay; no repeated strobe
CULT_HOLD = 0.25                # the long CULT stays fully readable after release
CULT_COOL = 1.65                # then leaves an unusually long ember wake
CULT_RELEASE_PPS = 300.0        # screen-space wake speed, independent of word length
LONG_GLYPH_SCALE = 0.78         # never crush a long run below readable character width
MAX_WORD_W = W - 120            # ordinary complete spellings must fit the viewport
OFFSCREEN_WORDS = {"cult"}      # deliberate multi-screen lyric bodies
MIN_HORIZONTAL_SCALE = 0.72     # protect glyph strokes from unreadable squeezing
MIN_WORD_SIZE = 160             # still 16 px tall in the 10% review image

VOWSET = set("aeiou")
FUNC_WORDS = {"the", "of", "i", "it", "a"}
NO_STRETCH = {"dot"}

def vowel_groups(w):
    gs, i = [], 0
    while i < len(w):
        if w[i] in VOWSET:
            j = i
            while j < len(w) and (w[j] in VOWSET or (w[j] == "y" and j > i)):
                j += 1
            gs.append((i, j)); i = j
        else:
            i += 1
    return gs

def stretch_part(part, extra):
    low = part.lower(); gs = vowel_groups(low)
    if len(gs) > 1:
        gs = [g for g in gs if not (g[1] - g[0] == 1 and g[0] == len(low) - 1 and low[g[0]] == "e")]
    if not gs or extra <= 0:
        return part
    a, b = max(gs, key=lambda g: g[1] - g[0])
    letters = [k for k in range(a, b) if low[k] in VOWSET]
    reps = {k: 0 for k in letters}
    for n in range(extra):
        reps[letters[n % len(letters)]] += 1
    return "".join(ch * (1 + reps.get(k, 0)) for k, ch in enumerate(part))

def longest_repeat(text):
    best = run = 0
    prior = None
    for c in text.lower():
        run = run + 1 if c == prior else 1
        best = max(best, run)
        prior = c
    return best

def font_for_size(size):
    size = max(MIN_WORD_SIZE, int(round(size / 2)) * 2)
    if size not in FONTS:
        FONTS[size] = ImageFont.truetype(FONT_B, size)
    return size, FONTS[size]

def fitted_word_font(text, base_size, target, offscreen=False):
    if offscreen:
        return font_for_size(base_size)
    natural = max(1.0, _meas.textlength(text, font=FONTS[base_size]))
    squeeze = target / natural
    if squeeze >= MIN_HORIZONTAL_SCALE:
        return base_size, FONTS[base_size]
    size = max(MIN_WORD_SIZE,
               int(base_size * squeeze / MIN_HORIZONTAL_SCALE))
    return font_for_size(size)

def stretch_word(word, extra):
    if word in NO_STRETCH or extra <= 0:
        return word
    parts = word.split(" ")
    idx = [i for i, p in enumerate(parts) if p.lower() not in FUNC_WORDS] or list(range(len(parts)))
    n = len(idx)
    for j, pi in enumerate(idx):
        parts[pi] = stretch_part(parts[pi], extra // n + (1 if j < extra % n else 0))
    return " ".join(parts)

# ---------------------------------------------------------------- words
# Each rail is one line. A word's target length is its hold at PPS; vowels
# fill that length. If the next word on the rail arrives before this one
# would end, the stretch shortens first, then the type steps down a size.
for spec in (argval("--stamp") or "").split(","):
    if not spec.strip():
        continue
    txt, rest = spec.rsplit("@", 1)
    tspan, _, srail = rest.partition(":")
    a, b = (float(x) for x in tspan.split("-"))
    S["tokens"].append({"word": txt.strip(), "rails": {srail or "None": {"t0": a, "t1": b}}})

rail_words = {r: [] for r in RAILS}
# The token summary unions overlapping copies of “run real fast” into one
# long span. The vocal-note lane retains the real, separately triggered
# performances; use those so two heard phrases become two scrolling phrases
# instead of one synthetic word stretched across both.
VOCAL_PHRASE_EVENTS = [
    e for lane in S.get("lanes", []) if lane.get("name") == "vocal notes"
    for e in lane.get("events", []) if e.get("word", "").lower() == "run real fast"
]
VOCAL_CULT_EVENTS = [
    e for lane in S.get("lanes", []) if lane.get("name") == "vocal notes"
    for e in lane.get("events", []) if e.get("word", "").lower() == "cult"
]
DASH_VOICE_EVENTS = [
    e for lane in S.get("lanes", []) if lane.get("name") == "dash voices"
    for e in lane.get("events", []) if e.get("word", "").lower() == "dash"
]
VOCAL_DASH_EVENTS = [
    e for lane in S.get("lanes", []) if lane.get("name") == "vocal notes"
    for e in lane.get("events", []) if e.get("word", "").lower() == "dash"
]

def event_rail(e):
    return e.get("who") or "None"

def union_span(events, fallback):
    if not events:
        return dict(fallback)
    return {"t0": min(float(e["t0"]) for e in events),
            "t1": max(float(e["t1"]) for e in events)}

def authoritative_dash_span(source_rail, span):
    """Recover the audible DASH body hidden by the token summary."""
    t0 = float(span["t0"])
    voices = [e for e in DASH_VOICE_EVENTS
              if event_rail(e) == source_rail
              and t0 - 0.85 <= float(e["t0"]) <= t0 + 0.05
              and float(e["t1"]) >= t0 - 0.05]
    body = union_span(voices, span)
    lead = [e for e in VOCAL_DASH_EVENTS
            if event_rail(e) == source_rail
            and float(e["t0"]) <= body["t1"]
            and float(e["t1"]) >= body["t0"] - 0.45]
    return union_span(voices + lead, body)

SPLIT_EXTRA = 0
for tk in S["tokens"]:
    for source_rail, span in tk["rails"].items():
        rail = "jeffrey" if BARE and source_rail == "None" else source_rail
        if rail in rail_words:
            spans = [span]
            if BARE and source_rail == "None" and tk["word"].lower() == "run real fast":
                matches = [e for e in VOCAL_PHRASE_EVENTS
                           if e["t0"] >= span["t0"] - 0.001
                           and e["t1"] <= span["t1"] + 0.001]
                if matches:
                    spans = matches
                    SPLIT_EXTRA += len(matches) - 1
            elif BARE and tk["word"].lower() == "cult":
                matches = [e for e in VOCAL_CULT_EVENTS
                           if event_rail(e) == source_rail
                           and e["t0"] <= span["t1"] + 0.001
                           and e["t1"] >= span["t0"] - 0.001]
                spans = [union_span(matches, span)]
            elif BARE and tk["word"].lower() == "dash":
                spans = [authoritative_dash_span(source_rail, span)]
            for actual in spans:
                if tk["word"].lower() == "run real fast":
                    disp = "run real faaaaast"
                elif tk["word"].lower() == "cult":
                    disp = "c" + "u" * 18 + "lt"
                elif tk["word"].lower() == "dash":
                    dash_dur = max(0.0, actual["t1"] - actual["t0"])
                    extra = max(0, min(STRETCH_CAP,
                                      round((dash_dur - STRETCH_BASE) / STRETCH_RATE)))
                    disp = stretch_word("dash", extra)
                else:
                    disp = tk.get("disp", tk["word"])
                rail_words[rail].append({"word": tk["word"], "t0": actual["t0"], "t1": actual["t1"],
                                         "disp": disp,
                                         "source_rail": source_rail,
                                         "color": WHO.get(source_rail, WHO[rail])})
for rail in RAILS:
    ws = sorted(rail_words[rail], key=lambda w: (w["t0"], w["t1"], w["word"]))
    # A singer can have multiple simultaneous phrases (and the ALL/chorus
    # material is folded into Jeffrey's visual register). Those are parallel
    # strands, not consecutive words. Greedy interval colouring gives every
    # token a real next-word neighbour without flattening overlaps into a
    # false sequence that truncates or hides one of them.
    strand_ends, strands = [], []
    for w in ws:
        strand = next((i for i, end in enumerate(strand_ends) if end <= w["t0"] + 1e-6), None)
        if strand is None:
            strand = len(strand_ends)
            strand_ends.append(-1.0)
            strands.append([])
        w["strand"] = strand
        strand_step = 190 if BARE else 82
        w["strand_y"] = 0 if strand == 0 else ((strand + 1) // 2) * (strand_step if strand % 2 else -strand_step)
        strand_ends[strand] = max(strand_ends[strand], w["t1"])
        strands[strand].append(w)
    for strand in strands:
        for a, b in zip(strand, strand[1:]):
            a["next_t0"] = b["t0"]
            a["next_word"] = b
    for w in ws:
        dur = max(1e-3, w["t1"] - w["t0"])
        gap = w.get("next_t0", w["t0"] + 99.0) - w["t0"]
        target = dur * PPS
        room = max(0.0, gap * PPS)
        if BARE:
            # The score dump already owns the lyric spelling. Its `disp`
            # contains the audited vowel repetitions (including holds across
            # every word in phrases such as "ruuuun reeeaaal faaaaast").
            # Never regenerate that string here: doing so hid the terminal
            # consonants until phrase end and disconnected the lyric.
            is_long = w["word"].lower() in OFFSCREEN_WORDS
            tgt = target if is_long else min(target, MAX_WORD_W)
            render_size, font = fitted_word_font(w["disp"], RSIZE[rail], tgt, is_long)
            w["render_size"] = render_size
            offs, xo = [], 0.0
            if w["word"].lower() == "run real fast":
                # The source phrase is three notes: RUN 320 ms, REAL 300 ms,
                # FAST 1700 ms. Preserve that ratio at every playback stretch.
                # Only FAST owns the long vowel; spreading the hold across all
                # three words was the visible mistiming the ear caught.
                for part, weight in zip(("run ", "real ", "faaaaast"), (0.32, 0.30, 1.70)):
                    widths = [_meas.textlength(c, font=font) for c in part]
                    budget = tgt * weight / 2.32
                    scale = budget / max(1.0, sum(widths))
                    for c, width in zip(part, widths):
                        wp = width * scale
                        offs.append((c, xo, wp)); xo += wp
            else:
                chs = list(w["disp"])
                bws = [_meas.textlength(c, font=font) for c in chs]
                natural = max(1.0, sum(bws))
                scale = tgt / natural
                if is_long:
                    # Long vowel runs live in world space and may span several
                    # screens without squeezing glyphs into the viewport.
                    scale = max(scale, LONG_GLYPH_SCALE)
                for k, c in enumerate(chs):
                    wp = bws[k] * scale
                    offs.append((c, xo, wp)); xo += wp
            w["chars"], w["px"], w["dur"] = offs, xo, dur
            w["long"] = is_long
            w["pps"] = PPS if w["long"] else xo / dur
            w["visual_dur"] = xo / w["pps"]
            continue
        w["size"], w["disp"], w["px"] = SIZES[-1], w["word"], 0.0
        for size in SIZES:
            font = FONTS[size]
            base = _meas.textlength(w["word"], font=font)
            vw = _meas.textlength("a", font=font)
            extra = int(max(0, min(STRETCH_CAP, round((target - base) / vw))))
            disp = stretch_word(w["word"], extra)
            px = _meas.textlength(disp, font=font)
            while extra > 0 and px > room:           # shorten the hold before shrinking
                extra -= 1
                disp = stretch_word(w["word"], extra)
                px = _meas.textlength(disp, font=font)
            if px <= room or size == SIZES[-1]:
                w["size"], w["disp"], w["px"] = size, disp, px
                break
    rail_words[rail] = ws
if BARE:
    # dynamic y fitting: super/subscript offsets from time overlap
    jiv = [(w["t0"], w["t0"] + w["px"] / w["pps"]) for w in rail_words["jeffrey"]]
    for rail, sgn in (("camille", -1), ("alex", 1)):
        for w in rail_words[rail]:
            a, b = w["t0"], w["t0"] + w["px"] / w["pps"]
            clash = any(a < j1 + 0.1 and j0 - 0.1 < b for (j0, j1) in jiv)
            w["yoff"] = w["strand_y"] + sgn * (360 if clash else 130)
    for w in rail_words["jeffrey"]:
        w["yoff"] = w["strand_y"]

def lexical_skeleton(text):
    out = []
    for c in text.lower():
        if c in VOWSET and out and out[-1] == c:
            continue
        out.append(c)
    return "".join(out)

def police():
    errors = []
    valid_lanes = 0
    for lane in S.get("lanes", []):
        lname = lane.get("name", "unnamed")
        if lane.get("kind") == "au":
            hz = float(lane.get("env_hz", 0))
            env = lane.get("env") or []
            good = hz > 0 and len(env) / hz >= DUR - 1 / hz
        else:
            evs = lane.get("events") or []
            good = all(0 <= float(e["t0"]) <= float(e["t1"]) <= DUR + 0.05
                       for e in evs)
            good = good and all(float(a["t0"]) <= float(b["t0"])
                                for a, b in zip(evs, evs[1:]))
        if good:
            valid_lanes += 1
        else:
            errors.append(f"data lane {lname}: incomplete or invalid timing")
    glyph_buses = {"bed 250+", "bass <250", "kick <150", "perc+skids", "signal"}
    missing_glyph_buses = glyph_buses - set(DATA_BUSES)
    if missing_glyph_buses:
        errors.append("glyph buses missing: " + ", ".join(sorted(missing_glyph_buses)))
    words = [w for rail in RAILS for w in rail_words[rail]]
    expected = (sum(1 for tk in S["tokens"] for r in tk["rails"]
                    if ("jeffrey" if BARE and r == "None" else r) in rail_words)
                + SPLIT_EXTRA)
    if len(words) != expected:
        errors.append(f"coverage: {len(words)} rendered spans != {expected} score spans")
    for rail in RAILS:
        by_strand = {}
        for w in rail_words[rail]:
            by_strand.setdefault(w["strand"], []).append(w)
            if lexical_skeleton(w["disp"]) != lexical_skeleton(w["word"]):
                errors.append(f"skeleton {rail} {w['t0']:.3f}: {w['disp']} != {w['word']}")
            if (not w["chars"] or w["px"] <= 0
                    or abs(w["dur"] - (w["t1"] - w["t0"])) > 1e-6
                    or w.get("pps", 0) <= 0
                    or abs(w["px"] / w["pps"] - w.get("visual_dur", 0)) > 0.5 / FPS
                    or (not w.get("long")
                        and abs(w["visual_dur"] - (w["t1"] - w["t0"])) > 0.5 / FPS)
                    or (w.get("long") and w["visual_dur"] + 0.5 / FPS < w["dur"])):
                errors.append(f"geometry {rail} {w['t0']:.3f} {w['word']}")
            if w.get("long") and w["px"] <= W:
                errors.append(f"offscreen long word collapsed {rail} {w['t0']:.3f} {w['word']}")
            if not w.get("long") and w["px"] > MAX_WORD_W + 1:
                errors.append(f"viewport word overflow {rail} {w['t0']:.3f} {w['word']}")
        for strand, seq in by_strand.items():
            seq.sort(key=lambda w: w["t0"])
            for a, b in zip(seq, seq[1:]):
                if a["t1"] > b["t0"] + 0.035:
                    errors.append(f"strand overlap {rail}:{strand} {a['word']}→{b['word']}")
                if a.get("next_word") is not b:
                    errors.append(f"missing link {rail}:{strand} {a['word']}→{b['word']}")
    score_start, score_end = score_time(START), score_time(END)
    selected = sorted((w for w in words if w["t1"] >= score_start and w["t0"] <= score_end),
                      key=lambda w: (w["t0"], w["source_rail"], w["word"]))
    selected_dots = [e for e in DOT_EVENTS
                     if e["t1"] >= score_start and e["t0"] <= score_end]
    selected_long = [w for w in selected if w.get("long")]
    selected_fit = [w for w in selected if not w.get("long") and w["px"] <= MAX_WORD_W + 1]
    starts = [w["t0"] for w in words]
    if not SCROLL_ONLY:
        for cut, view in SCENES:
            master_cut = master_time(cut)
            if START < master_cut < END and cut >= 90.0 and min(abs(cut - s) for s in starts) > 1 / FPS:
                errors.append(f"cut off attack {cut:.3f} {view}")
    print(f"POLICEMAN {'FAIL' if errors else 'PASS'} · {len(words)}/{expected} spans · "
          f"{sum(1 for w in words if w.get('next_word'))} links · {len(selected)} excerpt spans · "
          f"{len(selected_dots)} actual DOT events · {valid_lanes}/{len(S.get('lanes', []))} data lanes · "
          f"{len(glyph_buses) - len(missing_glyph_buses)}/{len(glyph_buses)} glyph buses · "
          f"{len(selected_fit)}/{len(selected) - len(selected_long)} viewport words · "
          f"{len(selected_long)} multi-screen exceptions · "
          f"sync 0.000/{SYNC_LAG:.3f}s · "
          f"{'scroll only' if SCROLL_ONLY else 'scenes'}")
    for w in selected:
        print(f"  {w['t0']:7.3f}–{w['t1']:7.3f}  {w['source_rail']:8s}  "
              f"s{w['strand']}  {w['disp']}")
    for e in errors:
        print("  FAIL " + e)
    return not errors

print("rails: " + ", ".join(f"{RAIL_LABEL[r].lower()}={len(rail_words[r])}" for r in RAILS)
      + f"  ·  scroll {PPS:.0f} px/s, {LOOK:.2f} s ahead", flush=True)

# ---------------------------------------------------------------- tracks
# The strip: every lane as a horizontal band across the full height, blocks
# and envelopes in the lane's color mixed toward the ground. Pre-rendered
# once at PPS, then sliced per frame — the lyrics scroll at the same rate,
# so a vocal-notes block and its word share an x.
print("rendering tracks...", flush=True)
LANES = S["lanes"]
NL = len(LANES)
LANE_H = H / NL
STRIP_W = int(math.ceil(DUR * PPS)) + W
def sx(t):
    return NOW_X + int(round(t * PPS))

def render_strip(lit):
    """The tracks at rest (lit=False) or lit as they play (lit=True): the
    same geometry, the second at full lane color so a per-column blend can
    light every block and waveform as the needle crosses it."""
    strip = Image.new("RGB", (STRIP_W, H), GROUND)
    sd = ImageDraw.Draw(strip)
    for li in range(NL):
        if li % 2:
            sd.rectangle([0, int(li * LANE_H), STRIP_W, int((li + 1) * LANE_H)], fill=P["ground2"])
    gcol = blend(GROUND, P["grid"], 0.10)
    gcol_h = blend(GROUND, P["grid"], 0.18)
    for bb in S["bars"]:
        x = sx(bb["t"])
        sd.line([x, 0, x, H], fill=gcol_h if bb["bar"] % 4 == 0 else gcol, width=2)
    for li, lane in enumerate(LANES):
        col = blend(tuple(lane["color"]), INK, 0.30)      # lane color, lifted toward the ink
        y0, y1 = int(li * LANE_H) + 6, int((li + 1) * LANE_H) - 6
        if lane["kind"] == "ev":
            evs = lane["events"]
            midis = [e["midi"] for e in evs if e.get("midi") is not None]
            lo, hi = (min(midis), max(midis)) if midis else (60, 60)
            bh = max(10, min(22, int(LANE_H * 0.16)))
            fill = blend(col, INK, 0.45) if lit else blend(GROUND, col, P["block_alpha"])
            for e in evs:
                m = e.get("midi") if e.get("midi") is not None else (lo + hi) / 2
                u = 0.5 if hi == lo else (m - lo) / (hi - lo)
                yc = y1 - bh / 2 - u * (y1 - y0 - bh)
                xa, xb = sx(e["t0"]), max(sx(e["t0"]) + 8, sx(e["t1"]))
                sd.rounded_rectangle([xa, yc - bh / 2, xb, yc + bh / 2], radius=bh // 2, fill=fill)
        else:
            env = np.array(lane["env"], dtype=np.float32) ** 0.8
            hz = lane["env_hz"]
            fill = blend(GROUND, col, 0.55) if lit else blend(GROUND, col, P["block_alpha"] * 0.55)
            wave = blend(col, INK, 0.55) if lit else blend(GROUND, col, P["wave_alpha"])
            mid = (y0 + y1) / 2
            amax = (y1 - y0) / 2 - 4
            for a, b in lane["clips"]:
                xa, xb = sx(a), sx(b)
                sd.rounded_rectangle([xa, y0, xb, y1], radius=10, fill=fill)
                for x in range(xa + 2, xb - 1, 2):
                    ci = int((x - NOW_X) / PPS * hz)
                    if 0 <= ci < len(env):
                        hgt = env[ci] * amax
                        if hgt > 1:
                            sd.line([x, mid - hgt, x, mid + hgt], fill=wave, width=2)
    return np.array(strip)

if not BARE:
    strip_np = render_strip(False)
    strip_lit = render_strip(True)
# per-column light: full at the needle, decaying leftward, off to the right
_xs = np.arange(W, dtype=np.float32)
LIGHT = np.where(_xs <= NOW_X, 0.18 + 0.82 * np.exp(-(NOW_X - _xs) / 260.0), 0.0)
LIGHT = LIGHT[None, :, None].astype(np.float32)

# ---------------------------------------------------------------- chrome
# Static overlay: title, rail labels, the needle. Composited over the
# scrolling strip each frame, under the words.
chrome = Image.new("RGBA", (W, H), (0, 0, 0, 0))
cd = ImageDraw.Draw(chrome)
# a soft veil across the safe box so the words read over busy tracks
veil = Image.new("RGBA", (W, H), (0, 0, 0, 0))
vd = ImageDraw.Draw(veil)
vd.rounded_rectangle([SAFE_X0 - 30, SAFE_Y0 - 20, W - 20, SAFE_Y1 + 20], radius=36,
                     fill=GROUND + (96,))
if not BARE:
    cd.text((SAFE_X0, SAFE_Y0 + 6), S["title"], font=f_title, fill=INK)
    cd.text((SAFE_X0 + 4, SAFE_Y0 + 84), S["artist"], font=f_sub, fill=blend(GROUND, INK, 0.72))
    for rail in RAILS:
        yc = RAIL_Y[rail]
        col = WHO[rail]
        cd.text((SAFE_X0, yc - RAIL_SIZE // 2 - 20), RAIL_LABEL[rail], font=f_rail,
                fill=blend(GROUND, col, 0.85), anchor="ls")
        cd.line([SAFE_X0, yc + RAIL_SIZE // 2 + 8, W - 20, yc + RAIL_SIZE // 2 + 8],
                fill=blend(GROUND, col, 0.22), width=2)
    cd.line([NOW_X, SAFE_Y0 + 140, NOW_X, SAFE_Y1 - 40], fill=INK + (110,), width=3)

# ---------------------------------------------------------------- frames
def draw_words(img, t):
    # The needle is the lit edge, by construction: a word lights exactly as
    # it crosses NOW_X, whatever its pixel length. Stretching makes that
    # length equal the hold for sung words; a word wider than its hold just
    # keeps crossing, and it is spent once its tail has passed.
    dd = ImageDraw.Draw(img)
    for rail in RAILS:
        yc = RAIL_Y[rail]
        col = WHO[rail]
        for w in rail_words[rail]:
            t0 = w["t0"]
            x = NOW_X + (t0 - t) * PPS
            if x > W:
                break
            xe = x + w["px"]
            t_end = t0 + w["px"] / PPS          # when the tail clears the needle
            if t > t_end + FADE or xe < -10:
                continue
            font = FONTS[w["size"]]
            if x >= NOW_X:                      # approaching: dim, brightening
                u = max(0.0, 1.0 - (t0 - t) / LOOK)
                k = 0.28 + 0.30 * u
                dd.text((x, yc), w["disp"], font=font, fill=blend(GROUND, DIM, k), anchor="lm")
                continue
            if xe <= NOW_X:                     # spent: fade the whole word
                k = 0.85 * (1.0 - min(1.0, (t - t_end) / FADE))
                dd.text((x, yc), w["disp"], font=font, fill=blend(GROUND, col, k), anchor="lm")
                continue
            # active: dim body, lit head exactly up to the needle
            lit = int(round(NOW_X - x))
            dd.text((x, yc), w["disp"], font=font, fill=blend(GROUND, DIM, 0.55), anchor="lm")
            if lit > 0:
                tw = int(w["px"]) + 8
                th = w["size"] + 24
                tile = Image.new("RGBA", (tw, th), (0, 0, 0, 0))
                td = ImageDraw.Draw(tile)
                td.text((0, th // 2), w["disp"], font=font, fill=col + (255,), anchor="lm")
                tile = tile.crop((0, 0, min(tw, lit), th))
                img.paste(tile, (int(round(x)), int(round(yc - th / 2))), tile)
            uy = yc + w["size"] // 2 + 2
            dd.rounded_rectangle([x, uy, NOW_X, uy + 10], radius=5, fill=col)

# ---- bare mode: per-character tiles, scaled by speed and utterance ----
DIMC = blend(GROUND, DIM, 0.40)
PREC = blend(GROUND, DIM, 0.48) + (190,) # upcoming glyphs keep the whole word legible
PRE_S = 0.56                    # active glyph still makes a decisive size jump

# Material enters from the right, crosses the now-point, and leaves the left
# edge completely; long words remain intact in world space outside the crop.
def xwarp(x):
    return x
PIN_S = 0.085                   # scale floor for the next words on a rail

def lyric_x0(w, t):
    """World-space head position, including CULT's continuous release ease."""
    pps = w.get("pps", PPS)
    visual_end = w["t0"] + w.get("visual_dur", w.get("dur", w["t1"] - w["t0"]))
    if w["word"].lower() == "cult" and t > visual_end:
        return (NOW_X + (w["t0"] - visual_end) * pps
                - (t - visual_end) * CULT_RELEASE_PPS)
    return NOW_X + (w["t0"] - t) * pps

def lyric_x1(w, t):
    return lyric_x0(w, t) + w["px"]

# The music is a second spatial system, not a decorative screenshot. Every
# visible body below is sourced: note events become duration capsules, attacks
# become ticks, and audio buses become their measured 50 Hz envelopes. All
# lanes meet the lyrics at NOW_X on the same clock, but use a wider time lens:
# roughly three seconds of arrangement remain visible instead of the lyric
# train's one-second crop. This reads as a live visualizer, not a looping skin.
TRACK_PPS = 360.0
TRACK_PAST, TRACK_FUTURE = 3.0, 6.5
TRACK_Y = [(i + 0.55) * H / NL for i in range(NL)]
for lane in LANES:
    lane["_times"] = [float(e["t0"]) for e in lane.get("events", [])]
    midis = [float(e["midi"]) for e in lane.get("events", []) if e.get("midi") is not None]
    lane["_midi"] = (min(midis), max(midis)) if midis else (60.0, 60.0)
DATA_BUSES = {lane["name"]: lane for lane in LANES if lane.get("kind") == "au"}

def bus_value(name, t):
    """Measured envelope and its signed one-frame change at score time t."""
    lane = DATA_BUSES[name]
    env, hz = lane["env"], float(lane["env_hz"])
    i = min(len(env) - 1, max(0, int(t * hz)))
    j = max(0, i - 1)
    return float(env[i]), float(env[i]) - float(env[j])

def character_vibe(t, rail, ch, off):
    """Map real bus energy onto one glyph's motion.

    Signal/percussion stretch width; bass/kick stretch height; signed bass/bed
    changes sway vertically; percussion and signal changes shake horizontally.
    A character-derived sign only spatializes those measured values so adjacent
    glyphs do not move as one rigid word.
    """
    bed, dbed = bus_value("bed 250+", t)
    bass, dbass = bus_value("bass <250", t)
    kick, _ = bus_value("kick <150", t)
    perc, dperc = bus_value("perc+skids", t)
    signal, dsignal = bus_value("signal", t)
    seed = ord(ch) + int(off) + RAILS.index(rail) * 37
    sx = -1.0 if seed & 1 else 1.0
    sy = -1.0 if seed & 2 else 1.0
    is_u = ch.lower() == "u"
    elastic = 2.15 if is_u else 1.0
    spread = 0.72 + 0.56 * hash01(seed)
    cross_spread = 0.72 + 0.56 * hash01(seed + 911)
    dx = elastic * spread * (sx * (10.0 * perc + 58.0 * dperc) + sy * 42.0 * dsignal)
    dy = elastic * cross_spread * (-34.0 * kick + sy * (78.0 * dbass + 54.0 * dbed
                                                        + 18.0 * signal))
    wide = max(0.72 if is_u else 0.78, min(1.72 if is_u else 1.58,
               1.0 + elastic * spread * (0.20 * signal + 0.18 * dsignal
                                          + 0.16 * abs(dperc))))
    tall = max(0.74 if is_u else 0.80, min(1.76 if is_u else 1.60,
               1.0 + elastic * cross_spread * (0.18 * bass + 0.14 * kick
                                                + 0.20 * dbass + 0.05 * bed)))
    angle = elastic * sx * (5.0 * signal + 12.0 * dperc)
    levels = (signal, bass, bed, perc, kick)
    accent_i = seed % len(GLYPH_ACCENTS)
    tint = min(0.72, 0.16 + 0.58 * levels[accent_i]
               + 0.18 * abs(dsignal + dperc + dbass + dbed))
    return dx, dy, wide, tall, angle, GLYPH_ACCENTS[accent_i], tint

DOT_EVENTS = sorted(
    (dict(e) for lane in LANES if lane.get("name") == "vocal notes"
     for e in lane.get("events", []) if e.get("word", "").lower() == "dot"),
    key=lambda e: (e["t0"], e["t1"], e.get("who") or "", e.get("midi") or 0),
)
DOT_EVENT_TIMES = [e["t0"] for e in DOT_EVENTS]
INTRO_END = INTRO_SPLICE
INTRO_EVENTS = [
    (li, dict(e)) for li, lane in enumerate(LANES)
    for e in lane.get("events", []) if e["t0"] < INTRO_END
]

def hash01(i):
    x = (int(i) * 2654435761) & 0xffffffff
    x ^= x >> 15
    x = (x * 2246822519) & 0xffffffff
    x ^= x >> 13
    return x / 4294967296.0

SCENES = [
    (0.0, "impact"), (4.05, "race"),
    (20.05, "impact"), (22.05, "race"), (24.05, "impact"),
    (26.05, "race"), (28.05, "impact"), (30.05, "race"),
    (32.05, "impact"), (34.05, "race"), (36.048, "poly"),
    (59.927, "race"),
    (75.515, "impact"), (77.515, "race"), (79.515, "impact"),
    (81.515, "race"), (83.515, "impact"), (85.515, "race"),
    (87.515, "impact"), (89.515, "race"),
    # The 24-second reel starts at 90.0. From here every cut is attached to
    # an actual vocal attack—none lands mid-utterance or in a lyric gap.
    (90.85, "poly"), (92.35, "impact"), (92.75, "poly"),
    (94.23, "impact"), (94.64, "race"), (96.51, "impact"),
    (97.45, "poly"),
    (98.375, "field"),
]
SCENE_TIMES = [s[0] for s in SCENES]
def scene_at(t):
    i = max(0, bisect.bisect_right(SCENE_TIMES, t) - 1)
    return i, SCENES[i][1], SCENES[i][0]

def view_at(t):
    return scene_at(t)[1]

if "--police" in sys.argv:
    sys.exit(0 if police() else 2)

# Decode the shipped stereo master once. These samples feed the oscilloscope
# buffers behind the words; they are not an envelope, animation curve, or
# approximation of a stem. Scope time is always the final-mix/master clock.
MIX_RATE = 12000
print("decoding final-mix oscilloscope...", flush=True)
_mix_raw = subprocess.check_output([
    "ffmpeg", "-v", "error", "-i", AUDIO, "-vn", "-ac", "2",
    "-ar", str(MIX_RATE), "-f", "f32le", "-acodec", "pcm_f32le", "-",
])
MIX_PCM = np.frombuffer(_mix_raw, dtype=np.float32).reshape(-1, 2)

# Canonical CULT/whistlegraph mark, cropped from the released cover source.
# Keep the photographed chalk and floor texture instead of redrawing a logo.
CULT_MARK_PATH = f"{REPO}/pop/cult/cover/cover-a-glyph.jpg"
_cult_source = Image.open(CULT_MARK_PATH).convert("RGB")
_cult_crop = _cult_source.crop((650, 400, 2450, 2200))
CULT_MARK_CACHE = {}

def cult_mark_tile(size, alpha=255):
    size = max(48, int(round(size / 4)) * 4)
    tile = CULT_MARK_CACHE.get(size)
    if tile is None:
        photo = _cult_crop.resize((size, size), Image.LANCZOS).convert("RGBA")
        mask = Image.new("L", (size, size), 0)
        md = ImageDraw.Draw(mask)
        md.rounded_rectangle((2, 2, size - 3, size - 3), radius=max(10, size // 9), fill=255)
        photo.putalpha(mask)
        frame = Image.new("RGBA", (size, size), (0, 0, 0, 0))
        frame.alpha_composite(photo)
        fd = ImageDraw.Draw(frame, "RGBA")
        fd.rounded_rectangle((2, 2, size - 3, size - 3), radius=max(10, size // 9),
                             outline=INK + (190,), width=max(2, size // 70))
        tile = frame
        CULT_MARK_CACHE[size] = tile
    if alpha >= 255:
        return tile
    out = tile.copy()
    out.putalpha(out.getchannel("A").point(lambda a: (a * alpha) // 255))
    return out

def track_y(li, x):
    # A fixed lane baseline preserves pitch/amplitude meaning. No decorative
    # sine displacement is allowed to masquerade as musical data.
    return TRACK_Y[li]

NOTATION_FONT_CACHE = {}
def notation_font(size):
    size = int(size)
    if size not in NOTATION_FONT_CACHE:
        NOTATION_FONT_CACHE[size] = ImageFont.truetype(FONT_B, size)
    return NOTATION_FONT_CACHE[size]

SCOPE_SPECS = (
    # channel, seconds per buffer, fixed y, half-height, cover color, opacity
    (0, 0.090, 300, 230, GLYPH_ACCENTS[0], 70),
    (1, 0.090, 720, 250, GLYPH_ACCENTS[1], 70),
    (0, 0.360, 1130, 270, GLYPH_ACCENTS[2], 60),
    (1, 0.360, 1580, 260, GLYPH_ACCENTS[3], 60),
)

def draw_mix_oscilloscope(layer, master_t):
    """Four fixed-baseline PCM buffers from the final stereo mix.

    Each buffer ends at the current master sample. Pixel columns preserve the
    minimum and maximum samples in their slice, so fast transients remain real
    instead of aliasing into a decorative line. Only the buffer contents move;
    their Y positions never sway.
    """
    dd = ImageDraw.Draw(layer, "RGBA")
    bins, x0, x1 = 270, 20.0, W - 20.0
    end = max(0, min(len(MIX_PCM), int(round(master_t * MIX_RATE))))
    for scope_i, (channel, seconds, yc, half_h, col, opacity) in enumerate(SCOPE_SPECS):
        count = int(round(seconds * MIX_RATE))
        # Both chosen buffer sizes divide evenly into 270 exact pixel bins.
        buf = np.zeros(count, dtype=np.float32)
        source0 = max(0, end - count)
        available = end - source0
        if available:
            buf[-available:] = MIX_PCM[source0:end, channel]
        grouped = buf.reshape(bins, count // bins)
        lo = grouped.min(axis=1)
        hi = grouped.max(axis=1)
        # Fixed transfer curve exposes quiet recorded detail without normalizing
        # every frame to the same height; relative level remains truthful.
        lo = np.sign(lo) * np.abs(lo) ** 0.72
        hi = np.sign(hi) * np.abs(hi) ** 0.72
        xs = np.linspace(x0, x1, bins)
        top = [(float(x), yc - float(v) * half_h) for x, v in zip(xs, hi)]
        bottom = [(float(x), yc - float(v) * half_h) for x, v in zip(xs, lo)]
        dd.polygon(top + list(reversed(bottom)), fill=col + (opacity,))
        edge = blend(col, INK, 0.34)
        # Wide translucent rails and exact min/max edges turn each buffer into
        # a full-frame rollercoaster without inventing a path. Cross-tie color
        # is driven by the recorded peak in that sample column.
        dd.line(top, fill=col + (22,), width=11)
        dd.line(bottom, fill=col + (22,), width=11)
        dd.line(top, fill=edge + (min(210, opacity + 72),), width=2)
        dd.line(bottom, fill=edge + (min(210, opacity + 72),), width=2)
        accent = GLYPH_ACCENTS[(scope_i + 1) % len(GLYPH_ACCENTS)]
        for bi in range(0, bins, 9):
            peak = min(1.0, max(abs(float(lo[bi])), abs(float(hi[bi]))))
            tie_col = blend(col, accent, 0.18 + 0.60 * peak)
            dd.line((top[bi][0], top[bi][1], bottom[bi][0], bottom[bi][1]),
                    fill=tie_col + (int(34 + 82 * peak),), width=2)

# The single's actual artwork: the blacklight powder field. It rides
# bottom-center as the identity badge and bumps with the record.
WANNA_COVER_PATH = f"{REPO}/pop/cult/cover/wannadash-cover-gpt-image-2-field.jpg"
_wanna_cover = Image.open(WANNA_COVER_PATH).convert("RGB")
COVER_TILE_CACHE = {}

def cover_tile(size):
    size = max(48, int(round(size / 8)) * 8)
    tile = COVER_TILE_CACHE.get(size)
    if tile is None:
        photo = _wanna_cover.resize((size, size), Image.LANCZOS).convert("RGBA")
        mask = Image.new("L", (size, size), 0)
        ImageDraw.Draw(mask).rounded_rectangle(
            (2, 2, size - 3, size - 3), radius=max(10, size // 9), fill=255)
        photo.putalpha(mask)
        fd = ImageDraw.Draw(photo, "RGBA")
        fd.rounded_rectangle((2, 2, size - 3, size - 3), radius=max(10, size // 9),
                             outline=INK + (190,), width=max(2, size // 70))
        COVER_TILE_CACHE[size] = tile = photo
    return tile

def draw_opening_identity(layer, t, master_t):
    """The scored opening sweep flashes around the artwork badge; after that
    the badge stays bottom-center, scaling with the live kick, tinted by the
    current act, and flashing white on each measured hit."""
    kickL, _ = bus_value("kick <150", min(DUR - 0.05, t))
    bassL, _ = bus_value("bass <250", min(DUR - 0.05, t))
    glow = min(1.0, 0.7 * kickL + 0.4 * bassL)
    act_col = GROUND
    for a in ACTS:
        if t >= a["t0"]:
            act_col = tuple(a["color"])
        else:
            break
    cx, cy = W / 2, H - 330.0
    if master_t < 1.05:
        dd = ImageDraw.Draw(layer, "RGBA")
        heat = math.exp(-master_t / 0.16)
        dd.rectangle((0, 0, W, H), fill=(255, 244, 188, int(178 * heat)))
        travel = 1.0 - math.exp(-master_t * 4.4)
        for ri, col in enumerate((INK, GLYPH_ACCENTS[4], GLYPH_ACCENTS[0], GLYPH_ACCENTS[2])):
            radius = 46 + 840 * travel + ri * 46
            ring_alpha = int(220 * heat * (1.0 - ri * 0.13))
            if ring_alpha > 2:
                dd.ellipse((cx - radius, cy - radius, cx + radius, cy + radius),
                           outline=col + (ring_alpha,), width=max(2, 13 - ri * 2))
    size = 210 * (1.0 + 0.16 * kickL + 0.07 * glow)
    icon = cover_tile(size).copy()
    tint_a = int(34 + 66 * glow)
    ta = icon.getchannel("A").point(lambda a: min(a, tint_a))
    tint = Image.new("RGBA", icon.size, act_col + (0,))
    tint.putalpha(ta)
    icon.alpha_composite(tint)
    if kickL > 0.03:
        hit_a = int(90 * kickL)
        wa = icon.getchannel("A").point(lambda a: min(a, hit_a))
        white = Image.new("RGBA", icon.size, (255, 255, 255, 0))
        white.putalpha(wa)
        icon.alpha_composite(white)
    layer.alpha_composite(icon, (int(round(cx - icon.width / 2)),
                                 int(round(cy - icon.height / 2))))

def midi_name(midi):
    names = ("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
    n = int(round(float(midi)))
    return names[n % 12] + str(n // 12 - 1)

def draw_intro_notation(layer, t):
    """One mark per scored intro event—no decorative population.
    Duration is the beam, attack is the glyph, and pitch names come directly
    from MIDI. The whole notation uses the same linear clock as the lyrics."""
    dd = ImageDraw.Draw(layer, "RGBA")
    drum = {"kick": "K", "hat": "X", "perc": "+", "skid": "/",
            "sweep": "~", "revkick": "<"}
    signal = {"beep": "-", "click": "|", "bop": "O"}
    for k, (li, e) in enumerate(INTRO_EVENTS):
        raw0 = NOW_X + (e["t0"] - t) * TRACK_PPS
        raw1 = NOW_X + (e["t1"] - t) * TRACK_PPS
        if raw1 < -40 or raw0 > W + 40:
            continue
        lane = LANES[li]
        col = tuple(lane["color"])
        y = track_y(li, raw0)
        word = str(e.get("word") or "")
        lname = lane["name"].lower()
        if "drum" in lname:
            mark = drum.get(word, word[:1].upper())
        elif "signal" in lname:
            mark = signal.get(word, word.upper())
        elif e.get("midi") is not None:
            mark = midi_name(e["midi"])
        else:
            mark = word.upper()
        active = e["t0"] <= t <= e["t1"]
        passed = t > e["t1"]
        alpha = 250 if active else (84 if not passed else 130)
        size = 34 if active else 24
        beam_y = y + 22
        dd.line((raw0, beam_y, raw1, beam_y), fill=col + (max(36, alpha // 2),),
                width=7 if active else 3)
        dd.text((raw0, y), mark, font=notation_font(size), fill=col + (alpha,), anchor="mm")
    dd.line((NOW_X, 36, NOW_X, H - 36), fill=INK + (82,), width=2)

def draw_track_data(layer, t, view):
    dd = ImageDraw.Draw(layer, "RGBA")
    # Literal scored events remain as a sparse notation layer. The five stem
    # envelopes still drive character dynamics, but their former bottom-heavy
    # scrolling bands are removed; the final-mix PCM scope owns the frame.
    strength = {"race": 0.56, "impact": 0.46, "poly": 0.50, "field": 0.34}[view]
    if strength <= 0:
        return
    for li, lane in enumerate(LANES):
        col = tuple(lane["color"])
        if lane["kind"] == "au":
            continue

        times = lane["_times"]
        evs = lane["events"]
        k = max(0, bisect.bisect_left(times, t - TRACK_PAST) - 1)
        lo, hi = lane["_midi"]
        lname = lane["name"].lower()
        while k < len(evs):
            e = evs[k]
            if e["t0"] > t + TRACK_FUTURE:
                break
            if e["t1"] >= t - TRACK_PAST:
                raw0 = NOW_X + (e["t0"] - t) * TRACK_PPS
                raw1 = NOW_X + (e["t1"] - t) * TRACK_PPS
                xa, xb = xwarp(raw0), xwarp(raw1)
                d = max(0.07, min(1.0, (xb - xa) / max(1.0, raw1 - raw0)))
                midi = float(e["midi"]) if e.get("midi") is not None else (lo + hi) / 2
                pu = 0.5 if hi == lo else (midi - lo) / (hi - lo)
                yc = track_y(li, (xa + xb) / 2) + (0.5 - pu) * 66 * d
                alpha = int((58 + 74 * (1 - abs((xa + xb) / 2 - NOW_X) / W)) * strength)
                active = e["t0"] <= t <= e["t1"]
                age = t - e["t0"]
                attack = math.exp(-age / 0.095) if 0.0 <= age <= 0.55 else 0.0
                passed = t > e["t1"]
                draw_col = blend(col, INK, 0.14 + 0.78 * attack)
                alpha = int(alpha * (0.58 if passed else 0.82))
                alpha = min(255, alpha + int(188 * attack))
                # The flash belongs to the source attack only, then decays
                # once. Sustained events never blink on an arbitrary LFO.
                if attack > 0.015:
                    flare = 5 + 26 * attack
                    flare_alpha = int(30 + 165 * attack)
                    dd.line((xa, yc - flare, xa, yc + flare),
                            fill=draw_col + (flare_alpha,), width=max(2, int(5 * d)))
                    dd.line((xa - flare * 0.55, yc, xa + flare * 0.55, yc),
                            fill=draw_col + (flare_alpha,), width=2)
                tail_alpha = max(18, int(alpha * (0.72 if active else 0.46)))
                dd.line((xa, yc, max(xa + 3, xb), yc),
                        fill=draw_col + (tail_alpha,), width=max(1, int(3 * d)))
                word = str(e.get("word") or "")
                midi_label = midi_name(midi) if e.get("midi") is not None else ""
                if "drum" in lname or "objects" in lname or "signal" in lname:
                    # Literal event mark + exact duration tail.
                    drum_mark = {"kick": "K", "hat": "X", "perc": "+",
                                 "skid": "/", "sweep": "~", "revkick": "<"}
                    signal_mark = {"click": "|", "beep": "-", "bop": "O"}
                    mark = drum_mark.get(word.lower(), signal_mark.get(word.lower(), word.upper()))
                    dd.text((xa + 3, yc - 4), mark, font=notation_font(16),
                            fill=draw_col + (alpha,), anchor="ls")
                elif "vocal" in lname or "voices" in lname:
                    # The scored word is the body; pitch remains its Y value.
                    mark = word.upper()
                    dd.text((xa + 3, yc - 4), mark, font=notation_font(14),
                            fill=draw_col + (alpha,), anchor="ls")
                else:
                    # Instrument name and exact pitch replace the generic pill.
                    mark = (word.upper() + (" " + midi_label if midi_label else "")).strip()
                    dd.text((xa + 3, yc - 4), mark, font=notation_font(13),
                            fill=draw_col + (alpha,), anchor="ls")
            k += 1

DOT_CLOUD_TILE_CACHE = {}
def dot_cloud_tile(size, col, alpha, angle):
    # Thousands of literal lyric particles are drawn during the rush. Cache a
    # small set of chalky word tiles so the cloud remains practical at 30 fps.
    size = max(12, int(round(size / 2)) * 2)
    alpha = max(16, min(255, int(round(alpha / 16)) * 16))
    angle = int(round(angle / 4)) * 4
    key = (size, col, alpha, angle)
    tile = DOT_CLOUD_TILE_CACHE.get(key)
    if tile is not None:
        return tile
    font = ImageFont.truetype(FONT_B, size)
    box = _meas.textbbox((0, 0), "DOT", font=font, stroke_width=1)
    tw, th = box[2] - box[0] + 10, box[3] - box[1] + 10
    tile = Image.new("RGBA", (tw, th), (0, 0, 0, 0))
    td = ImageDraw.Draw(tile, "RGBA")
    td.text((tw / 2 + 2, th / 2 + 2), "DOT", font=font,
            fill=(4, 2, 14, min(210, alpha)), anchor="mm")
    td.text((tw / 2, th / 2), "DOT", font=font, fill=col + (alpha,),
            stroke_width=1, stroke_fill=col + (max(8, alpha // 4),), anchor="mm")
    if angle:
        tile = tile.rotate(angle, resample=Image.BICUBIC, expand=True)
    DOT_CLOUD_TILE_CACHE[key] = tile
    return tile

def draw_dot_field(layer, t, view):
    # This is the piece, not an illustration of it: one visible DOT for every
    # DOT event in the vocal-note score. Time owns x, pitch/performer own y,
    # performer owns color, pitch owns rotation, and duration owns scale. The
    # real acceleration creates the cloud by itself; no random offsets or
    # generated particles are added.
    a = max(0, bisect.bisect_left(DOT_EVENT_TIMES, t - TRACK_PAST) - 1)
    b = bisect.bisect_right(DOT_EVENT_TIMES, t + TRACK_FUTURE)
    for i in range(a, b):
        e = DOT_EVENTS[i]
        raw = NOW_X + (e["t0"] - t) * TRACK_PPS
        x = xwarp(raw)
        if x < -50 or x > W + 50:
            continue
        who = e.get("who")
        rail = who if who in RAILS else "jeffrey"
        col = WHO.get(who, WHO["None"])
        midi = float(e["midi"]) if e.get("midi") is not None else 60.0
        pitch_y = (60.0 - midi) * 11.0
        y = ypath(rail, x) + pitch_y
        if y < 80 or y > H - 80:
            continue
        dur = max(0.01, e["t1"] - e["t0"])
        d = max(0.10, min(1.0, (xwarp(raw + 8) - x) / 8))
        size = (16 + min(30, dur * 110)) * (0.52 + 0.48 * d)
        if t < e["t0"]:
            alpha = int(52 + 82 * d)
        elif t <= e["t1"]:
            alpha = 245
        else:
            alpha = int(190 * max(0.0, 1.0 - (t - e["t1"]) / TRACK_PAST))
        if alpha <= 8:
            continue
        angle = max(-16.0, min(16.0, (midi - 60.0) * 1.6))
        word = dot_cloud_tile(size, col, alpha, angle)
        layer.paste(word, (int(x - word.width / 2), int(y - word.height / 2)), word)

# The rails are curves: each one crests at the centre — a word rises as
# its moment approaches, is highest and largest as it is sung, and slides
# away down the far side.
CURVE = {"jeffrey": 70, "camille": 70, "alex": 70}
def ypath(rail, x):
    u = max(-1.0, min(1.0, (x - NOW_X) / NOW_X))
    return RAIL_Y[rail] - CURVE.get(rail, 0) * math.cos(u * math.pi / 2)

# Dark-room grounds sampled from the cover's visual families. Each scene owns
# a different pair; the spatial wipe moves the new colour through the frame.
ACTS = S.get("acts") or []
COVER_BG = (
    (14, 18, 92),   # ultraviolet blue
    (72, 8, 76),    # powder magenta
    (92, 11, 50),   # hot pink / red
    (8, 61, 54),    # phosphor green
    (39, 19, 98),   # indigo
    (91, 34, 8),    # orange ember
    (8, 38, 76),    # deep cyan-blue
)
def bg_cols(t, scene_index=None):
    act_col = GROUND
    for a in ACTS:
        if t >= a["t0"]:
            act_col = tuple(a["color"])
        else:
            break
    if scene_index is None:
        scene_index = scene_at(t)[0]
    c0 = blend(COVER_BG[scene_index % len(COVER_BG)], act_col, 0.14)
    c1 = blend(COVER_BG[(scene_index + 2) % len(COVER_BG)], act_col, 0.09)
    breath = 0.06 * (0.5 + 0.5 * math.sin(t * 0.83))
    top = blend((3, 2, 18), c0, 0.82 + breath)
    bot = blend((2, 2, 13), c1, 0.61 + breath)
    return top, bot
BG_RAMP = np.linspace(0.0, 1.0, H, dtype=np.float32)[:, None, None]
def bg_frame(t, scene_index=None):
    top, bot = bg_cols(t, scene_index)
    a = np.array(top, dtype=np.float32)[None, None, :]
    b = np.array(bot, dtype=np.float32)[None, None, :]
    g = a * (1 - BG_RAMP) + b * BG_RAMP
    return np.ascontiguousarray(np.broadcast_to(g, (H, W, 3))).astype(np.uint8)

def scroll_bg_frame(t):
    """One continuously evolving dark field for the uninterrupted train.
    Palette movement is smooth, so colour never behaves like another cut."""
    phase = t / 3.8
    i = math.floor(phase)
    u = phase - i
    u = u * u * (3 - 2 * u)
    top_col = blend(COVER_BG[i % len(COVER_BG)],
                    COVER_BG[(i + 1) % len(COVER_BG)], u)
    bot_col = blend(COVER_BG[(i + 2) % len(COVER_BG)],
                    COVER_BG[(i + 3) % len(COVER_BG)], u)
    act_col = GROUND
    for act in ACTS:
        if t >= act["t0"]:
            act_col = tuple(act["color"])
        else:
            break
    breath = 0.04 * (0.5 + 0.5 * math.sin(t * 0.83))
    top = blend((3, 2, 18), blend(top_col, act_col, 0.11), 0.78 + breath)
    bot = blend((2, 2, 13), blend(bot_col, act_col, 0.07), 0.58 + breath)
    a = np.array(top, dtype=np.float32)[None, None, :]
    b = np.array(bot, dtype=np.float32)[None, None, :]
    g = a * (1 - BG_RAMP) + b * BG_RAMP
    return np.ascontiguousarray(np.broadcast_to(g, (H, W, 3))).astype(np.uint8)

# Only score-authored impact words can recolor the room. Their attack time,
# duration, performer color, and vertical register all come from the lyric
# event; the background simply makes that same hit spatial and ambient.
AMBIENT_WORDS = {"dash": 1.0, "cult": 0.92, "run real fast": 0.82}
AMBIENT_ROWS = np.arange(H, dtype=np.float32)[:, None, None]
def word_ambient_frame(bg, t):
    hits = []
    for rail in RAILS:
        for w in rail_words[rail]:
            age = t - w["t0"]
            if age < 0:
                break
            weight = AMBIENT_WORDS.get(w["word"].lower())
            if weight is None or age > min(0.72, w["dur"] + 0.16):
                continue
            attack = math.exp(-age / 0.105)
            body = 0.12 * max(0.0, 1.0 - age / max(0.01, w["dur"]))
            power = weight * (attack + body)
            hits.append((power, np.array(w.get("color", WHO[rail]), dtype=np.float32),
                         RAIL_Y[rail] + w.get("yoff", 0.0)))
    if not hits:
        return bg
    total = sum(p for p, _, _ in hits)
    color = sum((p * c for p, c, _ in hits), np.zeros(3, np.float32)) / total
    center = sum(p * y for p, _, y in hits) / total
    # The whole field changes, with the performing register carrying the
    # strongest light. Attack is a sharp flash; the word body is only a glow.
    band = 0.28 + 0.72 * np.exp(-((AMBIENT_ROWS - center) / 520.0) ** 2)
    alpha = np.minimum(0.46, total * 0.34) * band
    out = bg.astype(np.float32) * (1.0 - alpha) + color[None, None, :] * alpha
    return np.ascontiguousarray(out.clip(0, 255).astype(np.uint8))

TILE_CACHE = {}
BIG_FONT_CACHE = {}
def char_tile(ch, wpx, col, s=1.0, size=RAIL_SIZE,
              stroke_width=0, stroke_fill=None):
    stroke_fill = stroke_fill or col
    key = (ch, int(round(wpx)), col, round(s, 2), size,
           stroke_width, stroke_fill)
    tile = TILE_CACHE.get(key)
    if tile is None:
        font = FONTS[size]
        chh = size + 28
        bw = max(1, int(math.ceil(_meas.textlength(ch, font=font))))
        pad = stroke_width + 1 if stroke_width else 0
        im = Image.new("RGBA", (bw + 2 * pad, chh + 2 * pad), (0, 0, 0, 0))
        fill = col if len(col) == 4 else col + (255,)
        outline = stroke_fill if len(stroke_fill) == 4 else stroke_fill + (255,)
        ImageDraw.Draw(im).text((pad, pad + chh // 2), ch, font=font, fill=fill,
                                stroke_width=stroke_width, stroke_fill=outline,
                                anchor="lm")
        tw = max(1, int(round(wpx * s))) + 2 * pad
        th = max(1, int(round(chh * s))) + 2 * pad
        if (tw, th) != im.size:
            im = im.resize((tw, th), Image.LANCZOS)
        TILE_CACHE[key] = tile = im
    return tile

def active_words(t):
    out = []
    eps = 0.5 / FPS
    for rail in RAILS:
        for w in rail_words[rail]:
            if w["t0"] > t + eps:
                break
            if w["t0"] - eps <= t <= w["t1"] + eps:
                out.append((rail, w))
    return out

def display_at(w, t):
    full = "".join(c for c, _, _ in w["chars"])
    # The lexical skeleton is complete on the attack. Progressive substring
    # reveal was producing DAAA without SH and CUUU without LT.
    return full or w["word"]

def big_font(text, max_size, max_width):
    key = (text, max_size, max_width)
    font = BIG_FONT_CACHE.get(key)
    if font is not None:
        return font
    size = max_size
    while size > 62:
        font = ImageFont.truetype(FONT_B, size)
        if _meas.textlength(text, font=font) <= max_width:
            break
        size -= 8
    BIG_FONT_CACHE[key] = font
    return font

def chalk_path(layer, pts, col, alpha=180, width=4, seed=0):
    """A powdery hand line: one soft body and two imperfect chalk passes.
    The path carries real event relationships; texture never adds free marks."""
    dd = ImageDraw.Draw(layer, "RGBA")
    dd.line(pts, fill=col + (max(1, alpha // 5),), width=width * 5, joint="curve")
    for p in range(2):
        off = (hash01(seed * 13 + p * 97) - 0.5) * 4
        jp = [(x, y + off + (hash01(seed + i * 31 + p) - 0.5) * 3)
              for i, (x, y) in enumerate(pts)]
        dd.line(jp, fill=col + (max(1, alpha // 2),), width=max(1, width - p), joint="curve")

def draw_spatial_phrase(layer, rail, w, t, y=H / 2, max_size=310,
                        max_width=W - 86, row_amp=92):
    """Large readable type travelling on its own chalk trajectory. The
    phrase is never a static title card: its attack enters from a side, the
    glyphs sit on the drawn curve, and the curve continues into the score."""
    dd = ImageDraw.Draw(layer, "RGBA")
    txt = display_at(w, t).upper()
    font = big_font(txt, max_size, max_width)
    col = w.get("color", WHO[rail])
    widths = [_meas.textlength(c, font=font) for c in txt]
    total = max(1.0, sum(widths))
    seed = sum((i + 1) * ord(c) for i, c in enumerate(w["word"] + rail)) + w["strand"] * 101
    direction = -1 if seed % 2 else 1
    attack = max(0.0, min(1.0, (t - w["t0"]) / min(0.32, w["dur"])))
    attack = attack * attack * (3 - 2 * attack)
    enter = direction * (1 - attack) * W * 0.42
    x0 = (W - total) / 2 + enter
    slope = direction * (0.10 + 0.05 * hash01(seed + 3))
    phase = (hash01(seed + 17) - 0.5) * 0.8
    def py(x):
        u = (x - x0) / total
        return y + slope * (x - W / 2) + row_amp * math.sin((u + phase) * math.pi)
    p0, p1 = x0 - 160, x0 + total + 160
    path = [(x, py(x)) for x in np.linspace(p0, p1, 31)]
    chalk_path(layer, path, col, alpha=175, width=4, seed=seed)
    x = x0
    for c, cw in zip(txt, widths):
        cx = x + cw / 2
        cy = py(cx)
        if c != " ":
            dd.text((cx + 9, cy + 12), c, font=font, fill=(5, 3, 18, 205), anchor="mm")
            dd.text((cx, cy), c, font=font, fill=col + (255,), anchor="mm")
        x += cw

def draw_impact(layer, t):
    live = active_words(t)
    if not live:
        return
    if len(live) > 1:
        draw_poly(layer, t)
        return
    # Jeffrey leads where present; otherwise the latest attack gets the path.
    live.sort(key=lambda rw: (rw[0] == "jeffrey", rw[1]["t0"]))
    rail, w = live[-1]
    draw_spatial_phrase(layer, rail, w, t)

def draw_poly(layer, t):
    live = active_words(t)
    # Keep every truly simultaneous phrase, including parallel strands from
    # the same singer/chorus. The old latest-per-rail collapse is what made
    # lyric connections disappear.
    rows = sorted(live, key=lambda rw: (rw[1]["t0"], rw[0]))
    if len(rows) < 2:
        draw_impact(layer, t)
        return
    y0, y1 = 330, 1580
    for i, (rail, w) in enumerate(rows):
        yy = y0 + (y1 - y0) * (i + 0.5) / len(rows)
        draw_spatial_phrase(layer, rail, w, t, y=yy,
                            max_size=230 if len(rows) == 2 else max(104, 204 - len(rows) * 10),
                            max_width=W - 110, row_amp=48)

def draw_lyric_connections(layer, t, strong=False):
    # Draw only real consecutive relationships inside each interval-coloured
    # strand. Simultaneous phrases never steal one another's connector.
    for rail in RAILS:
        for w in rail_words[rail]:
            nxt = w.get("next_word")
            if nxt is None:
                continue
            # The connector starts at the authoritative utterance end, not
            # at a pixel-width proxy. Long gaps continue as horizon threads.
            ax = xwarp(lyric_x1(w, t))
            bx = xwarp(lyric_x0(nxt, t))
            if max(ax, bx) < -40 or min(ax, bx) > W + 40:
                continue
            ay = ypath(rail, ax) + w.get("yoff", 0.0)
            by = ypath(rail, bx) + nxt.get("yoff", 0.0)
            gap = max(0.0, nxt["t0"] - w["t1"])
            direction = -1 if rail == "camille" else (1 if rail == "alex" else 0)
            bend = direction * min(120.0, gap * 42.0)
            pts = []
            for u in np.linspace(0, 1, 17):
                x = ax + (bx - ax) * u
                y = ay + (by - ay) * u + math.sin(u * math.pi) * bend
                pts.append((x, y))
            col = w.get("color", WHO[rail])
            chalk_path(layer, pts, col, alpha=82 if strong else 56,
                       width=2,
                       seed=int(w["t0"] * 1000) + w["strand"] * 131)

def draw_bare(near, far, focus, t):
    # Each character is its own body: small and dim while it approaches,
    # snapping large and flashing as the VOICE reaches it. The animation is
    # clocked by the utterance itself for ordinary words. Long vowel trains
    # retain their world-space width: the attack is exact, the vowel remains
    # live while its offscreen body crosses, and the release belongs to the
    # actual tail instead of a viewport crop. Everything passes fully offscreen.
    for rail in RAILS:
        yc = RAIL_Y[rail]
        future = 0
        for w in rail_words[rail]:
            size = w.get("render_size", RSIZE[rail])
            chh = size + 28
            col = w.get("color", WHO[rail])
            t0 = w["t0"]
            t_end = t0 + w["dur"]
            visual_end = t0 + w.get("visual_dur", w["dur"])
            state_end = max(t_end, visual_end)
            is_cult = w["word"].lower() == "cult"
            x0 = lyric_x0(w, t)
            xe = x0 + w["px"]
            word_hold = CULT_HOLD if is_cult else 0.0
            word_decay = CULT_COOL if is_cult else WORD_COOL
            if t > state_end + word_hold + word_decay or (xe < -40 and not TUNNEL):
                continue
            if t0 > t:
                future += 1
                if future > (6 if TUNNEL else 2) and xwarp(x0) > W - 60:
                    break
            prog = (t - t0) / w["dur"]
            pinned = t0 > t and future <= 2
            pin_x = None
            for (c, off, wp) in w["chars"]:
                ck0 = off / w["px"]
                ck1 = (off + wp) / w["px"]
                char_start = t0 + ck0 * w["dur"]
                char_end = t0 + ck1 * w["dur"]
                if is_cult:
                    # CULT is one sustained vowel section, not eighteen tiny
                    # syllables. C opens it, every U stays live for the full
                    # scored hold, and LT releases together at its exact end.
                    edge = min(0.09, w["dur"] * 0.08)
                    if c.lower() == "u":
                        char_start = t0 + edge
                        char_end = state_end
                    elif c.lower() == "c":
                        char_start, char_end = t0, t0 + edge
                    elif c.lower() == "l":
                        char_start, char_end = state_end - edge, state_end - edge / 2
                    elif c.lower() == "t":
                        char_start, char_end = state_end - edge / 2, state_end
                active = char_start <= t < char_end
                burn_age = t - char_end
                fresh_burn = 0 <= burn_age < CHAR_TRAIL
                ember = char_end <= t <= state_end
                word_cool = t > state_end
                upcoming = t < char_start
                if not (upcoming or active or ember or word_cool):
                    pin_x = xwarp(x0 + off + wp) if pinned else None
                    continue
                cx = x0 + off
                xa, xb = xwarp(cx), xwarp(cx + wp)
                d = (xb - xa) / wp
                if pinned and d < PIN_S:
                    d = PIN_S
                    xa = pin_x if pin_x is not None else min(xa, W - 52 - w["px"] * PIN_S)
                    xb = xa + wp * d
                if c == " ":
                    pin_x = xb if pinned else None
                    continue
                if xb - xa < 1.5 or ((xa > W or xb < -10)
                                     and not (TUNNEL and (t < char_start or word_cool))):
                    # deep tunnel glyphs live near the vanishing point even
                    # when their arrival slot is far outside the frame
                    pin_x = xb if pinned else None
                    continue
                slot = xb - xa
                ycc = ypath(rail, (xa + xb) / 2) + w.get("yoff", 0.0)
                vibex, vibey, wide, tall, angle, accent, tint = character_vibe(
                    t, rail, c, off)
                live_col = blend(col, accent, tint)
                if active:
                    age = t - char_start
                    heat = math.exp(-age / FLASH_DECAY)
                    s = 1.02 + 0.24 * heat
                    tcol = blend(live_col, INK, 0.88 * heat)
                    fade = 1.0
                elif upcoming:
                    s = PRE_S
                    # emerging letters carry more of their voice color and
                    # alpha so the deep train reads bright out of the hole
                    tcol = blend(PREC[:3], live_col, 0.64) + (min(255, PREC[3] + 40),)
                    fade = 1.0
                elif ember:
                    cool = min(1.0, max(0.0, burn_age / CHAR_TRAIL))
                    # A completed character holds the performer's saturated
                    # color until every character in the word has been sung.
                    s = 1.0 - 0.03 * cool
                    tcol = blend(INK, live_col, 0.72 + 0.28 * cool)
                    fade = 0.90
                else:
                    cool = min(1.0, max(0.0, (t - state_end - word_hold) / word_decay))
                    if TUNNEL:
                        # the finished word does not burn out: its letters
                        # shrink and fly to the ring field, keeping color
                        target = blend(INK, blend(col, (255, 103, 42), 0.35), 0.72)
                        tcol = blend(blend(INK, live_col, 0.72), target, cool)
                        s = 0.97 - 0.82 * cool
                        fade = 0.88
                    else:
                        # The fully readable word cools as one body: voice
                        # color through orange ember into the ground.
                        ember_col = blend(live_col, (255, 103, 42), min(1.0, cool * 2.5))
                        tcol = blend(ember_col, GROUND, max(0.0, (cool - 0.38) / 0.62))
                        s = 0.97 - 0.20 * cool
                        fade = 0.90 * (1.0 - cool) ** 1.15
                        ycc -= 22 * cool
                vibe = (1.0 if active else 0.12 if upcoming else
                        0.58 if ember else 0.38 * (1.0 - cool))
                # In tunnel mode the letters ride the same wormhole as the
                # walls: an upcoming glyph is born at the vanishing point at
                # its own utterance depth and flies OUT of the tube, arriving
                # at its front position exactly when the voice reaches it; a
                # finished word blows past the camera and out of the frame.
                ccx = xa + slot / 2
                depth = 1.0
                if TUNNEL:
                    if upcoming:
                        dz = (char_start - t) + TUN_Z0
                        depth = TUN_Z0 / dz
                        fade *= 0.62 + 0.38 * depth
                    elif word_cool:
                        # flight to the aggregate field on the ring's outside
                        rcx_, rcy_, rr_ = TUN_RING_SCREEN
                        fseed = int(w["t0"] * 997) + int(off) * 31
                        ang = hash01(fseed) * 2 * math.pi
                        rad = rr_ + 44 + 170 * hash01(fseed + 7)
                        eu = cool * cool * (3 - 2 * cool)
                        ccx += (rcx_ + math.cos(ang) * rad - ccx) * eu
                        ycc += (rcy_ + math.sin(ang) * rad - ycc) * eu
                    if depth != 1.0:
                        vx, vy = TUN_VP
                        ccx = vx + (ccx - vx) * depth
                        ycc = vy + (ycc - vy) * depth
                        s *= depth
                        if depth < 0.05:
                            pin_x = xb if pinned else None
                            continue
                # Only the glyph being voiced is unblurred. Its hot outline
                # cools once into voice color. Completed letters stay bright
                # until phrase end, when the whole readable word burns out.
                layer = focus if active else (
                    far if (TUNNEL and depth < 0.55) else (near if d > 0.55 else far))
                tw = max(1, int(round(slot * s * (1.0 + (wide - 1.0) * vibe))))
                th = max(1, int(round(chh * s * d * (1.0 + (tall - 1.0) * vibe))))
                if active:
                    stroke = 1 + int(round(3 * heat))
                    full = char_tile(c, wp, tcol, 1.0, size,
                                     stroke_width=stroke, stroke_fill=INK)
                    tw += stroke * 2 + 2
                    th += stroke * 2 + 2
                    tile = full.resize((tw, th), Image.LANCZOS)
                elif upcoming and d >= 0.97 and depth >= 0.995:
                    tile = char_tile(c, wp, tcol, PRE_S, size)
                    tw, th = tile.width, tile.height
                else:
                    full = char_tile(c, wp, tcol, 1.0, size)
                    resample = Image.NEAREST if (TUNNEL and depth != 1.0) else Image.LANCZOS
                    tile = full.resize((tw, th), resample)
                if fade < 0.995:
                    tile = tile.copy()
                    tile.putalpha(tile.getchannel("A").point(lambda a: int(a * fade)))
                if abs(angle * vibe) >= 0.15:
                    tile = tile.rotate(angle * vibe, resample=Image.BICUBIC, expand=True)
                    tw, th = tile.size
                px_ = ccx + vibex * vibe - tw / 2
                py_ = ycc + vibey * vibe - th / 2
                layer.paste(tile, (int(round(px_)), int(round(py_))), tile)
                if fresh_burn:
                    # Three deterministic rising strokes read as burnt chalk,
                    # never as the DOT word-cloud's literal score marks.
                    bd = ImageDraw.Draw(near, "RGBA")
                    seed = int(w["t0"] * 1000) + int(off) * 17
                    ember_a = int(210 * fade)
                    for j in range(3):
                        ex = (xa + xb) / 2 + (hash01(seed + j * 19) - 0.5) * slot * 0.55
                        ey = ycc + (hash01(seed + j * 31) - 0.5) * th * 0.42
                        rise = 9 + 21 * hash01(seed + j * 47)
                        bd.line((ex, ey, ex + (hash01(seed + j * 59) - 0.5) * 9,
                                 ey - rise), fill=(255, 126, 54, ember_a), width=2)
                pin_x = xb if pinned else None

def hblur(im, k):
    # horizontal box blur on premultiplied RGBA — the motion of the scroll
    r = int(k) // 2
    if r < 1:
        return im
    a = np.asarray(im, dtype=np.float32)
    al = a[..., 3:4] / 255.0
    buf = np.concatenate([a[..., :3] * al, a[..., 3:4]], axis=2)
    n = 2 * r + 1
    pad = np.pad(buf, ((0, 0), (r, r + 1), (0, 0)))
    c = np.cumsum(pad, axis=1)
    out = (c[:, n:, :] - c[:, :-n, :]) / n
    al2 = out[..., 3:4]
    rgb = out[..., :3] / np.maximum(al2 / 255.0, 1e-4)
    return Image.fromarray(
        np.concatenate([rgb, al2], axis=2).clip(0, 255).astype(np.uint8), "RGBA")

# ---------------------------------------------------------------- tunnel
# --tunnel: the background becomes a Descent-style software-rasterized tube.
# The camera races down a gently curving 15-sided tunnel whose faces ARE the
# score's lanes: every wall texel is sourced from a real event block, a bar
# strut, or the measured 50 Hz bus envelopes — nothing decorative. The tube
# radius pumps with the actual kick and bass envelopes at each ring's own
# score time, so a loud passage ahead reads as a constriction rushing in.
# Triangulated quads with affine nearest-neighbour texture sampling, drawn
# painter's-order at half resolution and NN-upscaled ×2, keep the honest
# chunk (and the affine swim) of an early software renderer.
TUN_W, TUN_H = W // 2, H // 2
TUN_FOCAL = 420.0
TUN_Z0 = 0.42                   # camera floats just behind the now ring
TUN_FAR = 6.5                   # seconds of track visible ahead
TUN_RINGS = 34
TUN_R0 = 0.75
TEX_PPS = 18                    # texel columns per second — one ≈ 55 ms
TEX_H = 28
FOG = np.array((6, 3, 22), dtype=np.float32)

def _tex_rows(midi, lo, hi):
    if hi <= lo:
        return TEX_H // 2 - 4, TEX_H // 2 + 4
    u = (midi - lo) / (hi - lo)
    yc = int(round((1.0 - u) * (TEX_H - 8))) + 4
    return max(0, yc - 4), min(TEX_H, yc + 4)

def build_lane_texture(lane):
    cols = int(DUR * TEX_PPS) + 2
    col = np.array(blend(tuple(lane["color"]), INK, 0.18), dtype=np.float32)
    tex = np.zeros((TEX_H, cols, 3), dtype=np.float32)
    tex += col[None, None, :] * 0.22          # unlit wall panel
    for bb in S["bars"]:
        x = int(bb["t"] * TEX_PPS)
        if 0 <= x < cols:
            lift = 0.42 if bb["bar"] % 4 == 0 else 0.22
            tex[:, x] = np.maximum(tex[:, x], col * lift)
    if lane["kind"] == "au":
        env = np.asarray(lane["env"], dtype=np.float32)
        hz = float(lane["env_hz"])
        idx = np.clip((np.arange(cols) / TEX_PPS * hz).astype(int), 0, len(env) - 1)
        amp = env[idx] ** 0.72
        half = (amp * (TEX_H / 2 - 1)).astype(int)
        rows = np.arange(TEX_H)[:, None]
        m = np.abs(rows - TEX_H // 2) <= half[None, :]
        lit = col[None, None, :] * (0.42 + 0.85 * amp)[None, :, None]
        tex = np.where(m[..., None], np.maximum(tex, lit), tex)
    else:
        lo, hi = lane["_midi"]
        for e in lane["events"]:
            x0 = int(float(e["t0"]) * TEX_PPS)
            x1 = max(x0 + 1, int(float(e["t1"]) * TEX_PPS))
            midi = float(e["midi"]) if e.get("midi") is not None else (lo + hi) / 2
            r0, r1 = _tex_rows(midi, lo, hi)
            tex[r0:r1, x0:x1] = np.maximum(tex[r0:r1, x0:x1], col * 0.96)
            # the actual sound inside the clip: shipped-master peaks per
            # texel column drawn as a subwaveform within the block body
            bh = max(1, (r1 - r0) // 2)
            ctr = (r0 + r1) // 2
            wcol = np.array(blend(tuple(lane["color"]), INK, 0.55), dtype=np.float32)
            for x in range(x0, min(x1, cols)):
                hh = int(round(float(MIX_TEXCOL[x]) * bh))
                if hh > 0:
                    tex[max(r0, ctr - hh):min(r1, ctr + hh + 1), x] = wcol
            tex[r0:r1, x0] = 255.0            # white-hot attack texel column
    return np.clip(tex, 0, 255)

def _mix_texcols():
    """Shipped-master peak per texel column, on the score clock."""
    cols = int(DUR * TEX_PPS) + 2
    peaks = np.zeros(cols, dtype=np.float32)
    mono = np.abs(MIX_PCM).max(axis=1)
    for x in range(cols):
        a = int(master_time(x / TEX_PPS) * MIX_RATE)
        b = max(a + 1, int(master_time((x + 1) / TEX_PPS) * MIX_RATE))
        if a >= len(mono):
            break
        peaks[x] = float(mono[a:min(b, len(mono))].max())
    return peaks

MIX_TEXCOL = _mix_texcols() if TUNNEL else None
TUN_TEX = [build_lane_texture(lane) for lane in LANES] if TUNNEL else []

def tun_path(u):
    return (0.34 * math.sin(0.47 * u + 1.3) + 0.21 * math.sin(0.171 * u),
            0.26 * math.sin(0.29 * u + 4.1) + 0.13 * math.sin(0.531 * u))

def tun_dpath(u):
    e = 0.05
    x0, y0 = tun_path(u - e)
    x1, y1 = tun_path(u + e)
    return ((x1 - x0) / (2 * e), (y1 - y0) / (2 * e))

def _tri(fb, pts, uvs, tex, shade, fogc=FOG, decay=0.0):
    (x0f, y0f), (x1f, y1f), (x2f, y2f) = pts
    xa = max(0, int(min(x0f, x1f, x2f)))
    xb = min(TUN_W, int(max(x0f, x1f, x2f)) + 1)
    ya = max(0, int(min(y0f, y1f, y2f)))
    yb = min(TUN_H, int(max(y0f, y1f, y2f)) + 1)
    if xb <= xa or yb <= ya:
        return
    d = (x1f - x0f) * (y2f - y0f) - (x2f - x0f) * (y1f - y0f)
    if abs(d) < 1e-9:
        return
    X, Y = np.meshgrid(np.arange(xa, xb, dtype=np.float32) + 0.5,
                       np.arange(ya, yb, dtype=np.float32) + 0.5)
    w1 = ((X - x0f) * (y2f - y0f) - (x2f - x0f) * (Y - y0f)) / d
    w2 = ((x1f - x0f) * (Y - y0f) - (X - x0f) * (y1f - y0f)) / d
    # slightly inclusive edges: neighbouring triangles overlap by a hair
    # instead of leaving fog-coloured cracks that read as tearing
    e = 0.004
    m = (w1 >= -e) & (w2 >= -e) & (w1 + w2 <= 1 + e)
    if not m.any():
        return
    (u0, v0), (u1, v1), (u2, v2) = uvs
    tu = u0 + w1[m] * (u1 - u0) + w2[m] * (u2 - u0)
    tv = v0 + w1[m] * (v1 - v0) + w2[m] * (v2 - v0)
    th, tw = tex.shape[:2]
    ui = np.clip(tu.astype(np.int32), 0, tw - 1)
    vi = np.clip(tv.astype(np.int32), 0, th - 1)
    if decay > 0.0:
        # played material crumbles texel-by-texel as it crosses the now
        # ring: a deterministic per-texel dropout, no random sparkle.
        keep = (((ui.astype(np.int64) * 2654435761 + vi.astype(np.int64) * 2246822519)
                 >> 7) % 19) >= int(decay * 17)
        if not keep.any():
            return
        my, mx = np.where(m)
        m[my[~keep], mx[~keep]] = False
        ui, vi = ui[keep], vi[keep]
    sub = fb[ya:yb, xa:xb]
    sub[m] = tex[vi, ui] * shade[0] + fogc * shade[1]

def lane_flash(li, t):
    """How hard lane li's wall is hitting RIGHT NOW, from its own data:
    event lanes flame from their most recent real attack; envelope lanes
    flame from their measured one-frame rise. Never from a synthesized LFO."""
    lane = LANES[li]
    if lane.get("kind") == "au":
        _, d = bus_value(lane["name"], t)
        return min(1.0, max(0.0, d * 9.0))
    times = lane["_times"]
    i = bisect.bisect_right(times, t) - 1
    if i < 0:
        return 0.0
    dt = t - times[i]
    return math.exp(-dt / 0.11) if dt >= 0 else 0.0

TUN_VP = (W / 2, H / 2)          # where the wormhole swallows, full-res
TUN_RING_ZONE = 1.0              # seconds: material inside the now ring dies
TUN_RING_SCREEN = (W / 2, H / 2, 460.0)   # now-ring centre + radius, full-res

def tunnel_frame(t, master_t):
    global TUN_VP
    fb = np.zeros((TUN_H, TUN_W, 3), dtype=np.float32)
    fb += FOG
    cam = tun_path(t)
    dcam = tun_dpath(t)
    roll = -1.35 * dcam[0] + 0.10 * math.sin(0.11 * t)
    # the live hit state: measured levels and signed motion at the camera
    kickL, _ = bus_value("kick <150", t)
    bassL, _ = bus_value("bass <250", t)
    percL, dperc = bus_value("perc+skids", t)
    _, dsig = bus_value("signal", t)
    glow = min(1.0, 0.75 * kickL + 0.34 * bassL + 0.30 * percL
               + 3.2 * max(0.0, dsig))
    focal = TUN_FOCAL * (1.0 + 0.09 * kickL + 0.12 * max(0.0, dperc) * 3.2)
    flashes = [lane_flash(li, t) for li in range(NL)]
    # the act owns the atmosphere: fog inherits the current act's color, so
    # the whole tube changes hue at every act door like the flat bg used to.
    act_col = GROUND
    for a in ACTS:
        if t >= a["t0"]:
            act_col = tuple(a["color"])
        else:
            break
    fogc = np.array(blend((6, 3, 22), act_col, 0.42), dtype=np.float32)
    fb[:] = fogc
    rings = []
    # power-law depth spacing: the nearest slabs are thin so the giant
    # close-up quads stay small (less affine swim, no edge tearing), the
    # far field keeps its chunky Descent segments.
    for u in TUN_FAR * (np.arange(TUN_RINGS) / (TUN_RINGS - 1)) ** 1.7 + t:
        z = (u - t) + TUN_Z0
        s = focal / z
        ue = min(DUR - 0.05, u)
        kick, _ = bus_value("kick <150", ue)
        bass, _ = bus_value("bass <250", ue)
        R = TUN_R0 * (1.0 + 0.20 * kick + 0.10 * bass)
        px, py = tun_path(u)
        cx = TUN_W / 2 + (px - cam[0] - dcam[0] * (u - t)) * s
        cy = TUN_H / 2 + (py - cam[1] - dcam[1] * (u - t)) * s
        vs = [(cx + math.cos(roll + k * 2 * math.pi / NL) * R * s,
               cy + math.sin(roll + k * 2 * math.pi / NL) * R * s)
              for k in range(NL + 1)]
        rings.append((u, vs, (cx, cy)))
    TUN_VP = (rings[-1][2][0] * 2, rings[-1][2][1] * 2)
    for i in range(TUN_RINGS - 2, -1, -1):    # painter: far pair first
        u0, va, _ = rings[i]
        u1, vb, _ = rings[i + 1]
        zmid = (u0 + u1) / 2 - t + TUN_Z0
        fog = min(0.88, max(0.0, (zmid - 0.55) / TUN_FAR) ** 0.8)
        tx0, tx1 = u0 * TEX_PPS, u1 * TEX_PPS
        prox = max(0.0, 1.0 - (u0 - t) / 1.6)   # hits flame hardest up close
        # inside the now ring the material has been played: it crumbles
        decay = max(0.0, 1.0 - (u0 - t) / TUN_RING_ZONE)
        for k in range(NL):
            a_mid = roll + (k + 0.5) * 2 * math.pi / NL
            lam = 0.66 + 0.34 * (0.5 + 0.5 * math.cos(a_mid * 2.0))
            lit = ((1.0 - fog) * lam * (0.78 + 0.80 * glow)
                   * (1.0 + 2.2 * flashes[k] * prox)
                   * (1.0 - 0.45 * decay))
            tex = TUN_TEX[k]
            _tri(fb, (va[k], va[k + 1], vb[k]),
                 ((tx0, 0.0), (tx0, TEX_H - 1.0), (tx1, 0.0)), tex, (lit, fog), fogc, decay)
            _tri(fb, (va[k + 1], vb[k + 1], vb[k]),
                 ((tx0, TEX_H - 1.0), (tx1, TEX_H - 1.0), (tx1, 0.0)), tex, (lit, fog), fogc, decay)
    im = Image.fromarray(fb.clip(0, 255).astype(np.uint8), "RGB")
    # The now ring IS the waveform, radialized: the last 90 ms of the
    # shipped master bends the circle's radius point by point, and overall
    # loudness carries brightness and stroke. Fully inside the viewport —
    # it is the gate the walls crumble through, so it is seen entire.
    z_ring = TUN_RING_ZONE + TUN_Z0
    s_ring = focal / z_ring
    kickR, _ = bus_value("kick <150", min(DUR - 0.05, t + TUN_RING_ZONE))
    r_ring = TUN_R0 * (1.0 + 0.20 * kickR) * s_ring
    u_r = t + TUN_RING_ZONE
    rpx, rpy = tun_path(u_r)
    rcx = TUN_W / 2 + (rpx - cam[0] - dcam[0] * TUN_RING_ZONE) * s_ring
    rcy = TUN_H / 2 + (rpy - cam[1] - dcam[1] * TUN_RING_ZONE) * s_ring
    r_max = r_ring * 1.28
    rcx = max(r_max + 8, min(TUN_W - r_max - 8, rcx))
    rcy = max(r_max + 8, min(TUN_H - r_max - 8, rcy))
    global TUN_RING_SCREEN
    TUN_RING_SCREEN = (rcx * 2, rcy * 2, r_ring * 2)
    i1 = max(0, min(len(MIX_PCM), int(master_t * MIX_RATE)))
    i0 = max(0, i1 - MIX_RATE // 11)          # 90 ms window
    if i1 > i0 + 8:
        win = MIX_PCM[i0:i1].mean(axis=1)
        level = min(1.0, float(np.sqrt(np.mean(win ** 2))) * 2.6)
        if level > 0.03:
            NPT = 180
            samp = win[(np.arange(NPT) * (len(win) - 1) // (NPT - 1))]
            pts = []
            for j in range(NPT + 1):
                sj = float(samp[j % NPT])
                rj = r_ring * (1.0 + 0.26 * sj)
                aj = roll + (j % NPT) / NPT * 2 * math.pi
                pts.append((rcx + math.cos(aj) * rj, rcy + math.sin(aj) * rj))
            dd = ImageDraw.Draw(im, "RGBA")
            dd.line(pts, fill=(255, 240, 220, int(36 + 200 * level)),
                    width=max(2, int(1 + 8 * level)), joint="curve")
    return im.resize((W, H), Image.NEAREST)

def draw_letter_field(layer, t):
    """Every sung word's letters end up aggregated on the OUTSIDE of the now
    ring: a 2-D particle field of the record's spent lyrics that keeps
    growing as the track plays through. Placement is deterministic per glyph,
    with only a slow orbital drift once settled."""
    rcx, rcy, rr = TUN_RING_SCREEN
    for rail in RAILS:
        for w in rail_words[rail]:
            is_cult = w["word"].lower() == "cult"
            word_hold = CULT_HOLD if is_cult else 0.0
            word_decay = CULT_COOL if is_cult else WORD_COOL
            settle = (w["t0"] + max(w["dur"], w.get("visual_dur", w["dur"]))
                      + word_hold + word_decay)
            if settle > t:
                continue
            col = w.get("color", WHO[rail])
            pcol = blend(INK, blend(col, (255, 103, 42), 0.35), 0.72)
            size = w.get("render_size", RSIZE[rail])
            age = t - settle
            for (c, off, wp) in w["chars"]:
                if c == " ":
                    continue
                seed = int(w["t0"] * 997) + int(off) * 31
                drift = 0.035 * age * (1.0 if (seed & 4) else -1.0)
                ang = hash01(seed) * 2 * math.pi + drift
                rad = rr + 44 + 170 * hash01(seed + 7)
                px_ = rcx + math.cos(ang) * rad
                py_ = rcy + math.sin(ang) * rad
                if px_ < -40 or px_ > W + 40 or py_ < -40 or py_ > H + 40:
                    continue
                tile = char_tile(c, wp, pcol, 0.15, size)
                layer.paste(tile, (int(px_ - tile.width / 2),
                                   int(py_ - tile.height / 2)), tile)

def compose_editorial(t, scene_index, view):
    if TUNNEL and view == "race":
        base = tunnel_frame(t, master_time(t)).convert("RGBA")
    else:
        bg = scroll_bg_frame(t) if SCROLL_ONLY and view == "race" else bg_frame(t, scene_index)
        if SCROLL_ONLY and view == "race":
            bg = word_ambient_frame(bg, t)
        base = Image.fromarray(bg).convert("RGBA")
    data = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    dots = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    links = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    if not (TUNNEL and view == "race"):
        draw_track_data(data, t, view)
        draw_mix_oscilloscope(data, master_time(t))
        if SCROLL_ONLY and t < INTRO_END:
            draw_intro_notation(data, t)
    draw_dot_field(dots, t, view)
    draw_lyric_connections(links, t, strong=view == "race")
    base.alpha_composite(data)
    base.alpha_composite(dots)
    base.alpha_composite(links)       # chalk over the score, under the glyphs
    if TUNNEL and view == "race":
        fieldl = Image.new("RGBA", (W, H), (0, 0, 0, 0))
        draw_letter_field(fieldl, t)
        base.alpha_composite(fieldl)
    identity = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    draw_opening_identity(identity, t, master_time(t))
    base.alpha_composite(identity)
    if view == "race":
        nearl = Image.new("RGBA", (W, H), (0, 0, 0, 0))
        farl = Image.new("RGBA", (W, H), (0, 0, 0, 0))
        focusl = Image.new("RGBA", (W, H), (0, 0, 0, 0))
        draw_bare(nearl, farl, focusl, t)
        if BLUR >= 2:
            farl = hblur(farl, max(2, int(BLUR * 0.15)))
            nearl = hblur(nearl, BLUR)
        for layer in (farl, nearl, focusl):
            sh = Image.new("RGBA", layer.size, (5, 3, 18, 255))
            sh.putalpha(layer.getchannel("A").point(lambda v: (v * 118) // 255))
            base.alpha_composite(sh, (6, 8))
            base.alpha_composite(layer)
    else:
        paths = Image.new("RGBA", (W, H), (0, 0, 0, 0))
        if view == "poly":
            draw_poly(paths, t)
        else:
            draw_impact(paths, t)
        base.alpha_composite(paths)
    return base

TRANSITION = 0.34
def spatial_wipe(prev, cur, u, scene_index):
    u = max(0.0, min(1.0, u))
    u = u * u * (3 - 2 * u)
    mask = Image.new("L", (W, H), 0)
    md = ImageDraw.Draw(mask)
    overlay = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    col = (WHO["camille"], WHO["alex"], WHO["jeffrey"], INK)[scene_index % 4]
    mode = scene_index % 3
    if mode == 0:
        edge = -W * 0.35 + W * 1.70 * u
        slant = 0.22 if scene_index % 2 else -0.22
        pts = [(edge + slant * (y - H / 2), y) for y in np.linspace(0, H, 33)]
        poly = [(0, 0)] + pts + [(0, H)]
        md.polygon(poly, fill=255)
        chalk_path(overlay, pts, col, alpha=230, width=6, seed=scene_index * 101)
    elif mode == 1:
        edge = -H * 0.22 + H * 1.44 * u
        slant = -0.16 if scene_index % 2 else 0.16
        pts = [(x, edge + slant * (x - W / 2)) for x in np.linspace(0, W, 29)]
        poly = [(0, 0), (W, 0)] + list(reversed(pts))
        md.polygon(poly, fill=255)
        chalk_path(overlay, pts, col, alpha=230, width=6, seed=scene_index * 101)
    else:
        rx, ry = W * 0.78 * u, H * 0.70 * u
        md.ellipse((W / 2 - rx, H / 2 - ry, W / 2 + rx, H / 2 + ry), fill=255)
        pts = [(W / 2 + math.cos(a) * rx, H / 2 + math.sin(a) * ry)
               for a in np.linspace(0, 2 * math.pi, 49)]
        chalk_path(overlay, pts, col, alpha=230, width=6, seed=scene_index * 101)
    out = Image.composite(cur, prev, mask)
    out.alpha_composite(overlay)
    return out

def frame_at(t, master_t=None):
    if master_t is None:
        master_t = master_time(t)
    if BARE:
        if master_t >= DUR - 0.12:     # the final click cuts the picture too
            return Image.new("RGB", (W, H), (4, 2, 12))
        if SCROLL_ONLY:
            frame = compose_editorial(t, 0, "race").convert("RGB")
            return frame.filter(ImageFilter.UnsharpMask(radius=0.85, percent=145, threshold=2))
        scene_index, view, started = scene_at(t)
        cur = compose_editorial(t, scene_index, view)
        age = t - started
        if scene_index > 0 and age < TRANSITION:
            prev = compose_editorial(t, scene_index - 1, SCENES[scene_index - 1][1])
            cur = spatial_wipe(prev, cur, age / TRANSITION, scene_index)
        return cur.convert("RGB")
    off = int(round(t * PPS))
    win = strip_np[:, off:off + W].astype(np.float32)
    win += (strip_lit[:, off:off + W].astype(np.float32) - win) * LIGHT
    base = Image.fromarray(win.astype(np.uint8)).convert("RGBA")
    base.alpha_composite(veil)
    base.alpha_composite(chrome)
    if BLUR >= 2:
        wl = Image.new("RGBA", (W, H), (0, 0, 0, 0))
        draw_words(wl, t)
        base.alpha_composite(hblur(wl, BLUR))
        img = base.convert("RGB")
    else:
        img = base.convert("RGB")
        draw_words(img, t)
    if not BARE:
        # progress along the bottom of the safe box
        dd = ImageDraw.Draw(img)
        dd.rectangle([SAFE_X0, SAFE_Y1 - 6, W - 20, SAFE_Y1 - 2], fill=blend(GROUND, INK, 0.18))
        dd.rectangle([SAFE_X0, SAFE_Y1 - 6, SAFE_X0 + int((W - 20 - SAFE_X0) * t / DUR), SAFE_Y1 - 2],
                     fill=blend(GROUND, INK, 0.75))
    return img

if STILLS:
    for t in STILLS:
        path = OUT.replace(".mp4", f"-still-{t:05.1f}.png")
        frame_at(score_time(t), t).save(path)
        print(f"still → {path}")
    sys.exit(0)

NF = int((END - START) * FPS)
print(f"encoding {NF} frames → {OUT}", flush=True)
ff = subprocess.Popen(["ffmpeg", "-y", "-v", "error",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
    "-ss", str(START), "-t", str(END - START), "-i", AUDIO,
    "-map", "0:v", "-map", "1:a",
    "-c:v", "libx264", "-preset", "medium", "-tune", "animation", "-crf", "17",
    "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "256k", "-movflags", "+faststart", "-shortest", OUT],
    stdin=subprocess.PIPE)
t_start = time.time()
for f in range(NF):
    master_t = START + f / FPS
    ff.stdin.write(frame_at(score_time(master_t), master_t).tobytes())
    if f % 300 == 0:
        print(f"  frame {f}/{NF}  ({time.time() - t_start:.0f}s)", flush=True)
ff.stdin.close(); ff.wait()
print(f"done in {time.time() - t_start:.0f}s → {OUT}")
