#!/usr/bin/env python3
# lyricscroll.py — imab's lyric-timing review clip on the common chassis
# (pop/lib/lyricvideo.py). Strip-rendered for speed: the whole timeline
# is drawn ONCE into a wide image, each frame is a crop + playhead +
# active-word highlights. Taller canvas (1920×1440), full piano-note
# ladder up the left, thick blocks, per-take color themes: the lead
# take rides pink, every placed vocal set (vocal-sets.json) gets its
# own accent for blocks, ribbon and pitch trace.
#
# needs lyrictrack.mjs run first (timing JSONs) + floor demo1.
#   pop/.venv/bin/python pop/imab/bin/lyricscroll.py     (FPS=60 env for smooth)
#     → pop/imab/out/imab-floor-lyricscroll.mp4

import json, math, os, sys
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
OUT = os.path.join(LANE, "out")
sys.path.insert(0, os.path.join(os.path.dirname(LANE), "lib"))
import lyricvideo as lv

W, H = 1920, 1440
FPS = int(os.environ.get("FPS", "30"))
BPM = 124.0
SPB = 60.0 / BPM
PLAYHEAD_X = 560
scroll = lv.Scroll(playhead_x=PLAYHEAD_X, px_per_beat=96, spb=SPB)
TH = lv.theme()

# per-take accent themes (lead = the theme's pink; sets cycle these)
SET_ACCENTS = [
    {"HOT": (46, 172, 160), "BLOCK": (46, 172, 160, 52), "GLOW": (46, 172, 160, 72), "TRACE": (36, 150, 140)},
    {"HOT": (222, 152, 44), "BLOCK": (222, 152, 44, 52), "GLOW": (222, 152, 44, 72), "TRACE": (200, 134, 30)},
]

# ── the lead take: lyrictrack's JSONs · notes = the fitted GT register ─
stemdoc = json.load(open(os.path.join(OUT, "imab-sacredvox.lyrics.json")))
floordoc = json.load(open(os.path.join(OUT, "imab-floor-demo1.lyrics.json")))
GT_NOTES = ["C4", "G4", "C4", "C4", "C4", "C4", "C5", "C4", "C4", "C4",
            "G4", "F4", "E4", "D4", "E4", "E4", "D4", "C4", "C4", "C4"]
FITTED = os.path.join(OUT, "imab-gt-targets.fitted.json")
if os.path.exists(FITTED):
    GT_NOTES = [t["note"] for t in json.load(open(FITTED))]
sylls = stemdoc["syllables"]
assert len(sylls) == len(GT_NOTES), "syllable count drifted from the GT hook"

words, ribbon = [], []
for p in floordoc["passes"]:
    start = p["startSec"]
    for i, s in enumerate(sylls):
        words.append({"t": start + s["fromMs"] / 1000.0,
                      "dur": max((s["toMs"] - s["fromMs"]) / 1000.0, 0.2),
                      "stem_t": s["fromMs"] / 1000.0,
                      "midi": lv.to_midi(GT_NOTES[i]), "label": s["label"],
                      "note": GT_NOTES[i],
                      "off": p["syllables"][i]["gridOffMs"]})
    for wd in stemdoc["words"]:
        ribbon.append({"t": start + wd["fromMs"] / 1000.0,
                       "t1": start + wd["toMs"] / 1000.0, "text": wd["text"]})

AUD = os.path.join(OUT, "imab-floor-demo1.mp3")
if os.path.getmtime(AUD) < os.path.getmtime(os.path.join(OUT, "imab-floor-demo1.lyrics.json")):
    sys.exit("✗ floor mix is OLDER than the timing JSONs — the video would draw "
             "new timing over stale audio. re-run: IMAB_VOX=pop/imab/out/"
             "imab-aesthetivox-retimed.wav node pop/imab/bin/floor.mjs")
dur = lv.duration(AUD)
START = float(os.environ.get("START_BAR", "16")) * 4 * SPB
END = min(dur, float(os.environ.get("END_BAR", "33")) * 4 * SPB)
TOTAL_BEATS = int(math.ceil(dur / SPB))

samples = lv.mono(AUD)
VOX = next(p for p in ["imab-aesthetivox-retimed.wav", "imab-aesthetivox.wav",
                       "imab-sacredvox.wav"]
           if os.path.exists(os.path.join(OUT, p)))
voxsamp = lv.mono(os.path.join(OUT, VOX))
ft, fm = lv.f0_trace(os.path.join(OUT, VOX))

# ── placed vocal sets: word blocks from fitted targets + trace + ribbon ─
SETS = []
sets_path = os.path.join(LANE, "vocal-sets.json")
if os.path.exists(sets_path):
    placed = [s for s in json.load(open(sets_path)).get("sets", []) if s.get("at")]
    for k, s in enumerate(placed):
        L, beat = s["at"][0].upper(), float(s["at"][1:])
        off = (16 + ord(L) - 65) * 4 * SPB + (beat - 1) * SPB
        wav = os.path.join(OUT, f"imab-set-{s['take']}.wav")
        tgt = os.path.join(OUT, f"imab-set-{s['take']}-targets.fitted.json")
        if not os.path.exists(tgt):
            tgt = os.path.join(OUT, f"imab-set-{s['take']}-targets.json")
        if not (os.path.exists(wav) and os.path.exists(tgt)):
            continue
        acc = SET_ACCENTS[k % len(SET_ACCENTS)]
        targets = json.load(open(tgt))
        sft, sfm = lv.f0_trace(wav)
        SETS.append({
            "off": off, "accent": acc,
            "samp": lv.mono(wav), "ft": sft, "fm": sfm,
            "words": [{"t": off + t_["t"], "dur": max(t_["dur"], 0.2),
                       "stem_t": t_["t"], "midi": lv.to_midi(t_["note"]),
                       "label": t_["label"], "note": t_["note"]} for t_ in targets],
            "ribbon": [{"t": off + t_["t"], "t1": off + t_["t"] + t_["dur"],
                        "text": t_["label"], "accent": acc["HOT"]} for t_ in targets]})
for s in SETS:
    ribbon.extend(s["ribbon"])
ribbon.sort(key=lambda w: w["t"])

f_title, f_bar, f_word, f_note = lv.font(38), lv.font(30), lv.font(40), lv.font(20)
f_lyric, f_off, f_beat = lv.font(46), lv.font(20), lv.font(26)
ribbon = lv.ribbon_layout(ribbon, f_lyric, scroll.PXS)

ROLL_Y0, ROLL_Y1 = 140, 1010
LO, HI = 46, 74
ROWH = (ROLL_Y1 - ROLL_Y0) / (HI - LO + 1)
KICK_Y0, KICK_Y1 = 1030, 1090
WAVE_Y0, WAVE_Y1 = 1110, 1300
LYR_Y = 1330

LETTER_BAR = 16
def beat_label(b):
    bar, beat = b // 4, b % 4 + 1
    if bar >= LETTER_BAR and bar - LETTER_BAR < 26:
        return f"{chr(65 + bar - LETTER_BAR)}{beat}"
    return str(beat)

# ── the STRIP: everything time-invariant drawn once ───────────────────
T0S = START - PLAYHEAD_X / scroll.PXS
T1S = END + (W - PLAYHEAD_X) / scroll.PXS
STRIP_W = int(math.ceil((T1S - T0S) * scroll.PXS)) + 4
strip = Image.new("RGB", (STRIP_W, H), TH["CREAM"])
sd = ImageDraw.Draw(strip, "RGBA")
sx_of = lambda te: round((te - T0S) * scroll.PXS)
y_of = lambda midi: ROLL_Y1 - (midi - LO + 0.5) * ROWH
strip_scroll = lv.Scroll(playhead_x=0, px_per_beat=96, spb=SPB)

lv.piano_rows(sd, STRIP_W, sx_of, y_of, LO, HI, ROWH, TH, f_note, label_all=False)
lv.beat_columns(sd, max(0, T0S), sx_of, strip_scroll, STRIP_W, ROLL_Y0, ROLL_Y1,
                TOTAL_BEATS, TH, f_bar, hot_bars={16, 40, 56}, line_y1=KICK_Y1,
                beat_label=beat_label, f_beat=f_beat)
NEVER = -1e9
lv.blocks(sd, NEVER, sx_of, words, y_of, ROWH, STRIP_W, TH, f_word, f_off, voxsamp)
for s in SETS:
    lv.blocks(sd, NEVER, sx_of, s["words"], y_of, ROWH, STRIP_W, TH, f_word, f_off,
              s["samp"], accent=s["accent"])
lv.kick_floor(sd, sx_of, SPB, TOTAL_BEATS, KICK_Y0, KICK_Y1, TH, STRIP_W)
lv.bar_wave(sd, max(0, T0S), samples, strip_scroll, STRIP_W, WAVE_Y0, WAVE_Y1, TH,
            full_bright=True)
lv.lyric_ribbon(sd, NEVER, sx_of, ribbon, LYR_Y, TH, f_lyric, STRIP_W)
ALWAYS = 1e9
lv.pitch_trace(sd, ALWAYS, sx_of, ft, fm, y_of, TH, STRIP_W,
               offset=floordoc["passes"][0]["startSec"], lo=LO, hi=HI)
for p in floordoc["passes"][1:]:
    lv.pitch_trace(sd, ALWAYS, sx_of, ft, fm, y_of, TH, STRIP_W,
                   offset=p["startSec"], lo=LO, hi=HI)
for s in SETS:
    lv.pitch_trace(sd, ALWAYS, sx_of, s["ft"], s["fm"], y_of, TH, STRIP_W,
                   offset=s["off"], lo=LO, hi=HI, color=s["accent"]["TRACE"])
print(f"strip {STRIP_W}×{H} built")

# ── frames: crop + veil + actives + playhead ──────────────────────────
def draw_frame(t):
    xoff = int((t - T0S) * scroll.PXS) - PLAYHEAD_X
    frame = strip.crop((xoff, 0, xoff + W, H))
    d = ImageDraw.Draw(frame, "RGBA")
    x_of = scroll.at(t)
    d.rectangle([PLAYHEAD_X, 0, W, H], fill=(*TH["CREAM"], 64))   # the future, veiled
    act = [w for w in words if w["t"] <= t <= w["t"] + w["dur"]]
    if act:
        lv.blocks(d, t, x_of, act, y_of, ROWH, W, TH, f_word, f_off, voxsamp)
    for s in SETS:
        sact = [w for w in s["words"] if w["t"] <= t <= w["t"] + w["dur"]]
        if sact:
            lv.blocks(d, t, x_of, sact, y_of, ROWH, W, TH, f_word, f_off,
                      s["samp"], accent=s["accent"])
    ract = [w for w in ribbon if w["t"] <= t <= w["t1"]]
    if ract:
        lv.lyric_ribbon(d, t, x_of, ract, LYR_Y, TH, f_lyric, W)
    for m in range(LO, HI + 1):        # the note ladder rides the left edge
        ry = y_of(m)
        d.text((8, ry - ROWH / 2 + max(0, ROWH / 2 - 12)),
               lv.NAMES[m % 12] + str(m // 12 - 1), font=f_note,
               fill=(*TH["INK"], 190 if m % 12 == 0 else 80))
    lv.playhead(d, PLAYHEAD_X, ROLL_Y0 - 44, LYR_Y + 60, TH)
    d.text((36, 30), "imab · floor demo1 · lyric timing · 124", font=f_title, fill=TH["INK"])
    d.text((W - 320, 30), f"BAR {max(0, int(t / SPB / 4))}", font=f_title, fill=TH["PINK_HOT"])
    return frame

lv.render(os.path.join(OUT, "imab-floor-lyricscroll.mp4"), AUD, draw_frame,
          start=START, end=END, w=W, h=H, fps=FPS)
