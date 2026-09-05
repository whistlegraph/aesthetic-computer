#!/usr/bin/env python3
# setscroll.py — SOLO audit scroll for one vocal set: the tuned set
# wav alone (no floor bed masking it), its word blocks, f0 trace and
# ribbon under the needle. Born 2026-09-05 when the floor mix made it
# impossible to hear whether a block's sample really holds its word.
# Every mislabel or late boundary is undeniable here — dictate fixes
# into set-fixes/<take>.json by word name.
#
#   pop/.venv/bin/python pop/imab/bin/setscroll.py [take-id]
#     → pop/imab/out/imab-set-<take>-audit.mp4

import json, math, os, sys
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
OUT = os.path.join(LANE, "out")
sys.path.insert(0, os.path.join(os.path.dirname(LANE), "lib"))
import lyricvideo as lv

TAKE = sys.argv[1] if len(sys.argv) > 1 else "7427025009693986079"
WAV = os.path.join(OUT, f"imab-set-{TAKE}.wav")
TGT = os.path.join(OUT, f"imab-set-{TAKE}-targets.fitted.json")
if not os.path.exists(TGT):
    TGT = os.path.join(OUT, f"imab-set-{TAKE}-targets.json")
targets = json.load(open(TGT))

W, H = 1920, 1080
FPS = int(os.environ.get("FPS", "30"))
PLAYHEAD_X = 700
PXS = 640.0                      # slow-ish: 1 s ≈ 640 px, boundaries readable
TH = lv.theme()
ACC = {"HOT": (46, 172, 160), "BLOCK": (46, 172, 160, 52),
       "GLOW": (46, 172, 160, 72), "TRACE": (36, 150, 140)}

dur = lv.duration(WAV)
samples = lv.mono(WAV)
ft, fm = lv.f0_trace(WAV)

words = [{"t": t_["t"], "dur": max(t_["dur"], 0.12), "stem_t": t_["t"],
          "midi": lv.to_midi(t_["note"]), "label": t_["label"],
          "note": t_["note"]} for t_ in targets]
ribbon = [{"t": t_["t"], "t1": t_["t"] + t_["dur"], "text": t_["label"],
           "accent": ACC["HOT"]} for t_ in targets]

f_title, f_word, f_note = lv.font(38), lv.font(40), lv.font(22)
f_lyric, f_off = lv.font(52), lv.font(20)
ribbon = lv.ribbon_layout(ribbon, f_lyric, PXS)

ROLL_Y0, ROLL_Y1 = 130, 720
LO, HI = 44, 62                  # B2..D4 — this singer's set register
ROWH = (ROLL_Y1 - ROLL_Y0) / (HI - LO + 1)
WAVE_Y0, WAVE_Y1 = 750, 930
LYR_Y = 955

class Sc:                        # minimal scroll shim for the chassis
    def __init__(s): s.PXS, s.PLAYHEAD_X = PXS, PLAYHEAD_X
    def at(s, t): return lambda te: PLAYHEAD_X + (te - t) * PXS
scroll = Sc()

def y_of(midi): return ROLL_Y1 - (midi - LO + 0.5) * ROWH

def draw_frame(t):
    img = Image.new("RGB", (W, H), TH["CREAM"])
    d = ImageDraw.Draw(img, "RGBA")
    x_of = scroll.at(t)
    lv.piano_rows(d, W, x_of, y_of, LO, HI, ROWH, TH, f_note, label_all=True)
    # 100 ms ticks so boundary errors read in ms
    t0v = t - PLAYHEAD_X / PXS
    for k in range(int(t0v * 10) - 1, int((t0v + W / PXS) * 10) + 2):
        x = x_of(k / 10.0)
        big = k % 10 == 0
        d.line([x, ROLL_Y0 - (18 if big else 8), x, ROLL_Y0],
               fill=(*TH["INK"], 140 if big else 60), width=2 if big else 1)
        if big:
            d.text((x + 4, ROLL_Y0 - 44), f"{k // 10}s", font=f_note, fill=(*TH["INK"], 150))
    lv.blocks(d, t, x_of, words, y_of, ROWH, W, TH, f_word, f_off, samples, accent=ACC)
    lv.bar_wave(d, max(0.0, t0v), samples, scroll, W, WAVE_Y0, WAVE_Y1, TH, full_bright=True)
    lv.pitch_trace(d, t, x_of, ft, fm, y_of, TH, W, offset=0.0, lo=LO, hi=HI, color=ACC["TRACE"])
    lv.lyric_ribbon(d, t, x_of, ribbon, LYR_Y, TH, f_lyric, W, rowdy=52)
    lv.playhead(d, PLAYHEAD_X, ROLL_Y0 - 60, LYR_Y + 54, TH)
    d.text((36, 26), f"set {TAKE[:7]}… · SOLO AUDIT · tuned samples only",
           font=f_title, fill=TH["INK"])
    return img

MP4 = os.path.join(OUT, f"imab-set-{TAKE}-audit.mp4")
lv.render(MP4, WAV, draw_frame, start=0, end=dur, w=W, h=H, fps=FPS)
