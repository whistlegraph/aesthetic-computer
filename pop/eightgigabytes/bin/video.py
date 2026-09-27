#!/usr/bin/env python3
# video.py — the mp4 for eight gigabytes, on the common lyric-video chassis
# (pop/lib/lyricvideo.py): a fixed playhead, the roll scrolling under it.
# Three voices, three colors (members/*/voice.json): neo citrus, blueberry
# indigo, frisbee blush. Every syllable is a block at its written note with
# the vocal's own envelope inside; the sung f0 of each member traces through
# its blocks; the lyric ribbon underneath keeps every word at its true time.
#
#   pop/.venv/bin/python pop/eightgigabytes/bin/video.py            # → out/eightgigabytes.mp4
#   pop/.venv/bin/python pop/eightgigabytes/bin/video.py --start 24 --end 44   # a window
import json, os, sys
os.environ.setdefault("SCORE_THEME", "light")   # jeffrey: render the score mp4s in light mode
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "lib"))
import numpy as np
from PIL import Image, ImageDraw
import lyricvideo as lv

LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(LANE, "out")
TL = json.load(open(os.path.join(OUT, "timeline.json")))
AUDIO = os.path.join(OUT, "eightgigabytes.wav")
args = sys.argv[1:]
opt = lambda k, d=None: (args[args.index(k) + 1] if k in args else d)
START, END = float(opt("--start", 0)), (float(opt("--end")) if opt("--end") else None)
FPS = int(opt("--fps", 30))
MP4 = opt("--out", os.path.join(OUT, "eightgigabytes.mp4" if END is None else f"eightgigabytes-{int(START)}-{int(END)}.mp4"))

W, MAIN_H = 1920, 1080
STRIP_H = 450                      # the band: three laptops side by side, faces singing
H = MAIN_H + STRIP_H
FACE_W, FACE_H = 480, 300
TH = lv.theme()
SPB = 60 / TL["bpm"]; PRE = TL["pre"]
COL = {"neo": (143, 209, 63), "blueberry": (90, 87, 211), "frisbee": (242, 167, 185)}
VOICE = {"neo": "Noelle", "blueberry": "Allison", "frisbee": "Junior"}
NAME = {"intro": "intro", "r1": "refrain", "v2": "V. the crash", "r2": "refrain",
        "bridge": "XI. the other machines", "r3": "last refrain", "outro": "outro"}

# ── the material ──────────────────────────────────────────────────────
blocks_by = {m: [] for m in COL}; ribbon = []
for ln in TL["lines"]:
    if ln["role"] == "hum": continue
    pos, syls = 0.0, []
    words = ln["w"].split()
    for w in words:
        for s in w.split("-"): syls.append((w, s))
    k = 0
    for tok in ln["n"].split():
        key, _, d = tok.partition(":"); d = float(d or 1)
        if key != "r":
            word, syl = syls[k]; k += 1
            t = PRE + (ln["at"] + pos) * SPB
            blocks_by[ln["m"]].append(dict(t=t, dur=d * SPB, midi=int(key), label=syl, stem_t=t, word=word))
        pos += d
# words for the ribbon: a hyphenated word spans its syllables
for m, bl in blocks_by.items():
    i = 0
    while i < len(bl):
        j = i
        while j + 1 < len(bl) and bl[j + 1]["word"] == bl[i]["word"] and bl[j + 1]["t"] - bl[j]["t"] < 1.5 and "-" in bl[i]["word"]: j += 1
        ribbon.append(dict(text=bl[i]["word"].replace("-", ""), t=bl[i]["t"], t1=bl[j]["t"] + bl[j]["dur"], accent=COL[m]))
        i = j + 1
ribbon.sort(key=lambda e: e["t"])
# the ribbon follows the lead: a harmony word that doubles neo's within a
# third of a second is the same word, not a second one
lead = [e for e in ribbon if e["accent"] == COL["neo"]]
ribbon = [e for e in ribbon if e["accent"] == COL["neo"] or not any(abs(e["t"] - l["t"]) < 0.3 and e["text"] == l["text"] for l in lead)]

# captions: each member's lines, with the time of every word in them
lines_by = {m: [] for m in COL}
for ln in TL["lines"]:
    if ln["role"] == "hum": continue
    pos, words, k = 0.0, [], 0
    syls = [(i, w) for i, w in enumerate(ln["w"].split()) for _ in w.split("-")]
    for tok in ln["n"].split():
        key, _, d = tok.partition(":"); d = float(d or 1)
        if key != "r":
            word_index, word = syls[k]; k += 1
            t = PRE + (ln["at"] + pos) * SPB
            if words and words[-1]["word_index"] == word_index:
                words[-1]["t1"] = t + d * SPB
            else:
                words.append({"word": word, "text": word.replace("-", ""), "t": t, "t1": t + d * SPB, "word_index": word_index})
        pos += d
    lines_by[ln["m"]].append({"t0": words[0]["t"], "t1": words[-1]["t1"], "words": words})
for m in lines_by: lines_by[m].sort(key=lambda l: l["t0"])
f_cap = lv.font(30)

vox8 = lv.mono(os.path.join(OUT, "stems", "vox.wav"), 8000)
mix8 = lv.mono(AUDIO, 8000)
traces = {}
for m in COL:
    p = os.path.join(OUT, "stems", f"vox-{m}.wav")
    if os.path.exists(p):
        print(f"  f0 {m} …", flush=True)
        traces[m] = lv.f0_trace(p, fmin=110, fmax=520)

# ── the faces: one video per member from bin/faces.sh, read frame-locked ──
import subprocess
class FaceStream:
    def __init__(self, path, start):
        self.ok = os.path.exists(path)
        if not self.ok: self.proc = None; return
        self.proc = subprocess.Popen(["ffmpeg", "-v", "error", "-ss", f"{start:.6f}", "-i", path, "-f", "rawvideo",
                                      "-pix_fmt", "rgb24", "-r", str(FPS), "-s", f"{FACE_W}x{FACE_H}", "-"], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        self.last = None
    def frame(self):
        if not self.ok: return None
        b = self.proc.stdout.read(FACE_W * FACE_H * 3)
        if len(b) == FACE_W * FACE_H * 3: self.last = Image.frombytes("RGB", (FACE_W, FACE_H), b)
        return self.last
faces = {m: FaceStream(os.path.join(OUT, f"faces-{m}.mp4"), START) for m in COL}

def laptop(d, x, y, w, color, name, voice, singing):
    """A MacBook seen head-on: lid, hinge, foreshortened base. Flat shapes,
    ink outlines — the book's turnaround style. Draws the chrome only and
    returns the screen rect; the face is pasted there after compositing."""
    bezel = 14; sw = w - 2 * 36; sh = int(sw * FACE_H / FACE_W)
    lid_x, lid_y = x + 36, y
    mix = 0.55 if TH["name"] == "light" else 0.35
    base = (255, 255, 255) if TH["name"] == "light" else (20, 20, 26)
    body = tuple(int(c * (1 - mix) + b * mix) for c, b in zip(color, base))   # the member's color, as anodized metal
    edge = (*TH["INK"], 200)
    d.rounded_rectangle([lid_x - bezel, lid_y - bezel, lid_x + sw + bezel, lid_y + sh + bezel], radius=18, fill=(*body, 255), outline=edge, width=3)
    d.rectangle([lid_x, lid_y, lid_x + sw, lid_y + sh], fill=(0, 0, 0, 0))          # the screen: a hole for the face
    by = lid_y + sh + bezel
    d.rounded_rectangle([x, by, x + w, by + 24], radius=8, fill=(*body, 255), outline=edge, width=3)
    d.rounded_rectangle([x + w * 0.38, by + 7, x + w * 0.62, by + 18], radius=4, fill=(*TH["INK"], 40))
    lbl = f"{name} · {voice}"; tw = f_small.getlength(lbl)
    d.ellipse([x + w / 2 - tw / 2 - 26, by + 36, x + w / 2 - tw / 2 - 10, by + 52], fill=(*color, 255 if singing else 90))
    d.text((x + w / 2 - tw / 2, by + 32), lbl, font=f_small, fill=(*TH["INK"], 230 if singing else 140))
    return (lid_x, lid_y, sw, sh, by + 70)

def caption(d, m, t, cx, cy, maxw):
    """What this member is singing: its current line (or the one just sung,
    fading), every word at ink, the word sounding now in the member's color."""
    cur = None
    for l in lines_by[m]:
        if l["t0"] - 0.4 <= t <= l["t1"] + 1.6: cur = l
    if cur is None: return
    fade = 1.0 if t <= cur["t1"] else max(0.0, 1 - (t - cur["t1"]) / 1.6)
    words = cur["words"]; gap = 11
    widths = [f_cap.getlength(w["text"]) for w in words]
    total = sum(widths) + gap * (len(words) - 1)
    scale = min(1.0, maxw / max(1, total))
    font = f_cap if scale == 1.0 else lv.font(max(18, int(30 * scale)))
    widths = [font.getlength(w["text"]) for w in words]; total = sum(widths) + gap * (len(words) - 1)
    x = cx - total / 2
    for w, wd in zip(words, widths):
        active = w["t"] - 0.05 <= t <= w["t1"] + 0.05
        past = t > w["t1"]
        col = (*COL[m], int(255 * fade)) if active else (*TH["INK"], int((210 if past else 120) * fade))
        d.text((x, cy), w["text"], font=font, fill=col)
        x += wd + gap

def singing_now(m, t):
    return any(b["t"] - 0.15 <= t <= b["t"] + b["dur"] + 0.1 for b in blocks_by[m])

# ── the frame ─────────────────────────────────────────────────────────
scroll = lv.Scroll(playhead_x=600, px_per_beat=92, spb=SPB)
LO, HI = 48, 67
ROLL_Y0, ROLL_Y1 = 190, 790
rowh = (ROLL_Y1 - ROLL_Y0) / (HI - LO + 1)
y_of = lambda midi: ROLL_Y1 - (midi - LO + 0.5) * rowh
WAVE_Y0, WAVE_Y1 = 812, 892
RIB_Y = 930
f_title, f_small, f_note, f_bar, f_word, f_lyric = lv.font(40), lv.font(22), lv.font(14), lv.font(20), lv.font(18), lv.font(34)
TOTAL_BEATS = TL["totalBeats"]
ribbon = lv.ribbon_layout(ribbon, f_lyric, scroll.PXS, gap=26, rows=2)
sections = TL["sections"]
def section_at(t):
    cur = sections[0]
    for s in sections:
        if t >= s["at"]: cur = s
    return cur
hot_bars = {int(round((s["at"] - PRE) / SPB / 4)) for s in sections if s["id"].startswith("r")}
beat_label = lambda b: NAME.get(next((s["id"] for s in reversed(sections) if b * SPB + PRE >= s["at"] - 1e-6), ""), "") if b % 4 == 0 and any(abs((s["at"] - PRE) / SPB - b) < 1e-6 for s in sections) else None
# beat 0 is at PRE seconds: shift the grid by drawing beats in track time
x_beat = lambda x_of: (lambda te: x_of(te + PRE))
total_s = TL["seconds"]

def draw_frame(t):
    im = Image.new("RGB", (W, H), TH["CREAM"])
    ov = Image.new("RGBA", (W, H), (0, 0, 0, 0)); d = ImageDraw.Draw(ov)
    x_of = scroll.at(t)
    xb = x_beat(x_of)
    lv.piano_rows(d, W, x_of, y_of, LO, HI, rowh, TH, f_note)
    lv.beat_columns(d, t - PRE, xb, scroll, W, ROLL_Y0, ROLL_Y1, TOTAL_BEATS, TH, f_bar, hot_bars=hot_bars,
                    beat_label=beat_label, f_beat=f_small)
    lv.kick_floor(d, xb, SPB, TOTAL_BEATS, WAVE_Y0, WAVE_Y1, TH, W)
    lv.bar_wave(d, t, mix8, scroll, W, WAVE_Y0, WAVE_Y1, TH)
    for m in ("blueberry", "frisbee", "neo"):
        c = COL[m]
        lv.blocks(d, t, x_of, blocks_by[m], y_of, rowh, W, TH, f_word, f_note, vox8,
                  accent={"HOT": c, "BLOCK": (*c, 60), "GLOW": (*c, 130)})
        if m in traces:
            ft, fm = traces[m]
            lv.pitch_trace(d, t, x_of, ft, fm, y_of, TH, W, lo=LO, hi=HI, color=c)
    lv.lyric_ribbon(d, t, x_of, ribbon, RIB_Y, TH, f_lyric, W, rowdy=56)
    # header: title, section, the band
    sec = section_at(t)
    d.text((32, 28), TL["title"], font=f_title, fill=(*TH["INK"], 255))
    d.text((32 + f_title.getlength(TL["title"]) + 24, 44), f"the ballad of neo · pop cut · {TL['key']} · {TL['bpm']} bpm", font=f_small, fill=(*TH["INK"], 150))
    d.text((32, 88), NAME.get(sec["id"], sec["id"]), font=f_small, fill=(*TH["PINK"], 255))
    x = W - 32
    for m in ("frisbee", "blueberry", "neo"):
        lbl = f"{m} · {VOICE[m]}"; w = f_small.getlength(lbl)
        x -= w; d.text((x, 44), lbl, font=f_small, fill=(*TH["INK"], 220))
        x -= 26; d.ellipse([x, 50, x + 16, 66], fill=(*COL[m], 255)); x -= 34
    # progress: one thin band, sections tinted, the playhead marker
    px0, px1 = 32, W - 32
    for i, s in enumerate(sections):
        s_end = sections[i + 1]["at"] if i + 1 < len(sections) else total_s
        a, b = px0 + (s["at"] / total_s) * (px1 - px0), px0 + (s_end / total_s) * (px1 - px0)
        d.rectangle([a, 128, b - 2, 136], fill=(*TH["PINK"], 150) if s["id"].startswith("r") else (*TH["INK"], 50))
    xm = px0 + (min(max(t, 0), total_s) / total_s) * (px1 - px0)
    d.rectangle([xm - 2, 122, xm + 2, 142], fill=(*TH["INK"], 240))
    lv.playhead(d, scroll.PLAYHEAD_X, ROLL_Y0 - 10, MAIN_H - 14, TH)
    # the band, on their laptops
    d.line([0, MAIN_H, W, MAIN_H], fill=(*TH["INK"], 40), width=2)
    lw = 470; gap = (W - 3 * lw) / 4
    fr = {m: faces[m].frame() for m in COL}
    screens = {}
    for i, m in enumerate(("frisbee", "neo", "blueberry")):
        x0 = int(gap + i * (lw + gap))
        screens[m] = laptop(d, x0, MAIN_H + 30, lw, COL[m], m, VOICE[m], singing_now(m, t))
        caption(d, m, t, x0 + lw / 2, screens[m][4], lw + gap - 24)
    im.paste(ov, (0, 0), ov)
    for m, (sx, sy, sw, sh, _cy) in screens.items():
        if fr[m] is not None: im.paste(fr[m].resize((sw, sh)), (sx, sy))
        else: ImageDraw.Draw(im).rectangle([sx, sy, sx + sw, sy + sh], fill=COL[m])
        ImageDraw.Draw(im).rectangle([sx, sy, sx + sw, sy + sh], outline=TH["INK"], width=2)
    return im

lv.render(MP4, AUDIO, draw_frame, start=START, end=END, w=W, h=H, fps=FPS, progress=300)
