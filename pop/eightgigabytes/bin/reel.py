#!/usr/bin/env python3
# reel.py — the 9:16 reel for eight gigabytes: the Spotify Canvas design
# (bin/canvas.py — three member-colored laptops in a zigzag column on cream
# paper, each screen holding its singer from Menu Band's own face rig, the
# notepat keys lighting the note being sung) opened up to the whole screen,
# run over any window of the record WITH the audio, and captioned: under
# each laptop, the line that member is singing, the sounding word in its
# color. Not a loop — no seam, a fade at the cut.
#
#   bash bin/faces.sh 30 960 600 <start> <end> -reel        # the face window first
#   pop/.venv/bin/python pop/eightgigabytes/bin/reel.py --start 0 --end 50.4 \
#       [--audio out/eightgigabytes-dance.wav] [--timeline out/timeline-dance.json] \
#       [--out out/eightgigabytes-reel.mp4] [--still N]
import math, os, subprocess, sys, json
import numpy as np
from PIL import Image, ImageDraw, ImageFilter, ImageFont

LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(LANE, "out")
sys.path.insert(0, os.path.join(LANE, "..", "lib"))
args = sys.argv[1:]
opt = lambda k, d=None: (args[args.index(k) + 1] if k in args else d)
TLP = opt("--timeline", os.path.join(OUT, "timeline-dance.json"))
AUDIO = opt("--audio", os.path.join(OUT, "eightgigabytes-dance.wav"))
TL = json.load(open(TLP)); PRE = TL["pre"]; BPM = TL["bpm"]; SPB = 60 / BPM
START = float(opt("--start", 0)); END = float(opt("--end", TL["seconds"]))
FACE_BASE = float(opt("--face-base", START))      # where faces-reel-*.mp4 begin, in record seconds
TAG = opt("--tag", "-reel")
MP4 = opt("--out", os.path.join(OUT, "eightgigabytes-reel.mp4"))
FADE = float(opt("--fade", 2.0 if END < TL["seconds"] - 0.5 else 0.0))   # audio+picture fade at a mid-song cut
W, H, FPS = 1080, 1920, 30
FRAMES = int(round((END - START) * FPS))
FW, FH = 960, 600

CREAM = (255, 253, 246); INK = (26, 26, 34)
COL = {"neo": (143, 209, 63), "blueberry": (90, 87, 211), "frisbee": (242, 167, 185)}
VOICE = {"neo": "Noelle", "blueberry": "Allison", "frisbee": "Junior"}
def font(size, bold=False): return ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", size, index=1 if bold else 0)
F_CAP, F_TITLE, F_SUB, F_LBL = font(40, True), font(54, True), font(28), font(24)

# ── notepat's keyboard, exactly as canvas.py has it ───────────────────
BASE = {"c": (255, 50, 50), "d": (255, 160, 0), "e": (255, 230, 0), "f": (50, 200, 50), "g": (50, 120, 255), "a": (130, 50, 200), "b": (180, 80, 255)}
DAYGLO = {"c": (255, 40, 80), "d": (255, 180, 0), "e": (255, 255, 50), "f": (50, 255, 100), "g": (50, 200, 255), "a": (180, 50, 255), "b": (255, 80, 255)}
MUTED = {"c": (139, 26, 26), "d": (180, 100, 0), "e": (180, 150, 0), "f": (20, 90, 20), "g": (20, 60, 120), "a": (50, 0, 90), "b": (90, 30, 150)}
BLACK = (22, 22, 28); OLIVE = (118, 104, 62); CREAM_KEY = (248, 240, 214); LILAC = (240, 236, 252); LILAC_RIM = (186, 176, 228)
KEY_NOTE = {"c": "c", "v": "c#", "d": "d", "s": "d#", "e": "e", "f": "f", "w": "f#", "g": "g", "r": "g#", "a": "a", "q": "a#", "b": "b",
            "h": "+c", "t": "+c#", "i": "+d", "y": "+d#", "j": "+e", "k": "+f", "u": "+f#", "l": "+g", "o": "+g#", "m": "+a", "p": "+a#", "n": "+b",
            ";": "++c", "'": "++c#", "]": "++d", "z": "-a#", "x": "-b"}
NOTE_KEY = {v: k for k, v in KEY_NOTE.items()}
ROWS = [(0.0, "qwertyuiop[]"), (0.5, "asdfghjkl;'"), (1.0, "zxcvbnm,./")]
EXT = {"z": OLIVE, "'": OLIVE, "x": CREAM_KEY, ";": CREAM_KEY, "]": CREAM_KEY}
OCT = {",", "."}; EMPTY = {"[", "/"}
PCS = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]

def mix(c, b, a): return tuple(int(x * (1 - a) + y * a) for x, y in zip(c, b))
def key_style(k):
    if k in EXT: return EXT[k], mix(EXT[k], INK, 0.35)
    if k in OCT: return LILAC, LILAC_RIM
    if k in EMPTY: return None, (*INK, 50)
    note = KEY_NOTE[k]; oct = note.count("+") - note.count("-"); name = note.strip("+-")
    pal = DAYGLO if oct >= 1 else MUTED if oct <= -1 else BASE
    if "#" in name: return BLACK, pal.get(name[0], INK)
    c = pal[name]; return c, mix(c, INK, 0.45)
def midi_key(midi):
    pc = PCS[midi % 12]
    for octv in (midi // 12 - 5, midi // 12 - 4, midi // 12 - 6):
        note = ("+" * octv if octv > 0 else "-" * -octv) + pc
        if note in NOTE_KEY: return NOTE_KEY[note]
    return None

# ── what each singer sings, from the timeline (the score video's reading) ──
notes_by = {m: [] for m in COL}; lines_by = {m: [] for m in COL}
for ln in TL["lines"]:
    if ln["role"] == "hum" or ln["m"] not in COL: continue
    pos, words, k = 0.0, [], 0
    syls = [(i, w) for i, w in enumerate(ln["w"].split()) for _ in w.split("-")]
    for tok in ln["n"].split():
        key, _, d = tok.partition(":"); d = float(d or 1)
        if key != "r":
            t = PRE + (ln["at"] + pos) * SPB
            notes_by[ln["m"]].append((t, t + d * SPB, int(key)))
            wi, word = syls[k]; k += 1
            if words and words[-1]["wi"] == wi: words[-1]["t1"] = t + d * SPB
            else: words.append({"wi": wi, "text": word.replace("-", ""), "t": t, "t1": t + d * SPB})
        pos += d
    if words: lines_by[ln["m"]].append({"t0": words[0]["t"], "t1": words[-1]["t1"], "words": words})
for m in COL: lines_by[m].sort(key=lambda l: l["t0"])
def sounding(m, t):
    for a, b, n in notes_by[m]:
        if a <= t < b: return n
    return None

# ── the faces, frame-locked from bin/faces.sh's window render ─────────
class FaceStream:
    def __init__(self, m):
        p = os.path.join(OUT, f"faces{TAG}-{m}.mp4")
        if not os.path.exists(p): raise SystemExit(f"missing {p} — run: bash bin/faces.sh 30 {FW} {FH} {START} {END} {TAG}")
        self.proc = subprocess.Popen(["ffmpeg", "-v", "error", "-ss", f"{max(0.0, START - FACE_BASE):.6f}", "-i", p, "-f", "rawvideo",
                                      "-pix_fmt", "rgb24", "-r", str(FPS), "-s", f"{FW}x{FH}", "-"], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        self.last = Image.new("RGB", (FW, FH), CREAM)
    def frame(self):
        b = self.proc.stdout.read(FW * FH * 3)
        if len(b) == FW * FH * 3: self.last = Image.frombytes("RGB", (FW, FH), b)
        return self.last

# ── the laptop chrome (cached per member × lit key) ───────────────────
_chrome = {}
def laptop(m, lid_w, press):
    key = (m, lid_w, press)
    if key in _chrome: return _chrome[key]
    color = COL[m]
    bezel = 18; sw = lid_w - 2 * bezel; sh = int(sw * FH / FW)
    hinge = 22; base_h = int(lid_w * 0.21); pad = 40
    LW, LH = lid_w + 2 * pad, bezel * 2 + sh + hinge + base_h + pad * 2
    img = Image.new("RGBA", (LW, LH), (0, 0, 0, 0))
    body = mix(color, (255, 255, 255), 0.55); dark = mix(color, INK, 0.35); edge = (*INK, 210)
    x0, y0 = pad, pad
    sm = Image.new("L", (LW, LH), 0)
    ImageDraw.Draw(sm).rounded_rectangle([x0 + 10, y0 + 18, x0 + lid_w + 10, y0 + bezel * 2 + sh + hinge + base_h + 14], radius=26, fill=70)
    img.paste(Image.new("RGBA", (LW, LH), (*dark, 255)), (0, 0), sm.filter(ImageFilter.GaussianBlur(14)))
    d = ImageDraw.Draw(img)
    d.rounded_rectangle([x0, y0, x0 + lid_w, y0 + bezel * 2 + sh], radius=22, fill=(*body, 255), outline=edge, width=4)
    hy = y0 + bezel * 2 + sh
    d.rounded_rectangle([x0 + 6, hy - 4, x0 + lid_w - 6, hy + hinge], radius=8, fill=(*mix(body, INK, 0.18), 255), outline=edge, width=3)
    d.rounded_rectangle([x0 + lid_w * 0.40, hy + 5, x0 + lid_w * 0.60, hy + hinge - 6], radius=4, fill=(*INK, 50))
    by = hy + hinge
    d.polygon([(x0 - 8, by), (x0 + lid_w + 8, by), (x0 + lid_w + 22, by + base_h), (x0 - 22, by + base_h)], fill=(*body, 255), outline=edge)
    d.line([(x0 - 22, by + base_h), (x0 + lid_w + 22, by + base_h)], fill=edge, width=4)
    unit = lid_w * 0.86 / 12.5; kw = unit * 0.84; kh = base_h * 0.17; gap = unit - kw
    kx0 = x0 + lid_w / 2 - lid_w * 0.86 / 2; ky = by + base_h * 0.09
    caps = [(k, [kx0 + (off + i) * unit, ky + r * (kh + gap), kx0 + (off + i) * unit + kw, ky + r * (kh + gap) + kh])
            for r, (off, keys) in enumerate(ROWS) for i, k in enumerate(keys)]
    lit = next((rect for k, rect in caps if k == press), None)
    if lit is not None:
        fill, rim = key_style(press); flash = rim if fill == BLACK else fill
        gm = Image.new("L", img.size, 0)
        ImageDraw.Draw(gm).rounded_rectangle([lit[0] - 12, lit[1] - 12, lit[2] + 12, lit[3] + 12], radius=8, fill=200)
        img.paste(Image.new("RGBA", img.size, (*mix(flash, (255, 255, 255), 0.4), 255)), (0, 0), gm.filter(ImageFilter.GaussianBlur(9)))
        d = ImageDraw.Draw(img)
    for k, rect in caps:
        fill, rim = key_style(k)
        if fill is None: d.rounded_rectangle(rect, radius=4, outline=rim, width=1); continue
        if k == press:
            flash = rim if fill == BLACK else fill
            fill = mix(flash, (255, 255, 255), 0.3); rim = mix(flash, INK, 0.3)
        d.rounded_rectangle(rect, radius=4, fill=(*fill, 255), outline=(*rim, 255), width=2)
    tp_w = lid_w * 0.34; tp_y = ky + 3 * (kh + gap) + base_h * 0.05
    d.rounded_rectangle([x0 + lid_w / 2 - tp_w / 2, tp_y, x0 + lid_w / 2 + tp_w / 2, by + base_h - 10], radius=8, outline=(*INK, 90), width=2)
    res = (img, (x0 + bezel, y0 + bezel, sw, sh), LH - 2 * pad)
    _chrome[key] = res
    return res

def rounded_mask(w, h, r):
    m = Image.new("L", (w, h), 0); ImageDraw.Draw(m).rounded_rectangle([0, 0, w - 1, h - 1], radius=r, fill=255)
    return m

# ── the page ─────────────────────────────────────────────────────────
grain = (np.random.default_rng(8).normal(0, 1, (H, W, 1)) * 3.2)
yv = np.linspace(0, 1, H, dtype=np.float32)[:, None, None]
def paper(beat):
    bar = (beat % 4) / 4
    tint = 0.5 - 0.5 * math.cos(2 * math.pi * bar)
    base = np.array(mix(CREAM, (252, 246, 232), 0.35 * tint), np.float32)
    img = base[None, None, :] * (1 - 0.03 * yv) + grain
    return Image.fromarray(np.clip(img, 0, 255).astype(np.uint8), "RGB").convert("RGBA")

# layout: three laptops down the page, a slight zigzag, neo (lead) in the middle and a touch larger;
# each one's caption sits just under its base.
ORDER = [("frisbee", 500, -90, 2.4, 0.5), ("neo", 560, 0, -1.4, 0.0), ("blueberry", 500, 90, 2.0, 0.25)]
SLOT_Y = [372, 962, 1548]
TITLE_Y = 72

def bob(beat, phase):
    p = (beat - phase) % 1.0
    return math.exp(-p * 4.2) * math.sin(p * math.pi * 2.2) * 14

def caption(d, m, t, cx, cy, maxw):
    cur = None
    for l in lines_by[m]:
        if l["t0"] - 0.4 <= t <= l["t1"] + 1.6: cur = l
    if cur is None: return
    fade = 1.0 if t <= cur["t1"] else max(0.0, 1 - (t - cur["t1"]) / 1.6)
    words = cur["words"]; gap = 12
    f = F_CAP
    widths = [f.getlength(w["text"]) for w in words]; total = sum(widths) + gap * (len(words) - 1)
    if total > maxw:
        f = font(max(22, int(40 * maxw / total)), True)
        widths = [f.getlength(w["text"]) for w in words]; total = sum(widths) + gap * (len(words) - 1)
    x = cx - total / 2
    for w, wd in zip(words, widths):
        active = w["t"] - 0.05 <= t <= w["t1"] + 0.05
        past = t > w["t1"]
        col = (*COL[m], int(255 * fade)) if active else (*INK, int((215 if past else 120) * fade))
        d.text((x, cy), w["text"], font=f, fill=col)
        x += wd + gap

def draw_frame(i):
    t = START + i / FPS; beat = (t - PRE) / SPB
    page = paper(beat)
    for n, (m, lid_w, dx, tilt, phase) in enumerate(ORDER):
        midi = sounding(m, t)
        chrome, (sx, sy, sw, sh), vis_h = laptop(m, lid_w, midi_key(midi) if midi is not None else None)
        layer = chrome.copy()
        layer.paste(FACES[m].frame().resize((sw, sh), Image.BILINEAR), (sx, sy), rounded_mask(sw, sh, 10))
        sway = tilt + 0.7 * math.sin(2 * math.pi * (t / 4.8) + n * 2.1)
        rot = layer.rotate(sway, resample=Image.BICUBIC, expand=True)
        cx = W / 2 + dx; cy = SLOT_Y[n] + bob(beat, phase)
        page.alpha_composite(rot, (int(cx - rot.width / 2), int(cy - rot.height / 2)))
        d = ImageDraw.Draw(page)
        lbl = f"{m} · {VOICE[m]}"; tw = F_LBL.getlength(lbl)
        singing = midi is not None
        d.ellipse([cx - tw / 2 - 26, cy + vis_h / 2 + 12, cx - tw / 2 - 10, cy + vis_h / 2 + 28], fill=(*COL[m], 255 if singing else 90))
        d.text((cx - tw / 2, cy + vis_h / 2 + 6), lbl, font=F_LBL, fill=(*INK, 230 if singing else 140))
        caption(d, m, t, cx, cy + vis_h / 2 + 44, lid_w + 160)
    d = ImageDraw.Draw(page)
    tw = F_TITLE.getlength("eight gigabytes")
    d.text((W / 2 - tw / 2, TITLE_Y - 30), "eight gigabytes", font=F_TITLE, fill=(*INK, 240))
    sub = "the MacNeoPolitan Trio · aesthetic.computer"; tw = F_SUB.getlength(sub)
    d.text((W / 2 - tw / 2, TITLE_Y + 36), sub, font=F_SUB, fill=(*INK, 150))
    out = page.convert("RGB")
    if FADE > 0 and t > END - FADE:                       # picture fades to the paper with the audio
        a = (t - (END - FADE)) / FADE
        out = Image.blend(out, Image.new("RGB", (W, H), CREAM), min(1.0, a))
    return out

if __name__ == "__main__":
    FACES = {m: FaceStream(m) for m in COL}
    if "--still" in args:
        f = int(opt("--still", 20)); draw_frame(f).save(os.path.join(OUT, f"reel-still-{f}.png")); print("still", f); sys.exit()
    af = f"afade=t=out:st={END - START - FADE:.3f}:d={FADE:.3f}" if FADE > 0 else "anull"
    enc = subprocess.Popen(["ffmpeg", "-y", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
                            "-ss", f"{START:.3f}", "-t", f"{END - START:.3f}", "-i", AUDIO,
                            "-map", "0:v", "-map", "1:a", "-af", af, "-shortest",
                            "-c:v", "libx264", "-preset", "slow", "-crf", "17", "-pix_fmt", "yuv420p",
                            "-c:a", "aac", "-b:a", "256k", "-ar", "48000", "-movflags", "+faststart", MP4], stdin=subprocess.PIPE)
    for i in range(FRAMES):
        enc.stdin.write(draw_frame(i).tobytes())
        if i % 150 == 0: print(f"  {i}/{FRAMES}", file=sys.stderr, flush=True)
    enc.stdin.close(); enc.wait()
    print(MP4)
