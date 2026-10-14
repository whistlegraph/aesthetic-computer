#!/usr/bin/env python3
# canvas.py — the Spotify Canvas for eight gigabytes: the three singing
# faces, decorative. A 9:16 column of member-colored laptops on cream paper,
# each screen holding its singer from Menu Band's own face rig
# (bin/faces.sh renders those), looping the last refrain's hook —
# "I have eight gigabytes" — where all three sing. Silent, seamless,
# no text, no logos (Canvas rules). 2 bars at 100 BPM = 4.8 s.
#
#   bash bin/faces.sh 30 960 600 66.5 72.8 -canvas       # the face window, 2× res
#   pop/.venv/bin/python pop/eightgigabytes/bin/canvas.py  # → out/eightgigabytes-canvas.mp4
#
# Every motion has a period that divides the loop (beat 0.6 s, bar 2.4 s,
# loop 4.8 s), so the last frame flows into the first. The faces are the
# one thing that is not periodic: the loop's last 4 frames crossfade into
# the frames just before the window, where the singers are inhaling for the
# same line — the states match, the seam disappears.
import math, os, subprocess, sys
import numpy as np
from PIL import Image, ImageDraw, ImageFilter

LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(LANE, "out")
args = sys.argv[1:]
opt = lambda k, d=None: (args[args.index(k) + 1] if k in args else d)
MP4 = opt("--out", os.path.join(OUT, "eightgigabytes-canvas.mp4"))
W, H, FPS = 1080, 1920, 30
BPM = 100; SPB = 60 / BPM
LOOP = 8 * SPB                       # 2 bars, 4.8 s
FRAMES = int(round(LOOP * FPS))      # 144
FACE_BASE = float(opt("--face-base", 66.5))   # where faces-canvas-*.mp4 start, in record seconds
T0 = float(opt("--t0", 67.5))        # the loop's first frame: neo + blueberry about to sing "I have", frisbee 0.6 s behind — the same
                                     # footing they are on 4.8 s later, so the seam has nothing to hide (measured; 67.25 ghosted the eyes)
XF = int(opt("--xfade", 4))          # seam crossfade, frames (4 at 30 fps; longer doubles the mouths)
FW, FH = 960, 600                    # the face renders

CREAM = (255, 253, 246); INK = (26, 26, 34)
COL = {"neo": (143, 209, 63), "blueberry": (90, 87, 211), "frisbee": (242, 167, 185)}
# ── notepat's keyboard, exactly (disks/notepat.mjs + lib/note-colors.mjs) ──
# Each laptop's keys are the piece's: sharps black with the rim of the natural
# below them, base-octave naturals in the ROYGBIV palette, the upper octave in
# dayglo, the ext keys olive and cream, the octave keys lilac, two keys empty.
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

def key_style(k):
    """(fill, rim) for a keycap, the piece's way."""
    if k in EXT: return EXT[k], mix(EXT[k], INK, 0.35)
    if k in OCT: return LILAC, LILAC_RIM
    if k in EMPTY: return None, (*INK, 50)
    note = KEY_NOTE[k]; oct = note.count("+") - note.count("-"); name = note.strip("+-")
    if "#" in name:                       # a black key, rimmed in the natural below it
        nat = name[0]; pal = DAYGLO if oct >= 1 else MUTED if oct <= -1 else BASE
        return BLACK, pal.get(nat, INK)
    pal = DAYGLO if oct >= 1 else MUTED if oct <= -1 else BASE
    c = pal[name]; return c, mix(c, INK, 0.45)

def midi_key(midi):
    """The notepat key for a MIDI note; a note below the keyboard is shown an octave up."""
    pc = PCS[midi % 12]
    for octv in (midi // 12 - 5, midi // 12 - 4, midi // 12 - 6):     # as written, else up, else down
        note = ("+" * octv if octv > 0 else "-" * -octv) + pc
        if note in NOTE_KEY: return NOTE_KEY[note]
    return None

# ── what each singer sings, on the record's beat grid (stems/<m>/manifest.json) ──
import json
TL = json.load(open(os.path.join(OUT, "timeline.json"))); PRE = TL["pre"]
def read_notes(m):
    """[(start_s, end_s, midi)] for every sung note of this member."""
    out = []
    for l in json.load(open(os.path.join(OUT, "stems", m, "manifest.json")))["lines"]:
        beat = 0.0
        for tok in l["notes"].split(","):
            n, d = tok.split(":"); d = float(d)
            if n != "r": out.append((PRE + beat * SPB, PRE + (beat + d) * SPB, int(n)))
            beat += d
    return out
NOTES = {m: read_notes(m) for m in COL}
def sounding(m, t):
    for a, b, n in NOTES[m]:
        if a <= t < b: return n
    return None

# ── the faces: every frame of the window, in memory ───────────────────
def read_faces(m):
    p = os.path.join(OUT, f"faces-canvas-{m}.mp4")
    b = subprocess.run(["ffmpeg", "-v", "error", "-i", p, "-f", "rawvideo", "-pix_fmt", "rgb24", "-"],
                       capture_output=True, check=True).stdout
    n = len(b) // (FW * FH * 3)
    return np.frombuffer(b, np.uint8).reshape(n, FH, FW, 3)
FACES = {m: read_faces(m) for m in COL}
def face_at(m, t):
    i = int(round((t - FACE_BASE) * FPS)); a = FACES[m]
    return a[min(max(i, 0), len(a) - 1)]
def face_frame(m, i):
    """Frame i of the loop: the window's frame, crossfaded at the tail into
    the frames just before the window so frame FRAMES-1 flows into frame 0."""
    t = T0 + i / FPS
    f = face_at(m, t).astype(np.float32)
    k = i - (FRAMES - XF)
    if k >= 0:
        w = (k + 1) / (XF + 1)
        f = f * (1 - w) + face_at(m, t - LOOP).astype(np.float32) * w
    return Image.fromarray(f.astype(np.uint8))

# ── the laptop: lid, hinge, base with keycaps — flat shapes, ink lines ──
def mix(c, b, a): return tuple(int(x * (1 - a) + y * a) for x, y in zip(c, b))

def laptop(color, lid_w, beat, phase, press):
    """One member's MacBook as an RGBA layer, head-on. `beat` is the loop
    beat (float, 0..8); `press` the notepat key lit now (the note being sung)."""
    bezel = 18; sw = lid_w - 2 * bezel; sh = int(sw * FH / FW)
    hinge = 22; base_h = int(lid_w * 0.21); pad = 40
    LW, LH = lid_w + 2 * pad, bezel * 2 + sh + hinge + base_h + pad * 2
    img = Image.new("RGBA", (LW, LH), (0, 0, 0, 0)); d = ImageDraw.Draw(img)
    body = mix(color, (255, 255, 255), 0.55)       # anodized: the member's color, lightened
    dark = mix(color, INK, 0.35)
    edge = (*INK, 210)
    x0, y0 = pad, pad
    # shadow on the paper
    sm = Image.new("L", (LW, LH), 0)
    ImageDraw.Draw(sm).rounded_rectangle([x0 + 10, y0 + 18, x0 + lid_w + 10, y0 + bezel * 2 + sh + hinge + base_h + 14], radius=26, fill=70)
    img.paste(Image.new("RGBA", (LW, LH), (*dark, 255)), (0, 0), sm.filter(ImageFilter.GaussianBlur(14)))
    d = ImageDraw.Draw(img)
    # lid
    d.rounded_rectangle([x0, y0, x0 + lid_w, y0 + bezel * 2 + sh], radius=22, fill=(*body, 255), outline=edge, width=4)
    # hinge
    hy = y0 + bezel * 2 + sh
    d.rounded_rectangle([x0 + 6, hy - 4, x0 + lid_w - 6, hy + hinge], radius=8, fill=(*mix(body, INK, 0.18), 255), outline=edge, width=3)
    d.rounded_rectangle([x0 + lid_w * 0.40, hy + 5, x0 + lid_w * 0.60, hy + hinge - 6], radius=4, fill=(*INK, 50))
    # base: a little wider at the front, foreshortened
    by = hy + hinge
    d.polygon([(x0 - 8, by), (x0 + lid_w + 8, by), (x0 + lid_w + 22, by + base_h), (x0 - 22, by + base_h)], fill=(*body, 255), outline=edge)
    d.line([(x0 - 22, by + base_h), (x0 + lid_w + 22, by + base_h)], fill=edge, width=4)
    # keycaps: notepat's three rows, staggered like the piece's minimap.
    # The lit key's glow goes down first, under every cap, so it never tints its neighbours.
    unit = lid_w * 0.86 / 12.5; kw = unit * 0.84; kh = base_h * 0.17; gap = unit - kw
    kx0 = x0 + lid_w / 2 - lid_w * 0.86 / 2; ky = by + base_h * 0.09
    caps = []
    for r, (off, keys) in enumerate(ROWS):
        for k_i, k in enumerate(keys):
            x = kx0 + (off + k_i) * unit; y = ky + r * (kh + gap)
            caps.append((k, [x, y, x + kw, y + kh]))
    lit = next((rect for k, rect in caps if k == press), None)
    if lit is not None:
        fill, rim = key_style(press)
        flash = rim if fill == BLACK else fill                 # a black key flashes in its natural's color
        gm = Image.new("L", img.size, 0)
        ImageDraw.Draw(gm).rounded_rectangle([lit[0] - 12, lit[1] - 12, lit[2] + 12, lit[3] + 12], radius=8, fill=200)
        img.paste(Image.new("RGBA", img.size, (*mix(flash, (255, 255, 255), 0.4), 255)), (0, 0), gm.filter(ImageFilter.GaussianBlur(9)))
        d = ImageDraw.Draw(img)
    for k, rect in caps:
        fill, rim = key_style(k)
        if fill is None:                                       # an empty slot, dotted
            d.rounded_rectangle(rect, radius=4, outline=rim, width=1); continue
        if k == press:
            flash = rim if fill == BLACK else fill
            fill = mix(flash, (255, 255, 255), 0.3); rim = mix(flash, INK, 0.3)
        d.rounded_rectangle(rect, radius=4, fill=(*fill, 255), outline=(*rim, 255), width=2)
    # trackpad
    tp_w = lid_w * 0.34; tp_y = ky + 3 * (kh + gap) + base_h * 0.05
    d.rounded_rectangle([x0 + lid_w / 2 - tp_w / 2, tp_y, x0 + lid_w / 2 + tp_w / 2, by + base_h - 10], radius=8, outline=(*INK, 90), width=2)
    screen = (x0 + bezel, y0 + bezel, sw, sh)
    return img, screen

def rounded_mask(w, h, r):
    m = Image.new("L", (w, h), 0); ImageDraw.Draw(m).rounded_rectangle([0, 0, w - 1, h - 1], radius=r, fill=255)
    return m

# ── the page ─────────────────────────────────────────────────────────
grain = (np.random.default_rng(8).normal(0, 1, (H, W, 1)) * 3.2)
def paper(beat):
    """Cream paper, a warm breath of tint once a bar, fine grain."""
    bar = (beat % 4) / 4
    tint = 0.5 - 0.5 * math.cos(2 * math.pi * bar)
    base = np.array(mix(CREAM, (252, 246, 232), 0.35 * tint), np.float32)
    y = np.linspace(0, 1, H, dtype=np.float32)[:, None, None]
    img = base[None, None, :] * (1 - 0.03 * y) + grain
    return Image.fromarray(np.clip(img, 0, 255).astype(np.uint8), "RGB").convert("RGBA")

# layout: three laptops in a column, a slight zigzag, neo (lead) in the middle and a touch larger
ORDER = [("frisbee", 590, -96, 2.4, 0.5), ("neo", 650, 0, -1.4, 0.0), ("blueberry", 590, 96, 2.0, 0.25)]  # member, lid_w, x offset, tilt°, bob phase (beats)
SLOT_H = H / 3; SLOT_Y = [330, 960, 1590]

def bob(beat, phase):
    """A bounce on every beat, decaying — the kit's kick."""
    p = (beat - phase) % 1.0
    return math.exp(-p * 4.2) * math.sin(p * math.pi * 2.2) * 14

def draw_frame(i):
    t = i / FPS; beat = t / SPB
    page = paper(beat)
    for n, (m, lid_w, dx, tilt, phase) in enumerate(ORDER):
        midi = sounding(m, T0 + t)       # the key under this singer's finger: the note it sings now
        layer, (sx, sy, sw, sh) = laptop(COL[m], lid_w, beat, phase, midi_key(midi) if midi is not None else None)
        face = face_frame(m, i).resize((sw, sh), Image.LANCZOS)
        layer.paste(face, (sx, sy), rounded_mask(sw, sh, 10))
        sway = tilt + 0.7 * math.sin(2 * math.pi * (t / LOOP) + n * 2.1)
        rot = layer.rotate(sway, resample=Image.BICUBIC, expand=True)
        cx = W / 2 + dx; cy = SLOT_Y[n] + bob(beat, phase)
        page.alpha_composite(rot, (int(cx - rot.width / 2), int(cy - rot.height / 2)))
    return page.convert("RGB")

if __name__ == "__main__":
    if "--still" in args:
        f = int(opt("--still", 20)); draw_frame(f).save(os.path.join(OUT, f"canvas-still-{f}.png")); print("still", f); sys.exit()
    enc = subprocess.Popen(["ffmpeg", "-y", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
                            "-an", "-c:v", "libx264", "-preset", "slow", "-crf", "17", "-pix_fmt", "yuv420p", "-movflags", "+faststart", MP4], stdin=subprocess.PIPE)
    for i in range(FRAMES):
        enc.stdin.write(draw_frame(i).tobytes())
        if i % 24 == 0: print(f"  {i}/{FRAMES}", file=sys.stderr)
    enc.stdin.close(); enc.wait()
    print(MP4)
