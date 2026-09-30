#!/usr/bin/env python3
# lyric-video.py — the singalong MP4 for the sailor-song remix: cover
# blurred and darkened behind, a small square of it up top, the current
# line big and centred with a karaoke wipe through the word being sung,
# the next line smaller underneath. Frames are PIL (this ffmpeg has no
# drawtext / subtitles filter), piped raw into x264, then muxed with the
# master.
#
# Everything lyrical stays under src/ (gitignored): the lyric as she
# sings it (src/lyrics-sung.txt) and the aligned word times
# (src/words-aligned.json, whisper boundaries in TAKE time with the true
# words substituted in by pop/bin/mfa-align.mjs). The take is not the
# record: record_t = reg(lock(t)) − startSec, where lock and reg are the
# piecewise-linear sample maps under src/vox/{locked,reg}/timemap.txt
# and startSec is the record's open in out/sailor-song-v7.events.json.
# The bake can move startSec (and the maps) while this draws, so the
# receipt is read again after the render and the frames redrawn if it
# moved; a master that only changed bytes is re-muxed.
#
#   pop/.venv/bin/python pop/sailor-song/bin/lyric-video.py
#     [--master out/sailor-song-v7-master.wav] [--out out/sailor-song-v7-lyrics.mp4]
#     [--fps 30] [--mux-only]   # re-mux the last silent render with the master
#     [--stills 5,40,70,120]    # also write PNG frames at those record seconds

import argparse, json, math, os, re, subprocess, sys, time
import numpy as np
from PIL import Image, ImageChops, ImageDraw, ImageFilter, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
SRC = os.path.join(LANE, "src")
OUTD = os.path.join(LANE, "out")

ap = argparse.ArgumentParser()
import glob as _glob
_newest = sorted(_glob.glob(os.path.join(OUTD, "sailor-song-v*-master.wav")), key=os.path.getmtime)
_V = os.path.basename(_newest[-1]).split("-")[2] if _newest else "v7"     # newest version's tag, e.g. v9
ap.add_argument("--master", default=os.path.join(OUTD, f"sailor-song-{_V}-master.wav"))
ap.add_argument("--events", default=os.path.join(OUTD, f"sailor-song-{_V}.events.json"))
ap.add_argument("--words", default=os.path.join(SRC, "words-aligned.json"))
ap.add_argument("--lyrics", default=os.path.join(SRC, "lyrics-sung.txt"))
ap.add_argument("--vocal", default=os.path.join(SRC, "vox", "vocals-dry-48k.wav"))
ap.add_argument("--cover", default=os.path.join(LANE, "cover", "sailor-song-cover.jpg"))
ap.add_argument("--out", default=os.path.join(OUTD, f"sailor-song-{_V}-lyrics.mp4"))
ap.add_argument("--silent", default=None, help="video-only intermediate (default: beside --out, dot-prefixed)")
ap.add_argument("--fps", type=int, default=30)
ap.add_argument("--mux-only", action="store_true")
ap.add_argument("--stills", default=None, help="comma-separated record seconds → PNGs")
ap.add_argument("--stills-dir", default=None)
ap.add_argument("--no-render", action="store_true", help="stills / timing check only")
A = ap.parse_args()

W, H = 1920, 1080
FPS = A.fps
SILENT = A.silent or os.path.join(os.path.dirname(A.out), "." + os.path.basename(A.out).replace(".mp4", ".video.mp4"))

# ── palette (warm, off the cover: cream text on a dark warm blur) ─────
CREAM = (246, 234, 214)
DIM = 0.45             # unsung words of the current line
HOT = (255, 200, 118)  # the word being sung (the lamp in the cover)
NEXT_ALPHA = 0.52
LEAD = 1.0             # s before the first word a line starts fading in
FADE = 0.5             # s fade in / out
HOLD = 2.2             # s a line lingers after its last word when nothing follows

def font(size, name="Semibold"):
    f = ImageFont.truetype("/System/Library/Fonts/NewYork.ttf", size)
    try: f.set_variation_by_name(name)
    except Exception: pass
    return f

# ── time base: take → locked → regularized → record ───────────────────
LOCK_MAP = os.path.join(SRC, "vox", "locked", "timemap.txt")
REG_MAP = os.path.join(SRC, "vox", "reg", "timemap.txt")

def timemap(path):
    a = np.loadtxt(path)
    return a[:, 0] / 48000.0, a[:, 1] / 48000.0

def clock_state():
    """What the time base depends on — compared after the render."""
    return (float(json.load(open(A.events))["startSec"]),
            os.path.getmtime(LOCK_MAP), os.path.getmtime(REG_MAP))

# ── the vocal's envelope in take time, to firm up line onsets ────────
ENV_SR = 100
def vocal_env():
    r = subprocess.run(["ffmpeg", "-v", "error", "-i", A.vocal, "-ac", "1", "-ar", "8000",
                        "-f", "f32le", "-"], capture_output=True)
    y = np.frombuffer(r.stdout, np.float32)
    hop = 8000 // ENV_SR
    n = len(y) // hop
    rms = np.sqrt((y[:n * hop].reshape(n, hop) ** 2).mean(axis=1))
    return rms / max(1e-9, np.percentile(rms, 99))

# ── the lines, each carrying its words ────────────────────────────────
HEADER_RE = re.compile(r"^\[?(hook|verse \d+|outro|bridge|chorus|intro)\]?$", re.I)
norm = lambda s: re.sub(r"[^a-z0-9']", "", s.lower())

def load_words():
    words = json.load(open(A.words))
    # zero-length words come out of the aligner's gap interpolation when
    # the neighbours touch — spread the neighbours' span evenly over the run
    i = 0
    while i < len(words):
        if words[i]["toMs"] > words[i]["fromMs"]:
            i += 1; continue
        j = i
        while j < len(words) and words[j]["toMs"] <= words[j]["fromMs"]: j += 1
        lo = max(0, i - 1); hi = min(len(words) - 1, j)
        t0, t1 = words[lo]["fromMs"], words[hi]["toMs"]
        n = hi - lo + 1
        for k in range(lo, hi + 1):
            words[k]["fromMs"] = round(t0 + (t1 - t0) * (k - lo) / n)
            words[k]["toMs"] = round(t0 + (t1 - t0) * (k - lo + 1) / n)
        i = j
    return words

def build_lines():
    RAW = os.environ.get("SAILOR_CLOCK") == "raw"
    LS, LD = ((np.array([0.0, 1e4]), np.array([0.0, 1e4])) if RAW else timemap(LOCK_MAP)); RS, RD = ((np.array([0.0, 1e4]), np.array([0.0, 1e4])) if RAW else timemap(REG_MAP))
    start = float(json.load(open(A.events))["startSec"])
    segmap = os.path.join(SRC, "vox", "cut", "segmap.txt")
    segs = [tuple(map(float, l.split())) for l in open(segmap).read().strip().split("\n")] if os.path.exists(segmap) and not RAW else []
    def rec(s):
        t = float(np.interp(np.interp(s, LS, LD), RS, RD))
        if not segs: return t - start
        for a0, b0, o in segs:
            if a0 <= t < b0: return t - a0 + o - start
        return None                           # outside the arrangement
    env = vocal_env()
    words = load_words()
    lines_txt = [l.strip() for l in open(A.lyrics, encoding="utf8")]
    lines_txt = [l for l in lines_txt if l and not HEADER_RE.match(l)]
    lines = []; wi = 0
    for text in lines_txt:
        toks = [t for t in text.split() if norm(t)]
        ws = []
        for tok in toks:
            w = words[wi]; wi += 1
            assert norm(w["text"]) == norm(tok), f"lyric/word drift at {tok!r} vs {w['text']!r}"
            ws.append(dict(text=tok, a=w["fromMs"] / 1000, b=w["toMs"] / 1000))
        # whisper's boundary after a rest sits early: where a word starts in
        # silence, walk its onset forward to where her voice comes in (never
        # back, never past its own end), and let a squeezed word push the next
        prev_b = lines[-1]["words"][-1]["b"] if lines else 0.0
        for w in ws:
            w["a"] = max(w["a"], prev_b)
            i0 = int(w["a"] * ENV_SR); i1 = int((w["a"] + 0.5) * ENV_SR)
            if env[i0] >= 0.02:   # in voice already: only a breath within 250 ms counts as a rest
                d = next((i for i in range(i0, i0 + 25) if env[i] < 0.02), None)
                i0, i1 = (d, int(min(w["b"] - 0.05, w["a"] + 0.5) * ENV_SR)) if d else (None, None)
            if i0 is not None:
                for i in range(i0, max(i0, i1)):
                    if env[i] > 0.05: w["a"] = i / ENV_SR; break
            w["b"] = max(w["b"], w["a"] + 0.08); prev_b = w["b"]
        kept = []
        for w in ws:
            r0, r1 = rec(w["a"]), rec(w["b"])
            if r0 is None: continue           # the word is in the edit
            w["t0"] = max(0.0, r0); w["t1"] = max(w["t0"] + 0.02, r1 if r1 is not None else w["t0"] + 0.06); kept.append(w)
        if not kept: continue                 # the whole line is in the edit
        lines.append(dict(text=" ".join(w["text"] for w in kept), words=kept, t0=kept[0]["t0"], t1=kept[-1]["t1"]))
    assert wi == len(words), f"{len(words) - wi} aligned words left over"
    lines.sort(key=lambda L: L["t0"])         # record order: the arrangement may reorder verses
    # display windows: fade in LEAD before the first word; out when the next
    # line takes over, or HOLD after the last word if the gap is long
    # (a line that follows closely waits in the small slot underneath and takes
    # the big slot only once this line's last word is sung — never earlier)
    for k, L in enumerate(lines):
        L["on"] = L["t0"] - LEAD if k == 0 or not lines[k - 1]["cut"] else lines[k - 1]["off"]
        if k + 1 < len(lines):
            N = lines[k + 1]
            L["cut"] = N["t0"] - LEAD <= L["t1"] + HOLD
            L["off"] = min(N["t0"], max(N["t0"] - LEAD, L["t1"] - 0.3)) if L["cut"] else L["t1"] + HOLD
        else:
            L["cut"] = False; L["off"] = L["t1"] + HOLD
    # the record-time word list, for checking against the vocal (local)
    json.dump([dict(text=w["text"], fromMs=round(w["t0"] * 1000), toMs=round(w["t1"] * 1000))
               for L in lines for w in L["words"]],
              open(os.path.join(SRC, "words-record-lyricvideo.json"), "w"), indent=1)
    return lines, start, len(words)

# ── background: the cover, blurred and darkened, a small square of it ─
def build_bg():
    cov = Image.open(A.cover).convert("RGB")
    s = W / cov.width
    big = cov.resize((W, round(cov.height * s)), Image.LANCZOS)
    top = (big.height - H) // 2
    bg = big.crop((0, top, W, top + H)).filter(ImageFilter.GaussianBlur(42))
    arr = np.asarray(bg).astype(np.float32)
    gray = arr.mean(axis=2, keepdims=True)
    arr = arr * 0.72 + gray * 0.28                     # soften the colour
    arr *= 0.36                                        # darken
    arr *= np.array([1.06, 0.98, 0.9], np.float32)     # warm
    yy, xx = np.mgrid[0:H, 0:W]
    r = np.sqrt(((xx - W / 2) / (W / 2)) ** 2 + ((yy - H / 2) / (H / 2)) ** 2)
    vig = np.clip(1.0 - 0.55 * np.clip(r - 0.35, 0, 1) ** 1.6, 0, 1)
    arr *= vig[..., None]
    bg = Image.fromarray(np.clip(arr, 0, 255).astype(np.uint8))
    # the square: soft shadow, rounded corners
    S = 300; x0, y0 = (W - S) // 2, 118
    sq = cov.resize((S, S), Image.LANCZOS)
    mask = Image.new("L", (S, S), 0)
    ImageDraw.Draw(mask).rounded_rectangle([0, 0, S - 1, S - 1], radius=22, fill=255)
    shadow = Image.new("L", (S + 160, S + 160), 0)
    ImageDraw.Draw(shadow).rounded_rectangle([80, 88, 80 + S, 88 + S], radius=22, fill=150)
    shadow = shadow.filter(ImageFilter.GaussianBlur(28))
    bg.paste((8, 5, 4), (x0 - 80, y0 - 80), shadow)
    bg.paste(sq, (x0, y0), mask)
    return bg

# ── text layers, cached per line ──────────────────────────────────────
F_CUR_MAX, F_NEXT = 86, 46
MAXW = W - 200
PAD = 40
_meas = ImageDraw.Draw(Image.new("L", (4, 4)))

def fit_font(text, size):
    f = font(size)
    while _meas.textlength(text, font=f) > MAXW and size > 40:
        size -= 2; f = font(size)
    return f

def render_text(text, f, color):
    bbox = _meas.textbbox((0, 0), text, font=f)
    w, h = bbox[2] - bbox[0], bbox[3] - bbox[1]
    im = Image.new("RGBA", (w + PAD * 2, h + PAD * 2), (0, 0, 0, 0))
    ImageDraw.Draw(im).text((PAD - bbox[0], PAD - bbox[1]), text, font=f, fill=(*color, 255))
    return im, bbox

def with_alpha(layer, k):
    out = layer.copy(); out.putalpha(layer.split()[3].point(lambda v: int(v * k))); return out

def glow_of(layer):
    g = Image.new("RGBA", layer.size, (0, 0, 0, 0))
    g.paste((10, 6, 4, 210), (0, 0), layer.split()[3])
    return g.filter(ImageFilter.GaussianBlur(14))

CACHE = {}
def line_layers(k):
    if k in CACHE: return CACHE[k]
    L = LINES[k]
    f = fit_font(L["text"], F_CUR_MAX)
    lit, bbox = render_text(L["text"], f, CREAM)
    hot, _ = render_text(L["text"], f, HOT)
    # x of each word's start/end inside the layer, from prefix widths
    xs = []; pos = 0; text = L["text"]
    for w in L["words"]:
        j = text.index(w["text"], pos)
        x0 = PAD - bbox[0] + _meas.textlength(text[:j], font=f)
        x1 = PAD - bbox[0] + _meas.textlength(text[:j + len(w["text"])], font=f)
        xs.append((x0, x1)); pos = j + len(w["text"])
    nxt, _ = render_text(L["text"], fit_font(L["text"], F_NEXT), CREAM)
    CACHE[k] = dict(dim=with_alpha(lit, DIM), lit=lit, hot=hot, glow=glow_of(lit), xs=xs,
                    nxt=with_alpha(nxt, NEXT_ALPHA), nglow=glow_of(nxt))
    return CACHE[k]

def wipe_x(L, xs, t):
    """(x where the sung-cream stops, x where the gold wipe stops)."""
    cur = -1
    for i, w in enumerate(L["words"]):
        if t >= w["t0"]: cur = i
    if cur < 0: return 0, 0
    w = L["words"][cur]
    p = min(1.0, (t - w["t0"]) / (w["t1"] - w["t0"]))   # gold stays on through the gap after a word
    x0, x1 = xs[cur]
    return x0, x0 + (x1 - x0) * p

def paste_fade(frame, layer, xy, fade, mask=None):
    if fade <= 0: return
    a = layer.split()[3]
    if mask is not None: a = ImageChops.multiply(a, mask)
    if fade < 1: a = a.point(lambda v: int(v * fade))
    frame.paste(layer, xy, a)

Y_CUR, Y_NEXT = 640, 790

def draw_frame(t, bg):
    frame = bg.copy()
    cur = next((k for k, L in enumerate(LINES) if L["on"] <= t < L["off"]), None)
    if cur is None:
        # a rest: the upcoming line waits small, underneath, until its own fade-in
        nk = next((k for k, L in enumerate(LINES) if L["on"] > t), None)
        if nk is None: return frame
        gap0 = LINES[nk - 1]["off"] + 0.4 if nk else -1.0
        fade = min(1.0, (t - gap0) / FADE, (LINES[nk]["on"] - t) / FADE)
        if fade > 0:
            ln = line_layers(nk); nw, nh = ln["nxt"].size
            nx, ny = (W - nw) // 2, Y_NEXT - nh // 2
            paste_fade(frame, ln["nglow"], (nx, ny), fade * 0.8)
            paste_fade(frame, ln["nxt"], (nx, ny), fade)
        return frame
    L = LINES[cur]; ly = line_layers(cur)
    fade = min(1.0, (t - L["on"]) / (FADE if cur == 0 or not LINES[cur - 1]["cut"] else FADE * 0.5))
    if not L["cut"]:
        fade = min(fade, max(0.0, (L["off"] - t) / FADE))
    # current line: glow, dim base, cream up to the current word, gold wipe through it
    lw, lh = ly["dim"].size
    x, y = (W - lw) // 2, Y_CUR - lh // 2
    paste_fade(frame, ly["glow"], (x, y), fade)
    paste_fade(frame, ly["dim"], (x, y), fade)
    xa, xb = wipe_x(L, ly["xs"], t)
    if xa > 0:
        m = Image.new("L", (lw, lh), 0); ImageDraw.Draw(m).rectangle([0, 0, int(xa), lh], fill=255)
        paste_fade(frame, ly["lit"], (x, y), fade, m)
    if xb > xa:
        m = Image.new("L", (lw, lh), 0); ImageDraw.Draw(m).rectangle([int(xa), 0, int(xb), lh], fill=255)
        paste_fade(frame, ly["hot"], (x, y), fade, m)
    # the next line, small, underneath
    if cur + 1 < len(LINES):
        ln = line_layers(cur + 1)
        nw, nh = ln["nxt"].size
        nx, ny = (W - nw) // 2, Y_NEXT - nh // 2
        paste_fade(frame, ln["nglow"], (nx, ny), fade * 0.8)
        paste_fade(frame, ln["nxt"], (nx, ny), fade)
    return frame

# ── ffmpeg ────────────────────────────────────────────────────────────
def duration(path):
    return float(subprocess.run(["ffprobe", "-v", "error", "-show_entries", "format=duration",
                                 "-of", "csv=p=0", path], capture_output=True, text=True).stdout.strip())

def mux(silent, master, out):
    subprocess.run(["ffmpeg", "-hide_banner", "-loglevel", "error", "-y",
                    "-i", silent, "-i", master, "-map", "0:v", "-map", "1:a",
                    "-c:v", "copy", "-c:a", "aac", "-b:a", "320k",
                    "-movflags", "+faststart", out], check=True)
    print(f"✓ {out}  ({duration(out):.3f} s · master {duration(master):.3f} s)")

def render(silent, dur, bg):
    frames = math.ceil(dur * FPS)
    proc = subprocess.Popen(["ffmpeg", "-hide_banner", "-loglevel", "error", "-y",
        "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
        "-an", "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
        "-r", str(FPS), silent], stdin=subprocess.PIPE)
    t_start = time.time()
    for i in range(frames):
        t = (i + 0.5) / FPS
        proc.stdin.write(draw_frame(t, bg).tobytes())
        if i % 600 == 0:
            print(f"  {i}/{frames}  {time.time() - t_start:.0f}s", flush=True)
    proc.stdin.close(); proc.wait()
    if proc.returncode: sys.exit("✗ x264 failed")
    print(f"✓ {silent} ({frames} frames, {time.time() - t_start:.0f}s)")

if __name__ == "__main__":
    bg = build_bg()
    for attempt in range(3):
        clock0 = clock_state()
        master_mtime = os.path.getmtime(A.master)
        dur = duration(A.master)
        LINES, START, nwords = build_lines(); CACHE.clear()
        print(f"master {A.master} · {dur:.3f} s · startSec {START} · {len(LINES)} lines / {nwords} words")
        print(f"first word at {LINES[0]['t0']:.2f} s record · last word ends {LINES[-1]['t1']:.2f} s")
        if A.stills:
            d = A.stills_dir or os.path.dirname(A.out); os.makedirs(d, exist_ok=True)
            for s in A.stills.split(","):
                p = os.path.join(d, f"lyrics-still-{float(s):05.1f}s.png")
                draw_frame(float(s), bg).save(p); print(f"  still {p}")
        if A.no_render: break
        if not A.mux_only:
            render(SILENT, dur, bg)
        mux(SILENT, A.master, A.out)
        if clock_state() != clock0:
            print("! the time base moved during the render — drawing again")
            A.mux_only = False; continue
        if os.path.getmtime(A.master) != master_mtime:
            print("! the master changed during the render — re-muxing")
            mux(SILENT, A.master, A.out)
        break
