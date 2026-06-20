#!/usr/bin/env python3
"""
Restless Egg — Batch 2 intro video. ONE-SHOT VO pipeline.

Modeled on the LACMA Art+Technology Lab grant video (grants/lacma-2026/
build-video-v6.py): a single jeffrey-pvc ElevenLabs call produces the whole
narration plus char-level timestamps; the build splits sentence boundaries
from that one stream to time the cards and burn captions. Painterly stills get
a slow Ken Burns push/pan (brand-brief.md — "no generative video"), hard cuts
between beats, VO + burned SRT + a notepat QR in the corner.

Flow:
  1. vo()  — one TTS call → vo-full.wav + vo-full.json (char alignment)
  2. tokenize alignment → words + sentence boundaries
  3. card segments sized to their matching sentences (Ken Burns on stills)
  4. concat → burn SRT + overlay QR + VO
"""

import json, subprocess, base64, shutil
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

HERE = Path(__file__).parent
OUT = HERE / "restless-egg-video.mp4"
CARDS = HERE / "video-cards"
CARDS.mkdir(exist_ok=True)

VAULT_ENV = "/Users/jas/aesthetic-computer/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env"
VOICE_ID = "ZXoQQp5X0PKHGwyZpVIT"  # @jeffrey PVC
MODEL_ID = "eleven_multilingual_v2"

W, H = 1280, 720  # cinematic 16:9
# Aesthetic Computer palette (brand-brief: warm cream, deep violet, citrus
# terminal glow, coral accents).
CREAM = (240, 230, 207); DARK = (40, 28, 46); CORAL = (224, 110, 96)
VIOLET = (150, 110, 196); GREEN = (150, 214, 120)
RAINBOW = [CORAL, VIOLET, GREEN]  # per-character title palette for brand moments

YWFT_BOLD = str(Path.home() / "Library" / "Fonts" / "ywft-processing-bold.ttf")
YWFT_REG = "/Users/jas/aesthetic-computer/system/public/type/webfonts/ywft-processing-regular.ttf"
# LACMA-style captions: plain Arial (not YWFT) for clean, legible subtitles.
ARIAL = "/System/Library/Fonts/Supplemental/Arial.ttf"
ARIAL_BOLD = "/System/Library/Fonts/Supplemental/Arial Bold.ttf"
FALLBACK = "/System/Library/Fonts/SFNS.ttf"

QR = HERE / "qr-notepat.png"
QR_URL = "https://notepat.com"


def is_video(p):
    return str(p).lower().endswith((".mp4", ".mov", ".webm"))


# ========================================================================
# 1. ONE-SHOT VO — the canonical 60-second script from FORM-ANSWERS.md,
#    rewritten as flowing prose so delivery reads natural.
# ========================================================================
SCRIPT = (
    "Hi, I'm Jeffrey Scudder — a painter who spent the last decade building "
    "instruments instead of paintings. "
    "This is notepat. It's a software instrument: you play it with a keyboard, "
    "a touchscreen, MIDI, even pressure. "
    "Underneath is a hundred-and-twenty-eight-voice synthesizer I built from "
    "scratch, that models real instruments instead of playing back samples. "
    "And notepat runs anywhere — in your browser, on your phone, and as the very "
    "first thing that boots on a salvaged fifty-dollar laptop, with no operating "
    "system underneath it. "
    "It grows out of Aesthetic Computer, a platform where about eighteen thousand "
    "people have already made sixteen thousand little programs. "
    "My company, Aesthetic Inc., turns that into instruments people play, and pay "
    "for. "
    "I want to build the studio that makes computing something you play again. "
    "That's what I'd do with Restless Egg."
)


def load_key():
    for l in Path(VAULT_ENV).read_text().split("\n"):
        if l.startswith("ELEVENLABS_API_KEY="):
            return l.split("=", 1)[1].strip()
    raise RuntimeError("ELEVENLABS_API_KEY not found in vault env")


def generate_vo(force=False):
    wav = HERE / "vo-full.wav"
    js = HERE / "vo-full.json"
    if wav.exists() and js.exists() and not force:
        print(f"→ using cached {wav.name}")
        return wav, json.loads(js.read_text())

    import urllib.request
    key = load_key()
    url = (f"https://api.elevenlabs.io/v1/text-to-speech/{VOICE_ID}"
           f"/with-timestamps?output_format=mp3_44100_128")
    body = json.dumps({
        "text": SCRIPT,
        "model_id": MODEL_ID,
        "voice_settings": {
            "stability": 0.65, "similarity_boost": 0.9,
            "style": 0.15, "use_speaker_boost": True,
        },
    }).encode("utf-8")
    req = urllib.request.Request(url, data=body, method="POST",
        headers={"xi-api-key": key, "Content-Type": "application/json"})
    print(f"→ one-shot VO request ({len(SCRIPT)} chars, {MODEL_ID})")
    with urllib.request.urlopen(req) as r:
        data = json.loads(r.read().decode("utf-8"))
    mp3 = HERE / "vo-full.mp3"
    mp3.write_bytes(base64.b64decode(data["audio_base64"]))
    subprocess.run(["ffmpeg", "-y", "-i", str(mp3), "-ac", "2", "-ar", "48000",
                    "-c:a", "pcm_s16le", str(wav)],
                   check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    js.write_text(json.dumps({"text": SCRIPT, "alignment": data["alignment"]}, indent=2))
    print(f"  wrote {wav.name}")
    return wav, json.loads(js.read_text())


VO_WAV, VO_ALIGN = generate_vo(force=False)


# ========================================================================
# 2. Parse alignment → words + sentence boundaries
# ========================================================================
def tokenize(alignment):
    chars = alignment["characters"]
    starts = alignment["character_start_times_seconds"]
    ends = alignment["character_end_times_seconds"]
    words = []; cur = ""; cur_start = None
    for i, ch in enumerate(chars):
        next_ch = chars[i+1] if i+1 < len(chars) else " "
        prev_ch = chars[i-1] if i > 0 else " "
        inline_dot = ch == "." and prev_ch.isalpha() and next_ch.isalpha()
        is_break = ch.isspace() or ch in ",;:!?—–" or (ch in ".!?" and not inline_dot)
        if is_break:
            if cur:
                words.append((cur, cur_start, ends[i-1] if i > 0 else starts[i]))
                cur = ""; cur_start = None
            if ch in ".!?":
                words.append((ch, starts[i], ends[i]))
        else:
            if cur_start is None: cur_start = starts[i]
            cur += ch
    if cur: words.append((cur, cur_start, ends[-1]))
    return words


words = tokenize(VO_ALIGN["alignment"])
sent_ends = [w[2] for w in words if w[0] in ".!?"]
print(f"→ {len(sent_ends)} sentences; VO ends at {sent_ends[-1]:.2f}s")


def sent_end(i):
    return sent_ends[i] if i < len(sent_ends) else sent_ends[-1]


# Script sentence map (0-indexed):
# 0 Hi, I'm Jeffrey Scudder ... instead of paintings.        -> PAINTER
# 1 This is notepat.                                          ┐
# 2 It's a software instrument ... even pressure.             ┴─ INSTRUMENT
# 3 Underneath is a 128-voice synth ... back samples.         -> SYNTH
# 4 And notepat runs anywhere ... underneath it.              -> BOOT
# 5 It grows out of Aesthetic Computer ... little programs.   -> COMMONS
# 6 My company, Aesthetic Inc. ... and pay for.               ┐
# 7 I want to build the studio ... play again.                ┴─ INVITATION
# 8 That's what I'd do with Restless Egg.                     -> CLOSE
t_painter_end = sent_end(0)
t_instr_end   = sent_end(2)
t_synth_end   = sent_end(3)
t_boot_end    = sent_end(4)
t_commons_end = sent_end(5)
t_invite_end  = sent_end(7)
t_close_end   = sent_end(8) + 1.0


# ========================================================================
# 3. Card definitions
# ========================================================================
def font(path, size):
    try: return ImageFont.truetype(path, size)
    except: return ImageFont.truetype(FALLBACK, size)


def fit_cover(img, tw, th):
    sw, sh = img.size
    sr, tr = sw/sh, tw/th
    if sr > tr:
        new_h = th; new_w = int(th * sr)
    else:
        new_w = tw; new_h = int(tw / sr)
    img = img.resize((new_w, new_h), Image.LANCZOS)
    l = (new_w - tw) // 2; t = (new_h - th) // 2
    return img.crop((l, t, l + tw, t + th))


def tint(img, rgba):
    over = Image.new("RGBA", img.size, rgba)
    return Image.alpha_composite(img.convert("RGBA"), over).convert("RGB")


def make_bg(out, bg_path, tint_rgba=(20, 12, 26, 110)):
    if bg_path and Path(bg_path).exists():
        img = fit_cover(Image.open(bg_path).convert("RGB"), W, H)
        img = tint(img, tint_rgba)
    else:
        img = Image.new("RGB", (W, H), CREAM)
    img.convert("RGB").save(out, quality=95)


def make_text_overlay(out, title_lines, subline=None, pink_idx=None, colorize=False):
    img = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    d = ImageDraw.Draw(img, "RGBA")
    ink = CREAM; dim = (230, 220, 200)

    sizes = []; total_h = 0
    for line in title_lines:
        fs = 132; f = font(YWFT_BOLD, fs)   # LACMA title scale
        while d.textlength(line, font=f) > W * 0.90 and fs > 30:
            fs -= 4; f = font(YWFT_BOLD, fs)
        if d.textlength(line, font=f) < W * 0.3 and fs < 132:
            fs = min(132, fs + 12); f = font(YWFT_BOLD, fs)
        sizes.append((line, fs, f)); total_h += fs + 16

    y = (H - total_h) / 2 - (32 if subline else 0) - 24

    def shadowed(txt, x, yy, f, fill, dx=3, dy=3):
        d.text((x + dx, yy + dy), txt, font=f, fill=(0, 0, 0, 230))
        d.text((x, yy), txt, font=f, fill=fill)

    def rainbow_line(line, x, yy, f):
        cx = x; ci = 0
        for ch in line:
            cw = d.textlength(ch, font=f)
            if ch == " ":
                cx += cw; continue
            color = RAINBOW[ci % len(RAINBOW)]
            d.text((cx + 3, yy + 3), ch, font=f, fill=(0, 0, 0, 230))
            d.text((cx, yy), ch, font=f, fill=color)
            cx += cw; ci += 1

    for i, (line, fs, f) in enumerate(sizes):
        tw = d.textlength(line, font=f)
        if colorize:
            rainbow_line(line, (W - tw)/2, y, f)
        else:
            color = CORAL if pink_idx == i else ink
            shadowed(line, (W - tw)/2, y, f, color)
        y += fs + 16
    if subline:
        sfs = 38; sf = font(YWFT_REG, sfs)
        while d.textlength(subline, font=sf) > W * 0.92 and sfs > 16:
            sfs -= 2; sf = font(YWFT_REG, sfs)
        tw = d.textlength(subline, font=sf)
        shadowed(subline, (W - tw)/2, y + 20, sf, dim, dx=2, dy=2)
    img.save(out)


cards = [
    # (name, end_time, bg, title_lines, subline, pink_idx, zoom_dir, colorize)
    ("01-painter",   t_painter_end + 0.2, HERE / "felt" / "felt-painter.png",
        ["a painter who builds", "instruments"],
        "Jeffrey Scudder / Aesthetic, Inc.", None, "in", False),
    ("02-instrument", t_instr_end, HERE / "felt" / "felt-instrument.png",
        ["notepat"],
        "a software instrument / keyboard / touch / MIDI / pressure", None, "in", True),
    ("03-synth",     t_synth_end, HERE / "felt" / "felt-synth.png",
        ["128 voices,", "modeled from scratch"],
        "a real synthesizer, not sample playback", None, "in", False),
    ("04-boot",      t_boot_end, HERE / "felt" / "felt-boot.png",
        ["boots a $50 laptop", "into an instrument"],
        "browser / phone / bare metal / no OS underneath", None, "in", False),
    ("05-commons",   t_commons_end, HERE / "felt" / "felt-commons.png",
        ["18,000 makers", "16,000 programs"],
        "built on Aesthetic Computer", None, "pan-rl", False),
    ("06-invitation", t_invite_end, HERE / "felt" / "felt-invitation.png",
        ["instruments you play,", "not tools you manage"],
        "Aesthetic, Inc. / an instruments studio", None, "in", False),
    ("07-close",     t_close_end, HERE / "felt" / "felt-close.png",
        ["something you play again"],
        "notepat.com / Restless Egg / Batch 2", None, "in", True),
]

print("→ generating cards")
for name, end_t, bg, lines, sub, pink, zd, colorize in cards:
    if not is_video(bg):
        make_bg(CARDS / f"{name}-bg.jpg", str(bg))
    make_text_overlay(CARDS / f"{name}-text.png", lines, sub, pink, colorize)


def zp_expr(zoom_dir, frames):
    if zoom_dir == "in":
        z = "'min(zoom+0.0008,1.15)'"
        x, y = "'iw/2-(iw/zoom/2)'", "'ih/2-(ih/zoom/2)'"
    elif zoom_dir == "out":
        z = "'if(lte(on,1),1.15,max(1.0,zoom-0.0008))'"
        x, y = "'iw/2-(iw/zoom/2)'", "'ih/2-(ih/zoom/2)'"
    elif zoom_dir == "pan-lr":
        z = "'1.12'"
        x = f"'(iw-iw/zoom)*(on/{frames})'"; y = "'ih/2-(ih/zoom/2)'"
    elif zoom_dir == "pan-rl":
        z = "'1.12'"
        x = f"'(iw-iw/zoom)*(1-on/{frames})'"; y = "'ih/2-(ih/zoom/2)'"
    else:
        z = "'1'"; x = "0"; y = "0"
    return f"zoompan=z={z}:x={x}:y={y}:d={frames}:s={W}x{H}:fps=30"


# ========================================================================
# 4. Encode segments — Ken Burns on still bg
# ========================================================================
segments = []
prev = 0.0
print(f"→ card timings (VO total: {sent_ends[-1]:.2f}s, video total: {t_close_end:.2f}s)")
for name, end_t, bg, lines, sub, pink, zd, colorize in cards:
    dur = end_t - prev
    frames = max(int(dur * 30), 30)
    seg = CARDS / f"seg-{name}.mp4"
    text_png = CARDS / f"{name}-text.png"
    print(f"    {name:<14} [{prev:6.2f}s → {end_t:6.2f}s]  dur={dur:5.2f}s")
    fc = (
        f"[0:v]scale={W*2}:{H*2}:force_original_aspect_ratio=increase,crop={W*2}:{H*2},"
        f"{zp_expr(zd, frames)}[kb];"
        f"[kb][1:v]overlay=0:0[v]"
    )
    subprocess.run([
        "ffmpeg", "-y",
        "-loop", "1", "-framerate", "30", "-i", str(CARDS / f"{name}-bg.jpg"),
        "-loop", "1", "-framerate", "30", "-i", str(text_png),
        "-f", "lavfi", "-i", "anullsrc=r=48000:cl=stereo",
        "-filter_complex", fc,
        "-map", "[v]", "-map", "2:a",
        "-t", f"{dur:.3f}",
        "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
        "-c:a", "aac", "-b:a", "160k",
        str(seg),
    ], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    segments.append(seg)
    prev = end_t

# Concat
print("→ concat")
concat_txt = CARDS / "concat.txt"
concat_txt.write_text("\n".join(f"file '{s.name}'" for s in segments))
concat_raw = CARDS / "concat-raw.mp4"
subprocess.run([
    "ffmpeg", "-y", "-f", "concat", "-safe", "0", "-i", str(concat_txt),
    "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "160k",
    str(concat_raw),
], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


# ========================================================================
# 5. SRT captions from alignment
# ========================================================================
def ts(t):
    h = int(t // 3600); t -= h*3600
    m = int(t // 60); t -= m*60
    s = int(t); ms = int((t - s) * 1000)
    return f"{h:02d}:{m:02d}:{s:02d},{ms:03d}"


def chunk_cues(wlist, max_words=8, max_chars=52):
    cues = []; buf = []
    for w, s, e in wlist:
        if w in ".!?,;:":
            if buf:
                lw, ls, le = buf[-1]
                buf[-1] = (lw + w, ls, e)
            if w in ".!?" and buf:
                cues.append((buf[0][1], buf[-1][2], " ".join(b[0] for b in buf)))
                buf = []
            continue
        buf.append((w, s, e))
        if len(buf) >= max_words or len(" ".join(b[0] for b in buf)) >= max_chars:
            cues.append((buf[0][1], buf[-1][2], " ".join(b[0] for b in buf)))
            buf = []
    if buf:
        cues.append((buf[0][1], buf[-1][2], " ".join(b[0] for b in buf)))
    return cues


cues = chunk_cues(words)
print(f"→ {len(cues)} caption cues")

# Render each caption cue as a full-frame transparent PNG (bottom-centered,
# white text with a black outline + soft strip). Overlaid timed in the final
# pass — no libass dependency (this ffmpeg build ships without the subtitles
# filter).
def render_caption(out, text):
    # LACMA-style subtitle: plain Arial Bold, white with a black outline +
    # soft shadow, bottom-centered (matches the Helvetica/Outline=1/Shadow=2
    # burned-sub look — no background strip).
    img = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    d = ImageDraw.Draw(img, "RGBA")
    fs = 30; f = font(ARIAL_BOLD, fs)
    while d.textlength(text, font=f) > W * 0.86 and fs > 16:
        fs -= 2; f = font(ARIAL_BOLD, fs)
    tw = d.textlength(text, font=f)
    x = (W - tw) / 2; y = H - 62
    d.text((x + 2, y + 3), text, font=f, fill=(0, 0, 0, 170))           # shadow
    for dx, dy in [(-2,0),(2,0),(0,-2),(0,2),(-2,-2),(2,-2),(-2,2),(2,2)]:  # outline
        d.text((x + dx, y + dy), text, font=f, fill=(0, 0, 0, 255))
    d.text((x, y), text, font=f, fill=(255, 255, 255, 255))
    img.save(out)

cap_pngs = []
for i, (s, e, text) in enumerate(cues):
    p = CARDS / f"cap-{i:02d}.png"
    render_caption(p, text)
    cap_pngs.append((p, s, e))


# ========================================================================
# 6. QR (generated once via qrencode) + final encode: captions + VO + QR
# ========================================================================
if not QR.exists():
    qrencode = shutil.which("qrencode")
    if qrencode:
        subprocess.run([qrencode, "-o", str(QR), "-s", "10", "-m", "2",
                        "--foreground=3d2e1f", "--background=f0e6cf", QR_URL],
                       check=True)
        print(f"→ QR generated → {QR.name}")

print("→ final encode (captions + VO + QR)")
inputs = ["-i", str(concat_raw), "-i", str(VO_WAV)]
idx = 2
cap_idx = []
for p, s, e in cap_pngs:
    inputs += ["-i", str(p)]
    cap_idx.append((idx, s, e)); idx += 1
qr_idx = None
if QR.exists():
    inputs += ["-i", str(QR)]; qr_idx = idx; idx += 1

fc = ""
cur = "0:v"
for n, (i, s, e) in enumerate(cap_idx):
    nxt = f"c{n}"
    fc += f"[{cur}][{i}:v]overlay=0:0:enable='between(t,{s:.3f},{e:.3f})'[{nxt}];"
    cur = nxt
if qr_idx is not None:
    fc += f"[{qr_idx}:v]scale=104:104[qr];[{cur}][qr]overlay=W-w-22:22[v];"
else:
    fc += f"[{cur}]null[v];"
fc += "[1:a]volume=1.35[vo];[vo]aresample=48000[a]"

subprocess.run([
    "ffmpeg", "-y", *inputs,
    "-filter_complex", fc,
    "-map", "[v]", "-map", "[a]",
    "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "192k",
    "-movflags", "+faststart",
    str(OUT),
], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

dur = subprocess.check_output([
    "ffprobe", "-v", "error", "-show_entries", "format=duration",
    "-of", "default=nokey=1:noprint_wrappers=1", str(OUT),
]).decode().strip()
print(f"✓ {OUT}")
print(f"  {float(dur):.1f}s · {OUT.stat().st_size / 1024 / 1024:.1f} MB")
