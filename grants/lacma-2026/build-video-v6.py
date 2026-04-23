#!/usr/bin/env python3
"""
LACMA grant video v6 — ONE-SHOT VO pipeline.

Single ElevenLabs call produces vo-full.wav + vo-full.json (char-level
alignment). The build script splits sentence boundaries from that one
stream for card transitions + SRT captions. Homogeneous voice, no
hook/main seam.

Flow:
  1. vo() — one TTS call, writes vo-full.wav and vo-full.json
  2. parse alignment → word/sentence boundaries
  3. card segments sized to their matching sentences
  4. concat segments, overlay VO, burn SRT
"""

import json, subprocess, base64, os
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont
from urllib.request import Request, urlopen

HERE = Path(__file__).parent
OUT = HERE / "lacma-grant-video-v6.mp4"
CARDS = HERE / "video-cards-v6"
CARDS.mkdir(exist_ok=True)
ASSETS = Path.home() / "Desktop" / "ac-deck-assets"

VAULT_ENV = "/Users/jas/aesthetic-computer/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env"
VOICE_ID = "dYNGZ848Oo6DtNBoeqgh"  # @jeffrey PVC
MODEL_ID = "eleven_multilingual_v2"

W, H = 1280, 800
CREAM = (240, 230, 207); DARK = (61, 46, 31); PINK = (165, 73, 105); DIM = (138, 115, 86)
YWFT_BOLD = str(ASSETS / "fonts" / "ywft-processing-bold.ttf")
YWFT_REG = str(ASSETS / "fonts" / "ywft-processing-regular.ttf")
FALLBACK = "/System/Library/Fonts/SFNS.ttf"

# ========================================================================
# 1. ONE-SHOT VO: combined script read in one ElevenLabs call
# ========================================================================
SCRIPT = (
    # Flowing paragraph prose — fewer sentence breaks so delivery reads natural.
    "[calm] Hi, I'm Jeffrey, and for the last five years I've been building "
    "Aesthetic.Computer — a creative computing system that treats the personal "
    "computer itself as an unfinished instrument. "
    "It's three things at once: an operating system you can boot a laptop into, "
    "a programming language called KidLisp, and a social network where people "
    "make art together. "
    "The whole stack is free and open source — nineteen thousand commits in, "
    "with seventeen thousand KidLisp programs and twenty-eight hundred handles "
    "drawing, chatting, and composing in real time. "
    "With LLM-powered development there's never been a better time to write "
    "new software for old hardware — Windows 10 end-of-life just stranded "
    "hundreds of millions of laptops, and strip away the consumer operating "
    "system and those machines become a planetary population of half-built "
    "instruments waiting for a kernel. "
    "For the LACMA Art and Technology Lab, we propose growing Aesthetic.Computer "
    "into a public device library: a lending fleet of refurbished laptops "
    "flashed with our operating system at fifty dollars per seat, circulating "
    "through Flash Days, workshops, and Family Play afternoons at the museum. "
    "At the 2027 Symposium we boot the cohort; at the 2028 Demo Day we play the room. "
    "The personal computer is a civic instrument, and a new scene is just beginning."
)

def load_key():
    return (Path(VAULT_ENV).read_text().split("\n")
            and [l for l in Path(VAULT_ENV).read_text().split("\n")
                 if l.startswith("ELEVENLABS_API_KEY=")][0].split("=", 1)[1].strip())

def generate_vo(force=False):
    wav = HERE / "vo-full.wav"
    js = HERE / "vo-full.json"
    if wav.exists() and js.exists() and not force:
        print(f"→ using cached {wav.name}")
        return wav, json.loads(js.read_text())

    import urllib.request, urllib.error
    key = load_key()
    url = f"https://api.elevenlabs.io/v1/text-to-speech/{VOICE_ID}/with-timestamps?output_format=mp3_44100_128"
    body = json.dumps({
        "text": SCRIPT,
        "model_id": MODEL_ID,
        "voice_settings": {
            # Higher stability = more consistent/calmer cadence.
            # Lower style = less dramatic, more conversational.
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

VO_WAV, VO_ALIGN = generate_vo(force=True)  # always regen for homogeneity

# ========================================================================
# 2. Parse alignment → words + sentence boundaries
# ========================================================================
def tokenize(alignment):
    chars = alignment["characters"]
    starts = alignment["character_start_times_seconds"]
    ends = alignment["character_end_times_seconds"]
    words = []; cur = ""; cur_start = None
    for i, ch in enumerate(chars):
        if ch.isspace() or ch in ",.;:!?—–":
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

# Sentence index → card mapping
# sentences (0-indexed):
# 0: "Personal computers have not been very personal."           HOOK
# 1: "Windows 10 end-of-life just stranded ..."                  HOOK
# 2: "There has never been a better time ..."                    HOOK
# 3: "Aesthetic.Computer can run as your whole ..."              AC
# 4: "No desktop, no app store."                                 AC
# 5: "Fifty dollars per seat."                                   AC
# 6: "For the LACMA Art and Technology Lab, we propose ..."      LIB
# 7: "A lending fleet of AC Blank laptops ..."                   LIB
# 8: "A public waitlist."                                        LIB
# 9: "The library welcomes artists ..."                          LIB
# 10: "Aesthetic.Computer has been under active dev since 2021." COM
# 11: "Over nineteen thousand commits."                          COM
# 12: "Seventeen thousand KidLisp programs."                     COM
# 13: "Twenty-eight hundred registered handles."                 COM
# 14: "People draw, chat, and compose pieces together ..."       COM
# 15: "At the 2027 Symposium we boot the cohort."                EVT
# 16: "At the 2028 Demo Day we play the room."                   EVT
# 17: "Aesthetic dot Computer."                                  CLS
# 18: "A civic instrument."                                      CLS
# 19: "The new scene has just begun."                            CLS
def sent_end(i):
    return sent_ends[i] if i < len(sent_ends) else sent_ends[-1]

# New script sentence map (0-indexed):
# 0 Hi.
# 1 I'm Jeffrey.
# 2 For the last five years ... unfinished instrument.          -> HOOK ends here
# 3 It's three things at once.
# 4 An operating system you can boot a laptop into.
# 5 A programming language called KidLisp.
# 6 And a social network where people make art together.
# 7 The whole stack is free and open source.                    -> AC ends here
# 8 Nineteen thousand commits in, with ... in real time.        -> COMMUNITY ends here
# 9 There's never been a better time ...
# 10 Windows 10 end-of-life ...
# 11 Strip away the consumer OS ... waiting for a kernel.       -> FEEDSTOCK ends here
# 12 For the LACMA Art and Technology Lab ... public device library.
# 13 A lending fleet of refurbished laptops ... at the museum.
# 14 A public waitlist.                                         -> LIBRARY ends here
# 15 At the 2027 Symposium, we boot the cohort.
# 16 At the 2028 Demo Day, we play the room.                    -> EVENTS ends here
# 17 This is not a product.
# 18 It's an argument.
# 19 The personal computer is a civic instrument, ... begun.    -> CLOSE ends here
# New paragraph-prose script has 7 sentences (0-indexed):
# 0 Hi, I'm Jeffrey, ... unfinished instrument.       -> HOOK
# 1 It's three things at once: OS, language, network. -> AC
# 2 Free and open source — 19k commits, 17k kidlisp ..-> COMMUNITY
# 3 LLM-powered dev, Windows 10, half-built instruments-> FEEDSTOCK
# 4 LACMA Lab, device library, lending fleet ...      -> LIBRARY
# 5 Symposium + Demo Day                              -> EVENTS
# 6 civic instrument, new scene just beginning        -> CLOSE
t_hook_end = sent_end(0)
t_ac_end = sent_end(1)
t_com_end = sent_end(2)
t_feed_end = sent_end(3)
t_lib_end = sent_end(4)
t_evt_end = sent_end(5)
t_cls_end = sent_end(6) + 1.2

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

def make_bg(out, bg_path, tint_rgba=(20, 15, 10, 120)):
    """Background image only — used for Ken Burns base layer."""
    if bg_path and Path(bg_path).exists():
        img = fit_cover(Image.open(bg_path).convert("RGB"), W, H)
        img = tint(img, tint_rgba)
    else:
        img = Image.new("RGB", (W, H), CREAM)
    img.convert("RGB").save(out, quality=95)

def make_text_overlay(out, title_lines, subline=None, pink_idx=None):
    """Transparent PNG with just title + subline + accent — stays STILL
    over the Ken Burns background."""
    img = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    d = ImageDraw.Draw(img, "RGBA")
    ink = CREAM; dim = (230, 220, 200)

    sizes = []; total_h = 0
    for line in title_lines:
        fs = 132; f = font(YWFT_BOLD, fs)
        while d.textlength(line, font=f) > W * 0.90 and fs > 32:
            fs -= 4; f = font(YWFT_BOLD, fs)
        if d.textlength(line, font=f) < W * 0.3 and fs < 132:
            fs = min(132, fs + 12); f = font(YWFT_BOLD, fs)
        sizes.append((line, fs, f)); total_h += fs + 16

    y = (H - total_h) / 2 - (32 if subline else 0) - 30
    def shadowed(txt, x, yy, f, fill, dx=3, dy=3):
        d.text((x + dx, yy + dy), txt, font=f, fill=(0, 0, 0, 230))
        d.text((x, yy), txt, font=f, fill=fill)
    for i, (line, fs, f) in enumerate(sizes):
        tw = d.textlength(line, font=f)
        color = PINK if pink_idx == i else ink
        shadowed(line, (W - tw)/2, y, f, color); y += fs + 16
    if subline:
        # Auto-scale subline so long lines still fit
        sfs = 40; sf = font(YWFT_REG, sfs)
        while d.textlength(subline, font=sf) > W * 0.92 and sfs > 18:
            sfs -= 2; sf = font(YWFT_REG, sfs)
        tw = d.textlength(subline, font=sf)
        shadowed(subline, (W - tw)/2, y + 22, sf, dim, dx=2, dy=2)
    # (pink accent bar removed per feedback)
    img.save(out)  # keep transparency

cards = [
    # (name, end_time, bg, title_lines, subline, pink_idx, zoom_dir)
    # zoom_dir: 'in' | 'out' | 'pan-lr' | 'pan-rl' | None
    ("01-hook",      t_hook_end + 0.2, ASSETS / "jeffrey-IMG_2124.jpg",
        ["Aesthetic.Computer"],
        "a creative computing system · LACMA Art + Technology Lab 2026", 0, "in"),
    ("02-ac",        t_ac_end,  ASSETS / "ac-native-demo-poster.jpg",
        ["three layers"],
        "operating system · language · network", None, "out"),
    ("03-community", t_com_end, ASSETS / "platform-screenshot.jpg",
        ["19,000 commits", "17,000 KidLisp pieces", "2,800 handles"],
        "aesthetic.computer · active since 2021", None, "pan-lr"),
    ("04-feedstock", t_feed_end, ASSETS / "kidlisp-berz.jpg",
        ["never been a better time", "for new software", "on old hardware"],
        "240 million stranded x86 laptops · LLM-powered development", None, "in"),
    ("05-library",   t_lib_end, ASSETS / "card-gallery.jpg",
        ["AC Device Library"],
        "lending fleet · Flash Days · Family Play · public waitlist", None, "pan-rl"),
    # Only photo we've verified to contain laptops is IMG_2124 — reuse
    # it for Events with a different Ken Burns direction so it reads fresh.
    ("06-events",    t_evt_end, ASSETS / "jeffrey-IMG_2124.jpg",
        ["boot the cohort", "play the room"],
        "2027 Symposium · 2028 Demo Day", None, "pan-rl"),
    ("07-close",     t_cls_end, ASSETS / "kidlisp-roz.jpg",
        ["a civic instrument"],
        "aesthetic.computer/lacma-2026", None, "in"),
]

print("→ generating cards")
for name, end_t, bg, lines, sub, pink, zd in cards:
    make_bg(CARDS / f"{name}-bg.jpg", str(bg))
    make_text_overlay(CARDS / f"{name}-text.png", lines, sub, pink)

# Ken Burns zoompan expression builder
def zp_expr(zoom_dir, frames):
    if zoom_dir == "in":
        z = f"'min(zoom+0.0008,1.15)'"
        x, y = "'iw/2-(iw/zoom/2)'", "'ih/2-(ih/zoom/2)'"
    elif zoom_dir == "out":
        z = f"'if(lte(on,1),1.15,max(1.0,zoom-0.0008))'"
        x, y = "'iw/2-(iw/zoom/2)'", "'ih/2-(ih/zoom/2)'"
    elif zoom_dir == "pan-lr":
        z = "'1.12'"
        x = f"'(iw-iw/zoom)*(on/{frames})'"
        y = "'ih/2-(ih/zoom/2)'"
    elif zoom_dir == "pan-rl":
        z = "'1.12'"
        x = f"'(iw-iw/zoom)*(1-on/{frames})'"
        y = "'ih/2-(ih/zoom/2)'"
    else:
        z = "'1'"; x = "0"; y = "0"
    return f"zoompan=z={z}:x={x}:y={y}:d={frames}:s=1280x800:fps=30"

# ========================================================================
# 4. Encode segments — Ken Burns on bg ONLY, still text overlay on top
# ========================================================================
segments = []
prev = 0.0
for name, end_t, bg, lines, sub, pink, zd in cards:
    dur = end_t - prev
    frames = max(int(dur * 30), 30)
    seg = CARDS / f"seg-{name}.mp4"
    # [0] bg image (looped), [1] text overlay PNG (still), [2] silent audio
    fc = (
        f"[0:v]scale=2560:1600:force_original_aspect_ratio=increase,crop=2560:1600,"
        f"{zp_expr(zd, frames)}[kb];"
        f"[kb][1:v]overlay=0:0[v]"
    )
    subprocess.run([
        "ffmpeg", "-y",
        "-loop", "1", "-framerate", "30", "-i", str(CARDS / f"{name}-bg.jpg"),
        "-loop", "1", "-framerate", "30", "-i", str(CARDS / f"{name}-text.png"),
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
srt_path = CARDS / "captions.srt"
with open(srt_path, "w") as f:
    for i, (s, e, text) in enumerate(cues, 1):
        f.write(f"{i}\n{ts(s)} --> {ts(e)}\n{text}\n\n")
print(f"→ SRT: {len(cues)} cues")

# ========================================================================
# 6. Final encode: subs + VO
# ========================================================================
print("→ final encode")
subprocess.run([
    "ffmpeg", "-y",
    "-i", str(concat_raw),
    "-i", str(VO_WAV),
    "-filter_complex",
    # Traditional white-with-black-shadow subtitles, smaller + lower
    f"[0:v]subtitles='{srt_path}':force_style='FontName=Helvetica,FontSize=16,Bold=1,"
    "PrimaryColour=&H00FFFFFF,OutlineColour=&H00000000,"
    "BorderStyle=1,Outline=1,Shadow=2,Alignment=2,MarginV=18'[v];"
    "[1:a]volume=1.35[vo];"
    "[vo]aresample=48000[a]",
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
