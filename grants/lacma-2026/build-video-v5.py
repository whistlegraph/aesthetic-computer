#!/usr/bin/env python3
"""
LACMA grant video v5 — AC-STYLED visuals.

Replaces the plain-cream cards with background images from the AC
asset library, titled in YWFT Processing (the Proce55ing-style AC
display font) with the sepia/pink palette.
"""

import json, subprocess
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont, ImageFilter

HERE = Path(__file__).parent
OUT = HERE / "lacma-grant-video-v5.mp4"
CARDS = HERE / "video-cards-v5"
CARDS.mkdir(exist_ok=True)
ASSETS = Path.home() / "Desktop" / "ac-deck-assets"
DEMO = Path("/Users/jas/aesthetic-computer/system/public/assets/lacma-2026/ac-native-demo-narrated.mp4")
VO_HOOK = HERE / "vo-hook.wav"
VO_MAIN = HERE / "vo-main.wav"
ALIGN_HOOK = json.loads((HERE / "vo-hook.json").read_text())
ALIGN_MAIN = json.loads((HERE / "vo-main.json").read_text())
DEMO_DUR = 22.16

W, H = 1280, 800
CREAM = (240, 230, 207)
DARK = (61, 46, 31)
PINK = (165, 73, 105)
DIM = (138, 115, 86)

YWFT_BOLD = str(ASSETS / "fonts" / "ywft-processing-bold.ttf")
YWFT_REG = str(ASSETS / "fonts" / "ywft-processing-regular.ttf")
FALLBACK = "/System/Library/Fonts/SFNS.ttf"

def font(path, size):
    try: return ImageFont.truetype(path, size)
    except: return ImageFont.truetype(FALLBACK, size)

def fit_cover(img, target_w, target_h):
    """Resize+crop image to exactly target dimensions."""
    src_w, src_h = img.size
    src_aspect = src_w / src_h
    tgt_aspect = target_w / target_h
    if src_aspect > tgt_aspect:
        # too wide — fit height, crop width
        new_h = target_h
        new_w = int(target_h * src_aspect)
    else:
        new_w = target_w
        new_h = int(target_w / src_aspect)
    img = img.resize((new_w, new_h), Image.LANCZOS)
    left = (new_w - target_w) // 2
    top = (new_h - target_h) // 2
    return img.crop((left, top, left + target_w, top + target_h))

def tint_overlay(img, color, alpha):
    """Apply a translucent colored overlay for text legibility."""
    overlay = Image.new("RGBA", img.size, color + (alpha,))
    return Image.alpha_composite(img.convert("RGBA"), overlay).convert("RGB")

def make_card(out_path, bg_path, title_lines, subline=None, pink_idx=None,
              bg_tint=None, text_box=None, light_text=False):
    """
    bg_path: absolute image path for background (or None for solid cream).
    bg_tint: (r,g,b,alpha) overlay on top of bg to improve text contrast.
    text_box: (x, y, w, h) rectangle where the text card floats. If None,
              full-screen centered text on cream.
    light_text: if True, use cream text on dark bg (e.g. over a photo).
    """
    if bg_path and Path(bg_path).exists():
        bg = Image.open(bg_path).convert("RGB")
        img = fit_cover(bg, W, H)
        if bg_tint:
            img = tint_overlay(img, bg_tint[:3], bg_tint[3])
    else:
        img = Image.new("RGB", (W, H), CREAM)

    d = ImageDraw.Draw(img, "RGBA")

    ink = CREAM if light_text else DARK
    dim = (200, 180, 140) if light_text else DIM

    # Optional floating text panel (with slight cream tint) for legibility
    if text_box:
        tx, ty, tw, th = text_box
        # Translucent cream panel
        panel = Image.new("RGBA", (tw, th), CREAM + (235,))
        img.paste(panel, (tx, ty), panel)

    # Measure + stack title lines
    sizes = []; total_h = 0
    for line in title_lines:
        fs = 96
        f = font(YWFT_BOLD, fs)
        max_w = (text_box[2] if text_box else W) * 0.88
        while d.textlength(line, font=f) > max_w and fs > 24:
            fs -= 4
            f = font(YWFT_BOLD, fs)
        if d.textlength(line, font=f) < max_w * 0.3 and fs < 96:
            fs = min(96, fs + 10)
            f = font(YWFT_BOLD, fs)
        sizes.append((line, fs, f))
        total_h += fs + 14

    # Positioning
    box_x = text_box[0] if text_box else 0
    box_y = text_box[1] if text_box else 0
    box_w = text_box[2] if text_box else W
    box_h = text_box[3] if text_box else H
    y = box_y + (box_h - total_h) / 2 - (22 if subline else 0)

    for i, (line, fs, f) in enumerate(sizes):
        tw = d.textlength(line, font=f)
        color = PINK if pink_idx == i else ink
        d.text((box_x + (box_w - tw) / 2, y), line, font=f, fill=color)
        y += fs + 14

    if subline:
        f = font(YWFT_REG, 22)
        tw = d.textlength(subline, font=f)
        d.text((box_x + (box_w - tw) / 2, y + 12), subline, font=f, fill=dim)

    # Pink accent dot / bar
    cx = box_x + box_w / 2
    baseline = box_y + box_h - 36 if text_box else H - 120
    d.rectangle([(cx - 22, baseline), (cx + 22, baseline + 3)], fill=PINK)

    img.convert("RGB").save(out_path, quality=95)

# --- Word/sentence tokenization for sync
def tokenize(alignment, text):
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

hook_words = tokenize(ALIGN_HOOK["alignment"], ALIGN_HOOK["text"])
main_words = tokenize(ALIGN_MAIN["alignment"], ALIGN_MAIN["text"])
def sentence_ends(words): return [w[2] for w in words if w[0] in ".!?"]
hook_sent = sentence_ends(hook_words)
main_sent = sentence_ends(main_words)

t_ac_native_start = 0.0
t_library_start = main_sent[3]
t_community_start = main_sent[7]
t_events_start = main_sent[12]
t_close_start = main_sent[14]
t_end = main_sent[-1] + 0.1

hook_dur = hook_sent[-1] + 0.15
cards_meta = [
    # (name, duration, bg_path, title_lines, subline, pink_idx, bg_tint, text_box, light_text)
    ("01-hook", hook_dur,
        str(ASSETS / "ac-native-laptop-hero.png"),
        ["Personal computers", "are not done yet."],
        "LACMA Art + Technology Lab · 2026",
        None,
        (240, 230, 207, 170),  # cream tint for legibility over photo
        None,
        False),
    ("02-ac-native", t_library_start - t_ac_native_start,
        str(ASSETS / "ac-native-demo-poster.jpg"),
        ["Aesthetic.Computer"],
        "Boots a laptop directly into art software · $50 per seat",
        0,  # pink the main word
        (240, 230, 207, 220),
        None,
        False),
    ("03-library", t_community_start - t_library_start,
        str(ASSETS / "hardware-yoga.jpg"),
        ["AC Device Library"],
        "Lending fleet · AC Blank laptops at $128 · Flash Days · Family Play · public waitlist",
        None,
        (240, 230, 207, 220),
        None,
        False),
    ("04-community", t_events_start - t_community_start,
        str(ASSETS / "kidlisp-berz.jpg"),
        ["19,000 commits", "17,000 KidLisp pieces", "2,800 handles"],
        "aesthetic.computer · active since 2021",
        None,
        (240, 230, 207, 210),
        None,
        False),
    ("05-events", t_close_start - t_events_start,
        str(ASSETS / "lacma-2026-page.jpg"),
        ["boot the cohort", "play the room"],
        "2027 Symposium · 2028 Demo Day",
        None,
        (240, 230, 207, 210),
        None,
        False),
    ("06-close", t_end - t_close_start + 1.0,
        str(ASSETS / "platform-screenshot.jpg"),
        ["Aesthetic.Computer"],
        "A civic instrument · aesthetic.computer/lacma-2026",
        0,
        (240, 230, 207, 215),
        None,
        False),
]

print("→ generating cards")
for name, _, bg, lines, sub, pink, tint, tb, lt in cards_meta:
    make_card(CARDS / f"{name}.jpg", bg, lines, sub, pink, tint, tb, lt)

# --- Encode segments
segments = []
for i, (name, dur, *_) in enumerate(cards_meta):
    seg = CARDS / f"seg-{name}.mp4"
    subprocess.run([
        "ffmpeg", "-y",
        "-loop", "1", "-framerate", "30",
        "-i", str(CARDS / f"{name}.jpg"),
        "-f", "lavfi", "-i", "anullsrc=r=48000:cl=stereo",
        "-t", f"{dur:.3f}",
        "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
        "-c:a", "aac", "-b:a", "160k",
        str(seg),
    ], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    segments.append(seg)
    if i == 0:
        demo_seg = CARDS / "seg-demo.mp4"
        subprocess.run([
            "ffmpeg", "-y", "-i", str(DEMO),
            "-vf", "fps=30",
            "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
            "-c:a", "aac", "-b:a", "160k", "-ar", "48000",
            str(demo_seg),
        ], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        segments.append(demo_seg)

concat_txt = CARDS / "concat.txt"
concat_txt.write_text("\n".join(f"file '{s.name}'" for s in segments))

print("→ concatenating")
concat_raw = CARDS / "concat-raw.mp4"
subprocess.run([
    "ffmpeg", "-y", "-f", "concat", "-safe", "0", "-i", str(concat_txt),
    "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "160k",
    str(concat_raw),
], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

# VO track with silence for demo
hook_card_dur = cards_meta[0][1]
vo_track = CARDS / "vo-track.wav"
print("→ composing VO")
subprocess.run([
    "ffmpeg", "-y",
    "-i", str(VO_HOOK), "-i", str(VO_MAIN),
    "-filter_complex",
    f"[0:a]apad=whole_dur={hook_card_dur:.3f}[p1];"
    f"aevalsrc=0:d={DEMO_DUR:.3f}[s1];"
    f"[1:a]anull[p2];"
    f"[p1][s1][p2]concat=n=3:v=0:a=1[vo]",
    "-map", "[vo]", "-ar", "48000", "-ac", "2",
    str(vo_track),
], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

# SRT
def ts(t):
    h=int(t//3600); t-=h*3600
    m=int(t//60); t-=m*60
    s=int(t); ms=int((t-s)*1000)
    return f"{h:02d}:{m:02d}:{s:02d},{ms:03d}"

def chunk_words(words, offset, max_words=8, max_chars=52):
    cues = []; buf = []
    for w, s, e in words:
        if w in ".!?,;:":
            if buf:
                cues.append((buf[0][1]+offset, buf[-1][2]+offset, " ".join(b[0] for b in buf)))
                buf = []
            continue
        buf.append((w, s, e))
        if len(buf) >= max_words or len(" ".join(b[0] for b in buf)) >= max_chars:
            cues.append((buf[0][1]+offset, buf[-1][2]+offset, " ".join(b[0] for b in buf)))
            buf = []
    if buf:
        cues.append((buf[0][1]+offset, buf[-1][2]+offset, " ".join(b[0] for b in buf)))
    return cues

cues = chunk_words(hook_words, 0.0) + chunk_words(main_words, hook_card_dur + DEMO_DUR)
srt_path = CARDS / "captions.srt"
with open(srt_path, "w") as f:
    for i, (s, e, text) in enumerate(cues, 1):
        f.write(f"{i}\n{ts(s)} --> {ts(e)}\n{text}\n\n")
print(f"→ SRT: {len(cues)} cues")

# Final encode with burned subs + audio mix
print("→ final encode")
subprocess.run([
    "ffmpeg", "-y",
    "-i", str(concat_raw),
    "-i", str(vo_track),
    "-filter_complex",
    f"[0:v]subtitles='{srt_path}':force_style='FontName=Helvetica,FontSize=14,"
    "PrimaryColour=&H00e4d8bc,OutlineColour=&H001c1812,BackColour=&Hc01c1812,"
    "BorderStyle=3,Outline=1,Shadow=0,Alignment=2,MarginV=28'[v];"
    "[0:a]volume=0.22[bg];"
    "[1:a]volume=1.35[vo];"
    "[bg][vo]amix=inputs=2:duration=first:dropout_transition=0.3[a]",
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
