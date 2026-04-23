#!/usr/bin/env python3
"""
LACMA grant video v3 — PRECISE sync using ElevenLabs char-level alignment.
Smaller burned captions, snappier card transitions.
"""

import json, subprocess, re
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

HERE = Path(__file__).parent
CARDS = HERE / "video-cards-v3"
CARDS.mkdir(exist_ok=True)
OUT = HERE / "lacma-grant-video-v3.mp4"
DEMO = Path("/Users/jas/aesthetic-computer/system/public/assets/lacma-2026/ac-native-demo-narrated.mp4")
VO_HOOK = HERE / "vo-hook.wav"
VO_MAIN = HERE / "vo-main.wav"
ALIGN_HOOK = json.loads((HERE / "vo-hook.json").read_text())
ALIGN_MAIN = json.loads((HERE / "vo-main.json").read_text())

W, H = 1280, 800
CREAM = (240, 230, 207); DARK = (61, 46, 31); PINK = (165, 73, 105); DIM = (138, 115, 86)
FONT_HEAVY = "/System/Library/Fonts/SFNS.ttf"
DEMO_DUR = 22.16

def load_font(size):
    try: return ImageFont.truetype(FONT_HEAVY, size)
    except: return ImageFont.load_default()

def card(out_path, lines, subline=None, pink_line_idx=None):
    img = Image.new("RGB", (W, H), CREAM); d = ImageDraw.Draw(img)
    sizes = []; total_h = 0
    for line in lines:
        fs = 92; f = load_font(fs)
        while d.textlength(line, font=f) > W * 0.82 and fs > 24:
            fs -= 4; f = load_font(fs)
        if d.textlength(line, font=f) < W * 0.3 and fs < 92:
            fs = min(92, fs + 12); f = load_font(fs)
        sizes.append((line, fs, f)); total_h += fs + 18
    y = (H - total_h) / 2 - (30 if subline else 0) - 50
    for i, (line, fs, f) in enumerate(sizes):
        tw = d.textlength(line, font=f)
        color = PINK if pink_line_idx == i else DARK
        d.text((W/2 - tw/2, y), line, font=f, fill=color); y += fs + 18
    if subline:
        f = load_font(24); tw = d.textlength(subline, font=f)
        d.text((W/2 - tw/2, y + 10), subline, font=f, fill=DIM)
    d.rectangle([(W/2 - 30, H - 140), (W/2 + 30, H - 137)], fill=PINK)
    img.save(out_path, quality=95)

# --- Parse alignment into word-level tokens with start/end times.
def tokenize(alignment, text):
    chars = alignment["characters"]
    starts = alignment["character_start_times_seconds"]
    ends = alignment["character_end_times_seconds"]
    words = []  # (word, start, end)
    cur = ""; cur_start = None
    for i, ch in enumerate(chars):
        if ch.isspace() or ch in ",.;:!?—–":
            if cur:
                words.append((cur, cur_start, ends[i-1] if i > 0 else starts[i]))
                cur = ""; cur_start = None
            if ch in ".!?":
                # mark sentence end by appending a virtual punctuation token
                words.append((ch, starts[i], ends[i]))
        else:
            if cur_start is None: cur_start = starts[i]
            cur += ch
    if cur: words.append((cur, cur_start, ends[-1]))
    return words

hook_words = tokenize(ALIGN_HOOK["alignment"], ALIGN_HOOK["text"])
main_words = tokenize(ALIGN_MAIN["alignment"], ALIGN_MAIN["text"])

# --- Find sentence boundaries in main (indices of "." tokens).
def sentence_ends(words):
    return [w[2] for w in words if w[0] in ".!?"]

hook_sent = sentence_ends(hook_words)  # e.g. [2.x, 5.x, 10.x]
main_sent = sentence_ends(main_words)
print(f"hook sentences end at: {[f'{t:.2f}' for t in hook_sent]}")
print(f"main sentences end at: {[f'{t:.2f}' for t in main_sent]}")

# --- Decide card transitions in MAIN VO based on semantic groupings.
# The main VO text has these sentence groupings by card:
#   AC Native (sentences 1-4): "This is AC Native.", "A bare-metal creative computing operating system.", "No desktop, no app store.", "Fifty dollars per seat."
#   Library (sentences 5-9): "For the LACMA Art and Technology Lab, we propose growing it into a public device library.", "A lending fleet of AC Blank laptops flashed with our OS, circulating through Flash Days, workshops, and Family Play afternoons at LACMA.", "A public waitlist.", "The library welcomes artists flashing their own custom creative operating systems, joining a tradition of artist-run device libraries."
#   Community (sentences 10-14): "Aesthetic Computer has been under active development since 2021.", "Over nineteen thousand commits.", "Seventeen thousand KidLisp programs.", "Twenty-eight hundred registered handles.", "People draw, chat, and compose pieces together in real time."
#   Events (sentences 15-16): "At the 2027 Symposium we boot the cohort.", "At the 2028 Demo Day we play the room."
#   Close (sentences 17-19): "Aesthetic dot Computer.", "A civic instrument.", "The new scene has just begun."
#
# After each sentence_end[n] is the start of the next group.
# Card transition timestamps (in MAIN VO absolute seconds):
t_ac_native_start = 0.0
t_library_start = main_sent[3]              # after "Fifty dollars per seat."
t_community_start = main_sent[7]            # after "...tradition of artist-run device libraries."
t_events_start = main_sent[12]              # after "People draw, chat..."
t_close_start = main_sent[14]               # after "play the room."
t_end = main_sent[-1] + 0.1                 # last sentence end

print(f"card starts in MAIN: AC={t_ac_native_start:.1f} Lib={t_library_start:.1f} Com={t_community_start:.1f} Evt={t_events_start:.1f} Cls={t_close_start:.1f} End={t_end:.1f}")

# --- Generate title cards (same content as v2).
cards_meta = [
    ("01-hook",       hook_sent[-1] + 0.15, ["Personal computers", "are not done yet."], "LACMA Art + Technology Lab · 2026", None),
    ("02-ac-native",  t_library_start - t_ac_native_start, ["AC Native", "Bare-metal creative OS"], "PID 1 · 192 kHz · 32 voices · $50 per seat", None),
    ("03-library",    t_community_start - t_library_start, ["AC Device Library"], "Lending fleet · AC Blank laptops at $128 · Flash Days · Family Play · public waitlist", None),
    ("04-community",  t_events_start - t_community_start, ["19,000 commits", "17,000 KidLisp programs", "2,800 handles"], "aesthetic.computer · active since 2021", None),
    ("05-events",     t_close_start - t_events_start, ["boot the cohort", "play the room"], "2027 Symposium · 2028 Demo Day", None),
    ("06-close",      t_end - t_close_start + 1.0, ["Aesthetic.Computer"], "A civic instrument · aesthetic.computer/lacma-2026", 0),
]

print("→ generating cards")
for name, _, lines, subline, pink in cards_meta:
    card(CARDS / f"{name}.jpg", lines, subline, pink)

# Demo duration
demo_meta = ("demo", DEMO_DUR)

# --- Encode each segment ---
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

# --- Build composite VO track
# Hook card ends at cards_meta[0][1]. Then demo. Then main.
hook_card_dur = cards_meta[0][1]
vo_track = CARDS / "vo-track.wav"
print("→ composing VO track")
subprocess.run([
    "ffmpeg", "-y",
    "-i", str(VO_HOOK),
    "-i", str(VO_MAIN),
    "-filter_complex",
    f"[0:a]apad=whole_dur={hook_card_dur:.3f}[p1];"
    f"aevalsrc=0:d={DEMO_DUR:.3f}[s1];"
    f"[1:a]anull[p2];"
    f"[p1][s1][p2]concat=n=3:v=0:a=1[vo]",
    "-map", "[vo]", "-ar", "48000", "-ac", "2",
    str(vo_track),
], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

# --- Build SRT from word-level alignment.
# Group words into ~6-10 word chunks, break at sentence ends.
def ts(t):
    h = int(t // 3600); t -= h*3600
    m = int(t // 60); t -= m*60
    s = int(t); ms = int((t - s) * 1000)
    return f"{h:02d}:{m:02d}:{s:02d},{ms:03d}"

def chunk_words(words, offset, max_words=8, max_chars=52):
    """Group consecutive non-punctuation words into short caption cues."""
    cues = []; buf = []; buf_start = None
    for w, s, e in words:
        if w in ".!?,;:":
            if buf:
                text = " ".join(b[0] for b in buf)
                cues.append((buf[0][1] + offset, buf[-1][2] + offset, text))
                buf = []; buf_start = None
            continue
        buf.append((w, s, e))
        cur = " ".join(b[0] for b in buf)
        if len(buf) >= max_words or len(cur) >= max_chars:
            cues.append((buf[0][1] + offset, buf[-1][2] + offset, cur))
            buf = []
    if buf:
        text = " ".join(b[0] for b in buf)
        cues.append((buf[0][1] + offset, buf[-1][2] + offset, text))
    return cues

hook_cues = chunk_words(hook_words, offset=0.0)
main_cues = chunk_words(main_words, offset=hook_card_dur + DEMO_DUR)

all_cues = hook_cues + main_cues
srt_path = CARDS / "captions.srt"
with open(srt_path, "w") as f:
    for i, (s, e, text) in enumerate(all_cues, 1):
        f.write(f"{i}\n{ts(s)} --> {ts(e)}\n{text}\n\n")
print(f"→ SRT: {len(all_cues)} cues")

# --- Final encode: burn subtitles (SMALLER font) + mix audio.
print("→ final encode")
subprocess.run([
    "ffmpeg", "-y",
    "-i", str(concat_raw),
    "-i", str(vo_track),
    "-filter_complex",
    # Smaller burned subtitles: FontSize=14, tighter margin.
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
print(f"  duration: {float(dur):.2f}s, size: {OUT.stat().st_size / 1024 / 1024:.1f} MB")
