#!/usr/bin/env python3
"""
LACMA grant video, version 2 — precise VO↔slide sync + burned captions.

Timeline:
  0.0 – 10.8   hook card              vo-hook.wav
  10.8 – 32.8  notepat demo           (demo's own audio)
  32.8 – 45.8  AC Native card         vo-main.wav [0-13)
  45.8 – 65.8  Library card           vo-main.wav [13-33)
  65.8 – 80.8  Community card         vo-main.wav [33-48)
  80.8 – 86.8  Events card            vo-main.wav [48-54)
  86.8 – 93.4  Close card             vo-main.wav [54-60.64)
"""

import subprocess
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

HERE = Path(__file__).parent
CARDS = HERE / "video-cards-v2"
CARDS.mkdir(exist_ok=True)
OUT = HERE / "lacma-grant-video-v2.mp4"
DEMO = Path("/Users/jas/aesthetic-computer/system/public/assets/lacma-2026/ac-native-demo-narrated.mp4")
VO_HOOK = HERE / "vo-hook.wav"
VO_MAIN = HERE / "vo-main.wav"

W, H = 1280, 800
CREAM = (240, 230, 207)
DARK = (61, 46, 31)
PINK = (165, 73, 105)
DIM = (138, 115, 86)

FONT_HEAVY = "/System/Library/Fonts/SFNS.ttf"

def load_font(path, size):
    try: return ImageFont.truetype(path, size)
    except: return ImageFont.load_default()

def card(out_path, lines, subline=None, pink_line_idx=None):
    img = Image.new("RGB", (W, H), CREAM)
    d = ImageDraw.Draw(img)

    sizes = []
    total_h = 0
    for line in lines:
        fs = 92
        f = load_font(FONT_HEAVY, fs)
        while d.textlength(line, font=f) > W * 0.82 and fs > 24:
            fs -= 4
            f = load_font(FONT_HEAVY, fs)
        if d.textlength(line, font=f) < W * 0.3 and fs < 92:
            fs = min(92, fs + 12)
            f = load_font(FONT_HEAVY, fs)
        sizes.append((line, fs, f))
        total_h += fs + 18

    y = (H - total_h) / 2 - (30 if subline else 0) - 40  # shift up to leave room for captions
    for i, (line, fs, f) in enumerate(sizes):
        tw = d.textlength(line, font=f)
        color = PINK if (pink_line_idx is not None and i == pink_line_idx) else DARK
        d.text((W/2 - tw/2, y), line, font=f, fill=color)
        y += fs + 18

    if subline:
        f = load_font(FONT_HEAVY, 26)
        tw = d.textlength(subline, font=f)
        d.text((W/2 - tw/2, y + 10), subline, font=f, fill=DIM)

    d.rectangle([(W/2 - 40, H - 120), (W/2 + 40, H - 116)], fill=PINK)
    img.save(out_path, quality=95)

cards_spec = [
    ("01-hook", 10.8, ["Personal computers", "are not done yet."],
        "LACMA Art + Technology Lab · 2026"),
    # demo is inserted after this at runtime
    ("02-ac-native", 13.0, ["AC Native", "Bare-metal creative OS"],
        "PID 1 · 192 kHz · 32 voices · $50 per seat"),
    ("03-library", 20.0, ["AC Device Library"],
        "Lending fleet · AC Blank laptops at $128 · Flash Days · Family Play · public waitlist"),
    ("04-community", 15.0, ["19,000 commits", "17,000 KidLisp programs", "2,800 handles"],
        "aesthetic.computer · active since 2021"),
    ("05-events", 6.0, ["boot the cohort", "play the room"],
        "2027 Symposium · 2028 Demo Day"),
    ("06-close", 6.64, ["Aesthetic.Computer"],
        "A civic instrument · aesthetic.computer/lacma-2026", 0),
]

print("→ generating title cards")
for name, dur, lines, subline, *rest in cards_spec:
    pink_idx = rest[0] if rest else None
    card(CARDS / f"{name}.jpg", lines, subline, pink_idx)

# --- Encode each card as a silent video segment of the right duration
segments = []
for i, (name, dur, *_) in enumerate(cards_spec):
    seg = CARDS / f"seg-{name}.mp4"
    subprocess.run([
        "ffmpeg", "-y",
        "-loop", "1", "-framerate", "30",
        "-i", str(CARDS / f"{name}.jpg"),
        "-f", "lavfi", "-i", "anullsrc=r=48000:cl=stereo",
        "-t", str(dur),
        "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
        "-c:a", "aac", "-b:a", "160k",
        str(seg),
    ], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    segments.append(seg)
    print(f"  {seg.name}")
    # Insert demo after hook (index 0)
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
        print(f"  {demo_seg.name}")

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

# --- Build the composite VO track:
#   0-10.8   : vo-hook
#   10.8-32.8: silence (demo plays)
#   32.8-93.4: vo-main
vo_track = CARDS / "vo-track.wav"
print("→ composing VO track with silence gap for demo")
subprocess.run([
    "ffmpeg", "-y",
    "-i", str(VO_HOOK),
    "-i", str(VO_MAIN),
    "-filter_complex",
    # Pad hook to 10.8, then 22s silence, then main
    "[0:a]apad=whole_dur=10.8[p1];"
    "aevalsrc=0:d=22[s1];"
    "[1:a]anull[p2];"
    "[p1][s1][p2]concat=n=3:v=0:a=1[vo]",
    "-map", "[vo]",
    "-ar", "48000", "-ac", "2",
    str(vo_track),
], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

# --- Build SRT captions (readable short cues)
# Timing references: hook=0-10.8, main starts at 32.8 (after 22s demo).
# Silences found in vo-main.wav via silencedetect -n -30dB -d 0.4:
#   9.1, 13.4, 17.5, 22.8, 29.9, 33.4, 37.5, 41.8, 44.5, 49.4, 52.7, 54.7, 57.2, 59.0
# These approximately map to sentence ends.
def ts(t):
    h = int(t // 3600); t -= h*3600
    m = int(t // 60); t -= m*60
    s = int(t)
    ms = int((t - s) * 1000)
    return f"{h:02d}:{m:02d}:{s:02d},{ms:03d}"

cues = [
    # Hook (0-10.8)
    (0.0, 4.0, "Personal computers\nhave not been very personal."),
    (4.0, 7.0, "Windows 10 end-of-life just stranded\nhundreds of millions of laptops."),
    (7.0, 10.7, "There has never been a better time\nto develop new software for old hardware."),
    # Demo 10.8-32.8, no captions (demo has baked ones)
    # Main (starts at 32.8)
    (32.8, 36.0, "This is AC Native."),
    (36.0, 40.0, "A bare-metal creative\ncomputing operating system."),
    (40.0, 44.0, "No desktop. No app store.\nFifty dollars per seat."),
    (44.0, 50.0, "For the LACMA Art and Technology Lab,\nwe propose growing it into a public device library."),
    (50.0, 56.0, "A lending fleet of AC Blank laptops\nflashed with our OS."),
    (56.0, 62.0, "Circulating through Flash Days, workshops,\nand Family Play afternoons at LACMA."),
    (62.0, 66.0, "A public waitlist."),
    (66.0, 72.0, "The library welcomes artists flashing\ntheir own custom creative operating systems,"),
    (72.0, 76.0, "joining a tradition of\nartist-run device libraries."),
    (76.0, 80.0, "Aesthetic Computer has been under\nactive development since 2021."),
    (80.0, 84.0, "Over nineteen thousand commits.\nSeventeen thousand KidLisp programs."),
    (84.0, 87.0, "Twenty-eight hundred registered handles."),
    (87.0, 89.5, "At the 2027 Symposium\nwe boot the cohort."),
    (89.5, 91.8, "At the 2028 Demo Day\nwe play the room."),
    (91.8, 93.3, "Aesthetic.Computer.\nA civic instrument."),
]
srt_path = CARDS / "captions.srt"
with open(srt_path, "w") as f:
    for i, (s, e, text) in enumerate(cues, 1):
        f.write(f"{i}\n{ts(s)} --> {ts(e)}\n{text}\n\n")
print(f"→ SRT: {srt_path.name} ({len(cues)} cues)")

# --- Final composite: concat + burn subtitles + mix audio
print("→ final encode with captions + audio mix")
subprocess.run([
    "ffmpeg", "-y",
    "-i", str(concat_raw),           # 0: video with demo's audio
    "-i", str(vo_track),             # 1: VO track
    "-filter_complex",
    # Burn subtitles on video
    f"[0:v]subtitles='{srt_path}':force_style='FontName=Helvetica,FontSize=22,"
    "PrimaryColour=&H00e4d8bc,OutlineColour=&H001c1812,BackColour=&Hcc1c1812,"
    "BorderStyle=3,Outline=2,Shadow=0,Alignment=2,MarginV=50'[v];"
    # Duck source audio (notepat C note) to -12dB when VO exists; mix.
    "[0:a]volume=0.25[bg];"
    "[1:a]volume=1.35[vo];"
    "[bg][vo]amix=inputs=2:duration=first:dropout_transition=0.3[a]",
    "-map", "[v]", "-map", "[a]",
    "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "192k",
    "-movflags", "+faststart",
    str(OUT),
], check=True)

dur = subprocess.check_output([
    "ffprobe", "-v", "error", "-show_entries", "format=duration",
    "-of", "default=nokey=1:noprint_wrappers=1", str(OUT),
]).decode().strip()
print(f"✓ {OUT}")
print(f"  size: {OUT.stat().st_size / 1024 / 1024:.1f} MB")
print(f"  duration: {float(dur):.1f}s")
