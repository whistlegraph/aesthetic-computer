#!/usr/bin/env python3
"""
Assemble the LACMA 2026 grant video from title cards + existing notepat
demo, with VO overlaid. Single ffmpeg pipeline, no manual mixing.

Output: lacma-grant-video.mp4 (1280x800, H.264, MP3 audio)
"""

import subprocess, os, sys
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

HERE = Path(__file__).parent
CARDS = HERE / "video-cards"
CARDS.mkdir(exist_ok=True)
OUT = HERE / "lacma-grant-video.mp4"
DEMO = Path("/Users/jas/aesthetic-computer/system/public/assets/lacma-2026/ac-native-demo-narrated.mp4")
VO = HERE / "video-vo-chris.wav"

W, H = 1280, 800
CREAM = (240, 230, 207)  # AC parchment
DARK = (61, 46, 31)      # AC sepia ink
PINK = (165, 73, 105)    # AC pink
DIM = (138, 115, 86)

# --- Fonts: use SF Pro Display bundled with macOS if available
FONT_HEAVY = "/System/Library/Fonts/SFNS.ttf"  # San Francisco
FONT_MONO = "/System/Library/Fonts/SFNSMono.ttf"
# Fallbacks
for f in ["/System/Library/Fonts/Helvetica.ttc",
          "/System/Library/Fonts/Supplemental/Arial.ttf"]:
    if not Path(FONT_HEAVY).exists():
        FONT_HEAVY = f

def load_font(path, size):
    try:
        return ImageFont.truetype(path, size)
    except Exception:
        return ImageFont.load_default()

def card(out_path, lines, subline=None, pink_line_idx=None, tiny_label=None):
    img = Image.new("RGB", (W, H), CREAM)
    d = ImageDraw.Draw(img)

    # Optional tiny top label (like "§ 01" / "LIBRARY")
    if tiny_label:
        f = load_font(FONT_HEAVY, 20)
        tw = d.textlength(tiny_label, font=f)
        d.text((W/2 - tw/2, 70), tiny_label, font=f, fill=DIM)

    # Main lines — stacked, centered, big
    total_h = 0
    sizes = []
    for line in lines:
        # Auto-scale font so the widest line fills ~82% of width.
        fs = 92
        f = load_font(FONT_HEAVY, fs)
        while d.textlength(line, font=f) > W * 0.82 and fs > 24:
            fs -= 4
            f = load_font(FONT_HEAVY, fs)
        # But also cap font so short lines don't go absurdly big.
        if d.textlength(line, font=f) < W * 0.3 and fs < 92:
            fs = min(92, fs + 12)
            f = load_font(FONT_HEAVY, fs)
        sizes.append((line, fs, f))
        total_h += fs + 18

    y = (H - total_h) / 2 - (20 if subline else 0)
    for i, (line, fs, f) in enumerate(sizes):
        tw = d.textlength(line, font=f)
        color = PINK if (pink_line_idx is not None and i == pink_line_idx) else DARK
        d.text((W/2 - tw/2, y), line, font=f, fill=color)
        y += fs + 18

    # Optional subline
    if subline:
        f = load_font(FONT_HEAVY, 26)
        tw = d.textlength(subline, font=f)
        d.text((W/2 - tw/2, y + 10), subline, font=f, fill=DIM)

    # Bottom pink accent bar
    d.rectangle([(W/2 - 40, H - 60), (W/2 + 40, H - 56)], fill=PINK)

    img.save(out_path, quality=95)
    print(f"  wrote {out_path.name}")

# --- TITLE CARD SPECS (line list, optional subline, pink line index) ---
cards_spec = [
    # card 1: hook (0:00-0:15)
    {"name": "01-hook.jpg", "dur": 15, "lines": ["Personal computers",
                                                  "have not been very personal."],
                                        "subline": "LACMA Art + Technology Lab · 2026"},
    # card 2: thesis (after hook, before demo) — nope, we skip straight to demo.

    # --- DEMO is 22s (0:15-0:37) ---

    # card 3: what is AC Native (0:37-0:57)
    {"name": "02-ac-native.jpg", "dur": 20,
     "lines": ["AC Native", "Bare-metal creative OS"],
     "subline": "$50 per seat · 240 million x86_64 laptops stranded · never been a better time"},

    # card 4: library (0:57-1:22)
    {"name": "03-library.jpg", "dur": 25,
     "lines": ["AC Device Library"],
     "subline": "Lending fleet · AC Blank laptops at $128 · Flash Days · Family Play · public waitlist"},

    # card 5: community (1:22-1:40)
    {"name": "04-community.jpg", "dur": 18,
     "lines": ["19,000 commits", "17,000 KidLisp programs", "2,800 handles"],
     "subline": "aesthetic.computer · since 2021"},

    # card 6: events (1:40-1:58)
    {"name": "05-events.jpg", "dur": 18,
     "lines": ["2027 Symposium: boot the cohort",
               "2028 Demo Day: play the room"],
     "subline": "Flash days · workshops · family play at LACMA"},

    # card 7: close (1:58-end)
    {"name": "06-close.jpg", "dur": 8,
     "lines": ["Aesthetic.Computer"],
     "pink_line_idx": 0,
     "subline": "A civic instrument · aesthetic.computer/lacma-2026"},
]

print("→ generating title cards...")
for spec in cards_spec:
    card(CARDS / spec["name"],
         spec["lines"],
         subline=spec.get("subline"),
         pink_line_idx=spec.get("pink_line_idx"),
         tiny_label=spec.get("tiny_label"))

# --- Now build the video timeline.
# Each card becomes an image-loop segment of spec["dur"] seconds.
# The hero demo slots in between card 1 and card 2.

# We'll create a concat file listing sources.
concat_lines = []
tmp_segments = []
for i, spec in enumerate(cards_spec):
    seg = CARDS / f"seg-{i:02d}-{spec['name'].replace('.jpg','')}.mp4"
    print(f"→ encoding {seg.name} ({spec['dur']}s)")
    subprocess.run([
        "ffmpeg", "-y",
        "-loop", "1", "-framerate", "30",
        "-i", str(CARDS / spec["name"]),
        "-f", "lavfi", "-i", "anullsrc=r=48000:cl=stereo",
        "-t", str(spec["dur"]),
        "-c:v", "libx264", "-preset", "medium", "-crf", "18",
        "-pix_fmt", "yuv420p",
        "-c:a", "aac", "-b:a", "160k",
        str(seg),
    ], check=True)
    tmp_segments.append(seg)

    # Insert the hero demo after card 1 (index 0 in cards_spec).
    if i == 0:
        hero = CARDS / "seg-01-demo.mp4"
        print(f"→ copying hero demo → {hero.name}")
        # Re-encode to ensure compatible params (30 fps, matching audio setup)
        subprocess.run([
            "ffmpeg", "-y", "-i", str(DEMO),
            "-vf", "fps=30",
            "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
            "-c:a", "aac", "-b:a", "160k", "-ar", "48000",
            str(hero),
        ], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        tmp_segments.append(hero)

concat_txt = CARDS / "concat.txt"
with open(concat_txt, "w") as f:
    for seg in tmp_segments:
        f.write(f"file '{seg.name}'\n")

print("→ concatenating...")
concat_out = CARDS / "concat-no-vo.mp4"
subprocess.run([
    "ffmpeg", "-y", "-f", "concat", "-safe", "0", "-i", str(concat_txt),
    "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "160k",
    str(concat_out),
], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

# Now overlay the VO audio. The VO starts at 0:00 and plays over the
# concat. Demo's own audio (notepat C note) plays during 0:15-0:37;
# during that window we duck the demo audio by -12 dB so VO stays clear.
# But VO is only 1:24; concat is longer. Pad VO with silence.
#
# Simpler: mix VO with demo audio, using amix with weights.
print("→ overlaying VO...")
subprocess.run([
    "ffmpeg", "-y",
    "-i", str(concat_out),
    "-i", str(VO),
    "-filter_complex",
    # Ducks source audio under VO by mixing at ratio 1:1 with demo quieter.
    "[0:a]volume=0.25[bg];[1:a]volume=1.3[vo];"
    "[bg][vo]amix=inputs=2:duration=first:dropout_transition=0.5[a]",
    "-map", "0:v", "-map", "[a]",
    "-c:v", "copy",
    "-c:a", "aac", "-b:a", "192k",
    "-movflags", "+faststart",
    str(OUT),
], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

print(f"✓ {OUT}")
print(f"  size: {OUT.stat().st_size / 1024 / 1024:.1f} MB")
# Duration check
dur = subprocess.check_output([
    "ffprobe", "-v", "error", "-show_entries", "format=duration",
    "-of", "default=nokey=1:noprint_wrappers=1", str(OUT),
]).decode().strip()
print(f"  duration: {float(dur):.1f}s")
