"""Versions of Fía's masters for choir: colourways, the app's asset
catalog entries, and a logo try-out board.

usage: python versions.py <set_dir> <xcassets_dir>
"""
import json
import re
import subprocess
import sys
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

set_dir, assets = Path(sys.argv[1]), Path(sys.argv[2])
index = {m["name"]: m for m in json.loads((set_dir / "index.json").read_text())}

# choir's own inks (Theme.swift): ink, the red head rule, paper.
INKS = {"ink": "#1c1816", "red": "#cf4040", "white": "#ffffff"}

# What the app uses, by asset name → master.
APP = {
    "WordChat": "choir-word-chat-03",
    "WordDiscover": "choir-word-discover-03",
    "WordReply": "choir-word-reply-01",
    "WordSend": "choir-word-send-02",
    "GlyphPlus": "choir-glyph-plus-01",
    "GlyphClose": "choir-glyph-x-03",
    "GlyphQuestion": "choir-glyph-question-03",
    "GlyphBang": "choir-glyph-bang-01",
    "GlyphAsterisk": "choir-glyph-asterisk-01",
    "GlyphSendC": "choir-glyph-c-arrow-02",
    "DaisyLoop": "choir-daisy-loop-17",
    "DaisyPetal": "choir-daisy-petal-o-01",
}
# Logo candidates for the try-out board.
LOGOS = ["choir-daisy-loop-17", "choir-daisy-loop-06", "choir-daisy-loop-09",
         "choir-daisy-petal-o-01", "choir-daisy-petal-dot-03", "choir-daisy-petal-x-01"]

def recolor(svg_text, hex_color):
    return re.sub(r'fill="#[0-9a-fA-F]{6}"', f'fill="{hex_color}"', svg_text)

def render(svg_path, width, out_png):
    subprocess.run(["magick", "-density", "300", "-background", "none", str(svg_path),
                    "-resize", f"{width}x{width}", str(out_png)], check=True)

# 1. Colourways for every app pick and logo candidate: SVG + 1024 PNG.
versions = set_dir / "versions"
versions.mkdir(exist_ok=True)
picks = sorted(set(APP.values()) | set(LOGOS))
for name in picks:
    svg = (set_dir / "svg" / f"{name}.svg").read_text()
    for ink, hex_color in INKS.items():
        out_svg = versions / f"{name}-{ink}.svg"
        out_svg.write_text(recolor(svg, hex_color))
        render(out_svg, 1024, versions / f"{name}-{ink}.png")

# 2. The asset catalog: one vector imageset per app asset, template-rendered
#    so SwiftUI tints it like the existing Mark.
for asset, name in APP.items():
    folder = assets / f"{asset}.imageset"
    folder.mkdir(exist_ok=True)
    (folder / f"{name}.svg").write_text((set_dir / "svg" / f"{name}.svg").read_text())
    (folder / "Contents.json").write_text(json.dumps({
        "images": [{"filename": f"{name}.svg", "idiom": "universal"}],
        "info": {"author": "xcode", "version": 1},
        "properties": {"preserves-vector-representation": True, "template-rendering-intent": "template"},
    }, indent=2) + "\n")

# 3. Logo try-out board: each candidate as the app icon (an index card on the
#    desk, like today's), as a header lockup with "choir", and small.
DESK, PAPER, RULE, HEAD, INK = (226, 219, 204), (255, 255, 255), (150, 186, 224), (207, 64, 64), (28, 24, 22)
font_path = "/System/Library/Fonts/Supplemental/AmericanTypewriter.ttc"
word = ImageFont.truetype(font_path, 54)
label = ImageFont.truetype(font_path, 22)
row_h, cols = 300, [40, 330, 860]
board = Image.new("RGB", (1100, 90 + row_h * len(LOGOS)), DESK)
d = ImageDraw.Draw(board)
d.text((40, 30), "choir — logo try-outs from Fía's daisies (2026-10-12)", font=label, fill=INK)
tmp = set_dir / ".tryout.png"
for r, name in enumerate(LOGOS):
    y = 90 + r * row_h
    # The icon: a 220 px card with a red head rule and blue lines.
    card = Image.new("RGB", (220, 220), PAPER)
    cd = ImageDraw.Draw(card)
    cd.line([(0, 34), (220, 34)], fill=HEAD, width=2)
    for ly in range(58, 220, 22):
        cd.line([(0, ly), (220, ly)], fill=RULE, width=1)
    render(versions / f"{name}-ink.svg", 150, tmp)
    mark = Image.open(tmp).convert("RGBA")
    card.paste(mark, ((220 - mark.width) // 2, 44 + (176 - mark.height) // 2), mark)
    board.paste(card, (cols[0], y))
    # The header lockup: mark then the word, on paper.
    lock = Image.new("RGB", (480, 120), PAPER)
    render(versions / f"{name}-ink.svg", 96, tmp)
    mark = Image.open(tmp).convert("RGBA")
    lock.paste(mark, (16, (120 - mark.height) // 2), mark)
    ImageDraw.Draw(lock).text((130, 26), "choir", font=word, fill=INK)
    board.paste(lock, (cols[1], y + 20))
    # Small: 44 and 22 px, ink and red.
    for i, (size, ink) in enumerate([(44, "ink"), (22, "ink"), (44, "red")]):
        render(versions / f"{name}-{ink}.svg", size, tmp)
        mark = Image.open(tmp).convert("RGBA")
        board.paste(mark, (cols[2] + i * 70, y + 60 + (44 - mark.height) // 2), mark)
    d.text((cols[1], y + 160), name.replace("choir-", ""), font=label, fill=INK)
tmp.unlink()
board.save(set_dir / "logo-tryouts.png")
print(f"{len(picks)} picks × {len(INKS)} inks, {len(APP)} imagesets, {len(LOGOS)} logo try-outs")
