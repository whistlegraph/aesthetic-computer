#!/usr/bin/env python
# eyecons.py — the stickers for Fia's Cut (perf-relight.mjs --fia): one PNG per sung noun in src/eyecons/<name>.png,
# all in one family — the picture, a white cutout border (~6 px at the delivered size) and a soft drop shadow — so an
# emoji and a photo cutout read as the same kind of thing. (Fia: "a cutout of Anne Hathaway's face and other floating
# identifiers … wiggling up like emoji or illustrations like clipart, to represent the nouns.")
#
# Sources, all licence-clean: Google's Noto Color Emoji bitmaps (Apache-2.0; 2D/png/512 in googlefonts/noto-emoji) and,
# for Anne Hathaway, a Wikimedia Commons photograph under CC BY-SA 2.0, her head lifted by Apple Vision through
# bin/vision-matte.swift (subject lifting), the neck faded below the chin. Credits are written to src/eyecons/CREDITS.md
# and must travel with the deliverable's description.
#
#   pop/.venv/bin/python pop/sailor-song/bin/eyecons.py            # fetches what is missing into src/eyecons/raw, builds all
#   … --only hathaway,bee                                           # just those
import io, os, re, subprocess, sys, tempfile, urllib.request
from PIL import Image, ImageFilter, ImageChops
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__)); LANE = os.path.dirname(HERE)
DIR = os.path.join(LANE, "src", "eyecons"); RAW = os.path.join(DIR, "raw"); os.makedirs(RAW, exist_ok=True)
UA = {"User-Agent": "sailor-song-fia-cut/1.0 (mail@aesthetic.computer)"}
NOTO = "https://raw.githubusercontent.com/googlefonts/noto-emoji/main/2D/png/512/emoji_u%s.png"
NOTO_LICENSE = "https://github.com/googlefonts/noto-emoji/blob/main/LICENSE"

# name → (what it stands for in the lyric, Noto codepoint sequence)
EMOJI = {
    "eyes":     ("saw",                 "1f440"),
    "pen":      ("pen",                 "1f58a"),
    "cough":    ("coughed",             "1f62e_200d_1f4a8"),
    "knees":    ("knees",               "1f9b5"),
    "baby":     ("Baby",                "1f476"),
    "kiss":     ("kiss",                "1f48b"),
    "mouth":    ("mouth",               "1f444"),
    "heart":    ("love",                "2764"),
    "sailboat": ("sailor",              "26f5"),
    "tongue":   ("taste",               "1f445"),
    "icecream": ("flavor",              "1f366"),
    "pray":     ("God",                 "1f64f"),
    "halo":     ("savior",              "1f607"),
    "mom":      ("mom",                 "1f469_200d_1f37c"),
    "worried":  ("worried",             "1f61f"),
    "sleep":    ("sleep",               "1f634"),
    "hourglass":("wait",                "23f3"),
    "bee":      ("sting",               "1f41d"),
    "blood":    ("bleeding",            "1fa78"),
    "runner":   ("run (away)",          "1f3c3"),
    "bricks":   ("walls",               "1f9f1"),
    "house":    ("house",               "1f3e0"),
    "cat":      ("cat",                 "1f431"),
    "mouse":    ("mouse",               "1f42d"),
    "infinity": ("forever",             "267e"),
    "chair":    ("sit (it out)",        "1fa91"),
}
# the photo cutout: file page, original URL, author, licence; the head's crop box in the original and the neck fade (y)
HATHAWAY = dict(
    page="https://commons.wikimedia.org/wiki/File:Anne_Hathaway_2011.jpg",
    url="https://upload.wikimedia.org/wikipedia/commons/e/e6/Anne_Hathaway_2011.jpg",
    author="Mingle MediaTV (flickr.com/photos/minglemediatv)", licence="CC BY-SA 2.0", licence_url="https://creativecommons.org/licenses/by-sa/2.0/",
    crop=(330, 0, 1010, 900), neck=(790, 870))

SIZE, CONTENT, BORDER = 512, 352, 18          # the canvas, the picture's box, the white border — ~6 px once drawn at 16 % of 1080

def fetch(url, path):
    if os.path.exists(path): return path
    print("  ↓", url); req = urllib.request.Request(url, headers=UA)
    with urllib.request.urlopen(req, timeout=60) as r, open(path, "wb") as f: f.write(r.read())
    return path

def sticker(src):
    """src: RGBA, straight alpha. → the sticker on a SIZE×SIZE transparent canvas."""
    src = src.copy(); src.thumbnail((CONTENT, CONTENT), Image.LANCZOS)
    can = Image.new("RGBA", (SIZE, SIZE), (0, 0, 0, 0))
    can.paste(src, ((SIZE - src.width) // 2, (SIZE - src.height) // 2 - 6), src)          # a hair up: room for the shadow
    a = can.split()[3]
    # the border: the alpha dilated by BORDER px (a blur's 2σ contour → round corners, not square ones), edge softened
    dil = a.filter(ImageFilter.GaussianBlur(BORDER / 2)).point(lambda v: 255 if v >= 6 else 0).filter(ImageFilter.GaussianBlur(1.1))
    border = Image.new("RGBA", (SIZE, SIZE), (255, 255, 255, 0)); border.putalpha(dil)
    # the shadow: the bordered shape, soft, down and to the right, 45 %
    sh_a = ImageChops.offset(dil, 9, 13).filter(ImageFilter.GaussianBlur(9)).point(lambda v: int(v * 0.45))
    shadow = Image.new("RGBA", (SIZE, SIZE), (10, 8, 14, 0)); shadow.putalpha(sh_a)
    out = Image.alpha_composite(shadow, border); out = Image.alpha_composite(out, can)
    return out

def hathaway():
    raw = fetch(HATHAWAY["url"], os.path.join(RAW, "Anne_Hathaway_2011.jpg"))
    im = Image.open(raw).convert("RGB").crop(HATHAWAY["crop"]); W, H = im.size
    exe = os.path.join(tempfile.gettempdir(), "vision-matte-still")
    if not os.path.exists(exe): subprocess.run(["swiftc", "-O", os.path.join(HERE, "vision-matte.swift"), "-o", exe], check=True)
    rgb = np.array(im, dtype=np.uint8).tobytes()
    m = subprocess.run([exe, str(W), str(H)], input=rgb, capture_output=True, check=True).stdout
    mask = np.frombuffer(m, dtype=np.uint8).reshape(H, W).astype(np.float32) / 255
    y0, y1 = HATHAWAY["neck"]; ys = np.arange(H, dtype=np.float32)[:, None]                                      # the neck fades out under the chin
    fade = np.clip((y1 - ys) / (y1 - y0), 0, 1); fade = fade * fade * (3 - 2 * fade); mask = mask * fade
    a = np.dstack([np.array(im), (mask * 255).astype(np.uint8)]); cut = Image.fromarray(a, "RGBA")
    bbox = cut.split()[3].point(lambda v: 255 if v > 8 else 0).getbbox(); cut = cut.crop(bbox)
    return cut

def main():
    only = None
    if "--only" in sys.argv: only = set(sys.argv[sys.argv.index("--only") + 1].split(","))
    rows = []
    for name, (noun, cp) in EMOJI.items():
        rows.append((name, noun, f"Noto Color Emoji U+{cp.upper().replace('_', ' U+')}", NOTO % cp, "Apache-2.0", NOTO_LICENSE))
        if only and name not in only: continue
        raw = fetch(NOTO % cp, os.path.join(RAW, f"emoji_u{cp}.png"))
        sticker(Image.open(raw).convert("RGBA")).save(os.path.join(DIR, f"{name}.png")); print("  ✓", name)
    rows.append(("hathaway", "Anne Hathaway", "photo: \"Anne Hathaway 2011.jpg\" by " + HATHAWAY["author"] + " (head cutout, cropped)", HATHAWAY["page"], HATHAWAY["licence"], HATHAWAY["licence_url"]))
    if not only or "hathaway" in only:
        sticker(hathaway()).save(os.path.join(DIR, "hathaway.png")); print("  ✓ hathaway")
    with open(os.path.join(DIR, "CREDITS.md"), "w") as f:
        f.write("# Eyecon credits — Sage's Sailor Song, Fia's Cut\n\nBuilt by bin/eyecons.py. These credits go in the deliverable's description.\n\n")
        f.write("| eyecon | stands for | source | file | licence |\n|---|---|---|---|---|\n")
        for name, noun, src, url, lic, licurl in rows: f.write(f"| {name} | {noun} | {src} | {url} | [{lic}]({licurl}) |\n")
        f.write("\n**Anne Hathaway cutout:** \"Anne Hathaway 2011.jpg\" by " + HATHAWAY["author"] + ", " + HATHAWAY["page"] + ", licensed " + HATHAWAY["licence"] + " (" + HATHAWAY["licence_url"] + "). Modified: cropped to her head, background removed, neck faded, white sticker border and shadow added. Under CC BY-SA the cutout itself is shared under the same licence.\n")
        f.write("\n**Emoji:** Noto Color Emoji by Google, https://github.com/googlefonts/noto-emoji, PNG bitmaps under the Apache License 2.0. Modified: white sticker border and shadow added.\n")
    print("✓", os.path.join(DIR, "CREDITS.md"))

main()
