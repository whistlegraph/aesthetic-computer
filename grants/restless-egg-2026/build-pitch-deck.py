#!/usr/bin/env python3
"""
build-pitch-deck.py — the 10-slide Restless Egg product proposal deck.

Same brand language as the video (video/build-video.py): YWFT bold titles,
plain Arial body, AC palette on warm cream, felt stills (video/felt/*) as the
visual anchors. Renders ten 1920x1080 slides → deck/slide-NN.png and a combined
deck.pdf (the form wants a single ≤10-slide upload).

Content tracks the outline in FORM-ANSWERS.md.

Usage: python3 build-pitch-deck.py
"""

from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

Image.init()  # register encoders (PDF save needs JPEG registered explicitly)

HERE = Path(__file__).parent
FELT = HERE / "video" / "felt"
COVER = HERE / "figures" / "cover.png"
OUT = HERE / "deck"
OUT.mkdir(exist_ok=True)

W, H = 1920, 1080
CREAM = (240, 230, 207); INK = (44, 32, 50); DIM = (120, 100, 86)
CORAL = (208, 96, 84); VIOLET = (138, 100, 184); GREEN = (110, 168, 92)
ACCENTS = [CORAL, VIOLET, GREEN]

YWFT_BOLD = str(Path.home() / "Library" / "Fonts" / "ywft-processing-bold.ttf")
ARIAL = "/System/Library/Fonts/Supplemental/Arial.ttf"
ARIAL_BOLD = "/System/Library/Fonts/Supplemental/Arial Bold.ttf"
FALLBACK = "/System/Library/Fonts/SFNS.ttf"

M = 120  # margin


def font(path, size):
    try: return ImageFont.truetype(path, size)
    except: return ImageFont.truetype(FALLBACK, size)


def fit_cover(img, tw, th):
    sw, sh = img.size
    sr, tr = sw / sh, tw / th
    if sr > tr:
        nh = th; nw = int(th * sr)
    else:
        nw = tw; nh = int(tw / sr)
    img = img.resize((nw, nh), Image.LANCZOS)
    l = (nw - tw) // 2; t = (nh - th) // 2
    return img.crop((l, t, l + tw, t + th))


def wrap(d, text, f, maxw):
    words = text.split(" "); lines = []; cur = ""
    for w in words:
        trial = (cur + " " + w).strip()
        if d.textlength(trial, font=f) <= maxw:
            cur = trial
        else:
            if cur: lines.append(cur)
            cur = w
    if cur: lines.append(cur)
    return lines


# slide = (tag, title_lines, body_lines, image, accent_idx, hero)
#   hero=True → full-bleed image with title panel (slides 1 + 10)
SLIDES = [
    ("Restless Egg · Batch 2", ["Aesthetic, Inc."],
     ["instruments you play —", "not tools that make you efficient.",
      "", "notepat — the flagship instrument."],
     FELT / "felt-close.png", 0, True),

    ("01 · the problem", ["The problem"],
     ["Computing keeps getting optimized into a productivity appliance.",
      "Play is gated behind expensive hardware and rented software.",
      "Musicians, artists, and kids lose the one thing they want: something to play."],
     None, 0, False),

    ("02 · the product", ["notepat"],
     ["One expressive instrument, everywhere —",
      "browser · phone · desktop · DAW plugin · bare metal.",
      "Played with keyboard, touch, MIDI, even pressure.",
      "A 128-voice synthesizer, modeled from scratch — not samples."],
     FELT / "felt-instrument.png", 1, False),

    ("03 · the moat", ["We own", "the whole stack"],
     ["A bare-metal OS that boots as the very first process.",
      "A physical-modeling 128-voice synth running on $50 hardware.",
      "No one else makes an instrument that reaches this deep."],
     FELT / "felt-boot.png", 2, False),

    ("04 · traction", ["Traction"],
     ["~18,000 makers · 16,000+ programs made on the platform.",
      "Hacker News front page twice (notepat, No Paint).",
      "~20,000 commits over five years — fully open source.",
      "Honest gap: retention is still early. That's what these six months build."],
     FELT / "felt-commons.png", 0, False),

    ("05 · business", ["Who pays"],
     ["Musicians & creators → notepat pro, standalone app, DAW plugin.",
      "Schools & parents → learn-by-playing games (math, language, music).",
      "Later → refurbished hardware instruments at ~$50 per seat.",
      "Today they rent a patchwork: GarageBand, Ableton, $150 plugins, edtech apps."],
     None, 1, False),

    ("06 · the pipeline", ["A studio,", "not one app"],
     ["notepat → nom learning games → KidLisp → AC Native.",
      "One instrument-engine, many products.",
      "The open commons stays open; the company productizes instruments from it."],
     FELT / "felt-painter.png", 2, False),

    ("07 · why now", ["Why now"],
     ["AI makes everyone a programmer — taste becomes the moat.",
      "Our bare-metal OS just shipped.",
      "Windows 10 end-of-life is stranding hundreds of millions of capable laptops.",
      "Cheap capable hardware + new software = a new instrument category."],
     None, 0, False),

    ("08 · the founder", ["The founder"],
     ["Jeffrey Scudder — painter turned toolmaker.",
      "65+ live performances, from Café OTO to the Louisiana Museum.",
      "Collected by SMK (National Gallery of Denmark) and KADIST.",
      "A decade on one thesis: the computer is something you play."],
     FELT / "felt-commons.png", 1, False),

    ("09 · the ask", ["The ask"],
     ["Restless Egg's $100k anchors the existing Aesthetic, Inc. SAFE round.",
      "Six months → first product revenue + measurable retention.",
      "Shared Earnings keeps both paths open — durable business or venture scale.",
      "Let's make computing something you play again."],
     FELT / "felt-invitation.png", 0, True),
]


def render(slide, n):
    tag, title_lines, body, image, ai, hero = slide
    accent = ACCENTS[ai % len(ACCENTS)]
    img = Image.new("RGB", (W, H), CREAM)
    d = ImageDraw.Draw(img)

    if hero and image and Path(image).exists():
        # full-bleed felt image, dark gradient panel bottom for text
        bg = fit_cover(Image.open(image).convert("RGB"), W, H)
        img.paste(bg, (0, 0))
        d = ImageDraw.Draw(img, "RGBA")
        d.rectangle([0, H - 430, W, H], fill=(20, 12, 26, 150))
        # tag
        tf = font(ARIAL_BOLD, 30)
        d.text((M, H - 400), tag.upper(), font=tf, fill=accent)
        # title
        y = H - 350
        for line in title_lines:
            f = font(YWFT_BOLD, 120)
            d.text((M + 3, y + 3), line, font=f, fill=(0, 0, 0, 200))
            d.text((M, y), line, font=f, fill=CREAM); y += 128
        # body
        bf = font(ARIAL, 38)
        for line in body:
            if line:
                d.text((M, y + 12), line, font=bf, fill=(238, 230, 214));
            y += 50
        img.save(OUT / f"slide-{n:02d}.png", quality=95); return

    # standard slide: text column + optional felt panel on the right
    has_img = image and Path(image).exists()
    text_w = (980 if has_img else 1500)
    # kicker tag + rule
    tf = font(ARIAL_BOLD, 30)
    d.text((M, M - 8), tag.upper(), font=tf, fill=accent)
    d.rectangle([M, M + 34, M + 64, M + 40], fill=accent)
    # title
    y = M + 78
    for line in title_lines:
        f = font(YWFT_BOLD, 104)
        while d.textlength(line, font=f) > text_w and f.size > 48:
            f = font(YWFT_BOLD, f.size - 4)
        d.text((M, y), line, font=f, fill=INK); y += f.size + 12
    y += 28
    # body (wrapped)
    bf = font(ARIAL, 38)
    for line in body:
        if not line:
            y += 26; continue
        for i, wl in enumerate(wrap(d, line, bf, text_w)):
            if i == 0:
                d.ellipse([M, y + 16, M + 12, y + 28], fill=accent)
                d.text((M + 34, y), wl, font=bf, fill=INK)
            else:
                d.text((M + 34, y), wl, font=bf, fill=INK)
            y += 50
        y += 18
    # right felt panel
    if has_img:
        pw, ph = 690, 760
        px, py = W - M - pw, (H - ph) // 2
        panel = fit_cover(Image.open(image).convert("RGB"), pw, ph)
        img.paste(panel, (px, py))
        ImageDraw.Draw(img).rectangle([px, py, px + pw - 1, py + ph - 1], outline=accent, width=4)
    # footer
    ff = font(ARIAL, 24)
    d = ImageDraw.Draw(img)
    d.text((M, H - 70), "Aesthetic, Inc.  ·  notepat  ·  Restless Egg Batch 2", font=ff, fill=DIM)
    d.text((W - M - 90, H - 70), f"{n} / 10", font=ff, fill=DIM)
    img.save(OUT / f"slide-{n:02d}.png", quality=95)


print("→ rendering 10 slides")
imgs = []
for i, s in enumerate(SLIDES, 1):
    render(s, i)
    imgs.append(Image.open(OUT / f"slide-{i:02d}.png").convert("RGB"))
    print(f"  slide-{i:02d}.png")

pdf = HERE / "restless-egg-deck.pdf"
imgs[0].save(pdf, save_all=True, append_images=imgs[1:], resolution=150.0)
print(f"✓ {pdf}  ({len(imgs)} slides)")
