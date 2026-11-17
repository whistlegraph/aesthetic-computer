# choir promo graphics from Fía's speech bubbles, 26.10.26. Run cut.py first.
#   pop/.venv/bin/python marketing/choir/bubbles/compose.py   (after cut.py + legible.py)
# Writes to ~/Desktop/choir-bubbles/:
#   crowd-post.png    1080×1350  every bubble piled around the flower (IG post)
#   chat-story.png    1080×1920  the bubbles as one conversation (IG story)
#   grid-square.png   1080×1080  a tidy sheet of them on blue
#   sketch.png        2400×1800  the page re-laid flat on paper (Rhizome sketch upload)
import os, glob, math, random
import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "../../.."))
OUT = os.path.expanduser("~/Desktop/choir-bubbles"); os.makedirs(OUT, exist_ok=True)
DESK, INK, PAPER = (226, 219, 204), (28, 24, 22), (255, 255, 255)
BLUE, RED, BUTTER, SKY = (61, 120, 220), (207, 64, 64), (255, 219, 133), (184, 209, 255)
LOGO = Image.open(os.path.join(REPO, "system/public/choir/logo.png")).convert("RGBA")
BUBBLES = [Image.open(f).convert("RGBA") for f in sorted(glob.glob(os.path.join(HERE, "cut", "[0-9][0-9]-filled.png")))]

def font(px):
    for f in ["/System/Library/Fonts/Supplemental/AmericanTypewriter.ttc", "/System/Library/Fonts/Supplemental/Georgia.ttf"]:
        if os.path.exists(f): return ImageFont.truetype(f, px, index=0)
    return ImageFont.load_default()

def scaled(im, w):
    return im.resize((int(w), int(im.height * w / im.width)), Image.LANCZOS)

def tint(im, fill):
    """Recolor a bubble's white body, keep the ink."""
    a = np.array(im).astype(float); body = a[..., :3].mean(-1) > 128
    a[body, :3] = fill
    return Image.fromarray(a.astype(np.uint8))

def shadow(im, off=8):
    s = Image.new("RGBA", (im.width + off, im.height + off), (0, 0, 0, 0))
    sil = Image.new("RGBA", im.size, INK + (255,)); sil.putalpha(im.getchannel("A"))
    s.alpha_composite(sil, (off, off)); s.alpha_composite(im, (0, 0))
    return s

def centered(d, cx, y, text, f, fill=INK):
    w = d.textlength(text, font=f); d.text((cx - w / 2, y), text, font=f, fill=fill)

def pile(canvas, items, keepout, rng, tries=400, bleed=5):
    """Drop each bubble where it overlaps nothing placed so far (alpha masks)."""
    W, H = canvas.size; taken = np.zeros((H, W), bool)
    for x0, y0, x1, y1 in keepout: taken[y0:y1, x0:x1] = True
    for im in items:
        for _ in range(tries):
            x = rng.randint(-im.width // bleed, W - im.width + im.width // bleed) if bleed else rng.randint(40, W - im.width - 40)
            y = rng.randint(-im.height // bleed, H - im.height + im.height // bleed) if bleed else rng.randint(40, H - im.height - 40)
            m = np.array(im.getchannel("A")) > 0
            ya, xa = max(y, 0), max(x, 0); yb, xb = min(y + im.height, H), min(x + im.width, W)
            mm = m[ya - y:yb - y, xa - x:xb - x]
            if not (taken[ya:yb, xa:xb] & mm).any():
                taken[ya:yb, xa:xb] |= mm; canvas.alpha_composite(im, (x, y)); break

def crowd():
    rng = random.Random(7); W, H = 1080, 1350
    c = Image.new("RGBA", (W, H), DESK + (255,))
    fills = [PAPER, PAPER, PAPER, BUTTER, SKY, PAPER]
    items = []
    for i, b in enumerate(BUBBLES):  # each bubble once: no one says the same thing twice
        im = scaled(tint(b, fills[i % len(fills)]), rng.uniform(290, 360))
        items.append(shadow(im.rotate(rng.uniform(-9, 9), expand=True, resample=Image.BICUBIC), 6))
    items.sort(key=lambda im: -im.width * im.height)  # biggest first, so they all find room
    logo = scaled(LOGO, 300); lx, ly = (W - logo.width) // 2, 430
    pile(c, items, [(lx - 40, ly - 40, lx + logo.width + 40, ly + logo.height + 170)], rng, tries=3000)
    c.alpha_composite(logo, (lx, ly))
    d = ImageDraw.Draw(c)
    centered(d, W / 2, ly + logo.height + 6, "choir", font(84))
    centered(d, W / 2, ly + logo.height + 104, "comments on culture", font(34))
    c.convert("RGB").save(os.path.join(OUT, "crowd-post.png"))

def story():
    rng = random.Random(3); W, H = 1080, 1920
    c = Image.new("RGBA", (W, H), DESK + (255,)); d = ImageDraw.Draw(c)
    logo = scaled(LOGO, 150); c.alpha_composite(logo, ((W - logo.width) // 2, 70))
    centered(d, W / 2, 230, "choir", font(64))
    picks = [1, 3, 7, 12, 5, 9, 10, 15]  # a thread: hi → taxes → grant → accountant → … → Blick
    y, left = 340, True
    for n in picks:
        b = BUBBLES[n]
        im = shadow(scaled(tint(b, PAPER if left else SKY), rng.uniform(420, 500)), 7)
        if y + im.height > H - 170: continue
        x = 50 if left else W - im.width - 50
        c.alpha_composite(im, (x, y)); y += int(im.height * 0.78); left = not left
    centered(d, W / 2, H - 130, "a room of our own", font(46))
    centered(d, W / 2, H - 70, "choir.ac", font(34), fill=BLUE)
    c.convert("RGB").save(os.path.join(OUT, "chat-story.png"))

def grid():
    W = H = 1080; c = Image.new("RGBA", (W, H), BLUE + (255,)); d = ImageDraw.Draw(c)
    cols, cell = 4, W // 4
    rng = random.Random(11)
    for i, b in enumerate(BUBBLES[:16]):
        r, k = divmod(i, cols)
        im = scaled(b, cell - 34)
        im = im.rotate(rng.uniform(-6, 6), expand=True, resample=Image.BICUBIC)
        c.alpha_composite(shadow(im, 5), (k * cell + (cell - im.width) // 2, r * cell + (cell - im.height) // 2))
    c.convert("RGB").save(os.path.join(OUT, "grid-square.png"))

def sketch():
    rng = random.Random(5); W, H = 2400, 1800
    c = Image.new("RGBA", (W, H), (250, 248, 242, 255))
    d = ImageDraw.Draw(c)
    for x in range(30, W, 60):  # her dot grid, redrawn clean
        for y in range(30, H, 60): d.ellipse((x - 2, y - 2, x + 2, y + 2), fill=(200, 196, 188))
    items = [scaled(b, b.width * 0.95).rotate(rng.uniform(-4, 4), expand=True, resample=Image.BICUBIC) for b in BUBBLES]
    items.sort(key=lambda im: -im.width * im.height)
    pile(c, items, [], rng, tries=4000, bleed=0)
    c.convert("RGB").save(os.path.join(OUT, "sketch.png"))

for f in (crowd, story, grid, sketch): f()
print(OUT)
