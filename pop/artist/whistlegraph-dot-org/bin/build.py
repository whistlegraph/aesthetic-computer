"""Artist-page assets for Whistlegraph Dot Org.

Whistlegraph Dot Org is the sibling imprint to Aesthetic Dot Computer, and its
Spotify page is dressed the same way: a candid photograph across the header,
a drawn mark for the avatar. The photographs are real working shots — ink,
markers, a notebook, a hand still holding the pen — because a whistlegraph is
a drawing you sing, and the drawing is the score.

Header crops are given as fractions of the source frame so they survive a
re-export at a different resolution. Spotify overlays the artist name in large
white type across the lower left, so every crop keeps that corner dark and
uncluttered; `--proof` renders that overlay so a crop can be judged the way a
listener will see it.

Sources live in refs/ and are deliberately untracked (see .gitignore) — they
are personal photographs, and one of them carries a street address well outside
any crop published here.

    python3 bin/build.py            # build every asset
    python3 bin/build.py --proof    # also write header proofs with name overlay
"""
import os
import sys

import numpy as np
from PIL import Image, ImageDraw, ImageFilter, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
REFS = os.path.join(ROOT, "refs")
WG = "/Users/jas/aesthetic-computer/system/public/whistlegraph.org"

# Spotify's published minimums are 750x750 for the avatar and 2660x1140 for the
# header. We build well above both so the same files serve Apple Music for
# Artists, Tidal and a YouTube Official Artist Channel without a second pass.
AVATAR = 3000
HEADER = (2660, 1140)
GALLERY = 2048

# name, source, (centre x, centre y, width) as fractions of the frame, lift
#
# `lift` raises the shadows on frames shot in a dim room; Spotify darkens the
# lower half of a header behind the artist name, and a photograph that is
# already black down there goes to mud.
#
# The fifth candid, dayscore-wide.jpeg, is deliberately absent: its notebook
# page carries a street address inside every crop that also keeps the drawing.
HEADERS = [
    ("source", "source-score-bright.jpeg", (0.500, 0.380, 1.000), 0.00),
    ("penbed", "print-in-hand.jpeg", (0.440, 0.500, 0.800), 0.10),
    ("sketchbook", "sketchbook-ipad.jpg", (0.500, 0.440, 1.000), 0.00),
    ("sheets", "loose-sheets.jpeg", (0.520, 0.520, 0.920), 0.05),
]

# name, source, (centre x, centre y, width) as fractions of the frame
GALLERIES = [
    ("source-score", "source-score-bright.jpeg", (0.470, 0.470, 0.900)),
    ("loose-sheets", "loose-sheets.jpeg", (0.480, 0.520, 0.760)),
    ("studio", "studio-yellow.jpg", (0.500, 0.620, 0.980)),
    ("sketchbook", "sketchbook-ipad.jpg", (0.480, 0.560, 0.900)),
]


def band(img, cx, cy, w, out_w, out_h):
    """Crop a fraction-addressed band and resize it to exactly out_w x out_h."""
    W, H = img.size
    cw = int(w * W)
    ch = int(round(cw * out_h / out_w))
    if ch > H:  # the requested width is taller than the frame; fit to height
        ch = H
        cw = int(round(ch * out_w / out_h))
    x0 = int(np.clip(cx * W - cw / 2, 0, max(0, W - cw)))
    y0 = int(np.clip(cy * H - ch / 2, 0, max(0, H - ch)))
    return img.crop((x0, y0, x0 + cw, y0 + ch)).resize((out_w, out_h), Image.LANCZOS)


def square(img, cx, cy, w, side):
    return band(img, cx, cy, w, side, side)


def lift_shadows(img, amount):
    """Raise the darkest end of the curve without touching the highlights."""
    if not amount:
        return img
    a = np.array(img).astype(np.float32) / 255.0
    a = a + amount * (1 - a) ** 3
    return Image.fromarray((np.clip(a, 0, 1) * 255).astype(np.uint8))


def crisp_mark(side):
    """The magenta W, re-crisped from the 512px favicon.

    The mark is flat two-colour art — #FF24FF under a black outline — so it
    upscales cleanly if the fill and outline are carried as masks and
    re-thresholded rather than interpolated as pixels. Interpolating instead
    leaves a grey halo that reads as a blurry logo at avatar size.
    """
    src = Image.open(os.path.join(WG, "favicon.png")).convert("RGBA")
    a = np.array(src).astype(np.float32)
    alpha = a[..., 3] / 255.0
    # Black outline vs magenta fill, judged on RED: the fill is #FF24FF, so its
    # green channel is as dark as the outline's and only red separates them.
    ink = (a[..., 0] < 96) * alpha

    big = lambda m: np.array(
        Image.fromarray((m * 255).astype(np.uint8)).resize(
            (side, side), Image.LANCZOS
        )
    ).astype(np.float32) / 255.0

    A, I = big(alpha), big(ink)
    A = np.clip((A - 0.5) * 8 + 0.5, 0, 1)  # smoothstep the silhouette
    I = np.clip((I - 0.5) * 8 + 0.5, 0, 1)

    fill = np.zeros((side, side, 3), np.float32)
    fill[..., 0], fill[..., 1], fill[..., 2] = 1.0, 0x24 / 255, 1.0
    rgb = fill * (1 - I[..., None])  # ink is black, so just darken the fill
    return rgb, A


def avatar(side=AVATAR):
    """The mark on the record's ground: deep indigo lifting to magenta."""
    y = np.linspace(0, 1, side, dtype=np.float32)[:, None]
    top = np.array([0x0B / 255, 0x08 / 255, 0x1E / 255], np.float32)
    bot = np.array([0x2A / 255, 0x04 / 255, 0x3C / 255], np.float32)
    ground = top + (bot - top) * (y**1.6)
    canvas = np.repeat(ground[:, None, :], side, axis=1)

    mark_side = int(side * 0.62)
    rgb, alpha = crisp_mark(mark_side)
    off = (side - mark_side) // 2
    # optical centring: the W's black outline sits low in its own box
    oy = off - int(side * 0.015)
    sl = (slice(oy, oy + mark_side), slice(off, off + mark_side))
    a = alpha[..., None]
    canvas[sl] = canvas[sl] * (1 - a) + rgb * a
    return Image.fromarray((np.clip(canvas, 0, 1) * 255).astype(np.uint8))


def proof(img, name="Whistlegraph Dot Org"):
    """Render Spotify's name overlay so a header can be judged in situ."""
    out = img.copy()
    W, H = out.size
    scrim = Image.new("L", (W, H), 0)
    d = ImageDraw.Draw(scrim)
    for i in range(H // 2):
        d.line([(0, H - 1 - i), (W, H - 1 - i)], fill=int(150 * (i / (H / 2)) ** 1.4))
    out = Image.composite(Image.new("RGB", (W, H), (0, 0, 0)), out, scrim)
    d = ImageDraw.Draw(out)
    size = int(H * 0.17)
    for path in (
        "/System/Library/Fonts/SFNS.ttf",
        "/System/Library/Fonts/Helvetica.ttc",
    ):
        try:
            font = ImageFont.truetype(path, size)
            break
        except OSError:
            font = None
    if font is None:
        font = ImageFont.load_default()
    d.text((int(W * 0.035), H - int(H * 0.17) - size), name, font=font, fill="white")
    return out


def main():
    want_proof = "--proof" in sys.argv
    os.makedirs(os.path.join(ROOT, "proofs"), exist_ok=True)

    p = os.path.join(ROOT, "wgdo-avatar-3000.jpg")
    avatar().save(p, quality=95, subsampling=0)
    print("→", os.path.basename(p))

    for name, src, crop, lift in HEADERS:
        img = Image.open(os.path.join(REFS, src)).convert("RGB")
        out = lift_shadows(band(img, *crop, *HEADER), lift)
        p = os.path.join(ROOT, f"wgdo-header-{name}-2660.jpg")
        out.save(p, quality=95, subsampling=0)
        print("→", os.path.basename(p), f"from {src}")
        if want_proof:
            q = os.path.join(ROOT, "proofs", f"header-{name}.jpg")
            proof(out).save(q, quality=90)

    for name, src, crop in GALLERIES:
        img = Image.open(os.path.join(REFS, src)).convert("RGB")
        p = os.path.join(ROOT, f"wgdo-gallery-{name}-2048.jpg")
        square(img, *crop, GALLERY).save(p, quality=95, subsampling=0)
        print("→", os.path.basename(p), f"from {src}")


if __name__ == "__main__":
    main()
