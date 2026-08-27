"""Covers built from the score's own final graphic.

The last cell of Camille's LONER whistlegraph — the completed figure, the one
under "pass" — extracted at full resolution and composited directly, so the
drawing on the sleeve is the drawing itself and not a redrawing of it.
"""
import os
import numpy as np
from PIL import Image, ImageDraw, ImageFilter

HERE = os.path.dirname(os.path.abspath(__file__))
COVERS = os.path.dirname(HERE)
SIDE = 3000
FIG = Image.open(os.path.join(COVERS, "refs", "loner-final-figure.png")).convert("L")


def figure_mask(height):
    """The drawing as an alpha mask, scaled to `height` px tall."""
    w = round(FIG.width * height / FIG.height)
    g = FIG.resize((w, height), Image.LANCZOS)
    # ink is dark on white; invert into coverage
    return Image.fromarray((255 - np.asarray(g)).astype(np.uint8))


def paint(mask, colour):
    layer = Image.new("RGBA", mask.size, colour + (0,))
    layer.putalpha(mask)
    return layer


def glow(mask, radius, strength):
    g = mask.filter(ImageFilter.GaussianBlur(radius))
    return Image.fromarray((np.asarray(g).astype(float) * strength).clip(0, 255).astype(np.uint8))


def save(im, name):
    p = os.path.join(COVERS, f"lonerclub-cover-{name}.png")
    im.convert("RGB").save(p)
    print("→", os.path.basename(p))


# g · paper — the graphic as it exists, given room, on the score's own white
def paper():
    im = Image.new("RGBA", (SIDE, SIDE), (247, 246, 243, 255))
    m = figure_mask(int(SIDE * 0.66))
    im.alpha_composite(paint(m, (122, 124, 128)),
                       ((SIDE - m.width) // 2, int(SIDE * 0.17)))
    return im


# h · floor — one lamp under the figure, the club reading of the same drawing
def floor():
    im = Image.new("RGBA", (SIDE, SIDE), (10, 12, 26, 255))
    d = ImageDraw.Draw(im)
    for i in range(SIDE // 26):           # the boards, barely there
        y = i * 26
        d.line([(0, y), (SIDE, y)], fill=(15, 18, 34), width=1)

    m = figure_mask(int(SIDE * 0.60))
    x, y = (SIDE - m.width) // 2, int(SIDE * 0.20)
    foot = (x + int(m.width * 0.42), y + m.height)

    lamp = Image.new("RGBA", (SIDE, SIDE), (0, 0, 0, 0))
    ld = ImageDraw.Draw(lamp)
    for r, a in ((int(SIDE * 0.30), 34), (int(SIDE * 0.19), 46), (int(SIDE * 0.10), 70)):
        ld.ellipse([foot[0] - r, foot[1] - r, foot[0] + r, foot[1] + r], fill=(232, 62, 168, a))
    im.alpha_composite(lamp.filter(ImageFilter.GaussianBlur(SIDE // 42)))

    im.alpha_composite(paint(glow(m, SIDE // 150, 0.5), (236, 120, 190)), (x, y))
    im.alpha_composite(paint(m, (243, 240, 246)), (x, y))
    d.ellipse([foot[0] - 17, foot[1] - 11, foot[0] + 17, foot[1] + 11], fill=(255, 226, 246))
    return im


# i · pass — the ink flooded with the record's colour, edge to edge
def flood():
    top, bot = np.array([24, 20, 58]), np.array([118, 26, 96])
    ramp = np.linspace(0, 1, SIDE)[:, None, None]
    bg = (top * (1 - ramp) + bot * ramp).astype(np.uint8)
    im = Image.fromarray(np.repeat(bg, SIDE, axis=1)).convert("RGBA")
    m = figure_mask(int(SIDE * 0.74))
    x, y = (SIDE - m.width) // 2, int(SIDE * 0.13)
    im.alpha_composite(paint(glow(m, SIDE // 90, 0.42), (255, 214, 120)), (x, y))
    im.alpha_composite(paint(m, (255, 246, 232)), (x, y))
    return im


save(paper(), "g-paper")
save(floor(), "h-floor")
save(flood(), "i-flood")
