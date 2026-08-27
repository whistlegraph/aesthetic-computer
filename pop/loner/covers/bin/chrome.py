"""Chrome sleeves built from the score's own final graphic, at full size.

@jeffrey on the solo-floor cover: "much larger loner, more contrast, more
metallic / shiny — more like the track." So the drawing is poured as metal
rather than chalk: the ink becomes a chrome rope lit from one side, and the
figure is scaled to fill the sleeve instead of sitting small in a wide room.

The line stays Camille's — every treatment here is a fill applied through the
extracted drawing's own mask, never a redrawing of it.
"""
import os
import numpy as np
from PIL import Image, ImageDraw, ImageFilter

HERE = os.path.dirname(os.path.abspath(__file__))
COVERS = os.path.dirname(HERE)
SIDE = 3000
SRC = Image.open(os.path.join(COVERS, "refs", "loner-final-figure.png")).convert("L")


def mask(height):
    w = round(SRC.width * height / SRC.height)
    g = SRC.resize((w, height), Image.LANCZOS)
    return Image.fromarray((255 - np.asarray(g)).astype(np.uint8))


def place(height, cx=0.5, top=0.06):
    """Full-size alpha for the figure, scaled and positioned on the sleeve."""
    m = mask(height)
    a = Image.new("L", (SIDE, SIDE), 0)
    a.paste(m, (int(SIDE * cx - m.width / 2), int(SIDE * top)))
    return a


def chrome_bar(stops):
    """A vertical metal ramp: [(pos, (r,g,b)), …] → SIDE-tall gradient image."""
    pos = np.array([s[0] for s in stops], float)
    cols = np.array([s[1] for s in stops], float)
    t = np.linspace(0, 1, SIDE)
    ramp = np.stack([np.interp(t, pos, cols[:, i]) for i in range(3)], axis=1)
    return np.repeat(ramp[:, None, :], SIDE, axis=1).astype(np.uint8)


def relief(alpha, dx, dy, radius):
    """Offset-blur of the mask — the shading trick that gives the rope volume."""
    b = alpha.filter(ImageFilter.GaussianBlur(radius))
    out = Image.new("L", (SIDE, SIDE), 0)
    out.paste(b, (dx, dy))
    return out


def metal(alpha, ramp, lit=(255, 255, 255), shade=(6, 6, 14), lift=26):
    """Light the figure like poured metal: top glint, underside shadow."""
    body = Image.fromarray(ramp).convert("RGBA")
    body.putalpha(alpha)

    a = np.asarray(alpha).astype(np.int16)
    top = np.asarray(relief(alpha, 0, lift, 9)).astype(np.int16)
    bot = np.asarray(relief(alpha, 0, -lift, 9)).astype(np.int16)
    hi = np.clip(a - top, 0, 255).astype(np.uint8)      # upper edge catches light
    lo = np.clip(a - bot, 0, 255).astype(np.uint8)      # lower edge falls into shadow

    sheet = Image.new("RGBA", (SIDE, SIDE), (0, 0, 0, 0))
    sheet.alpha_composite(body)
    hl = Image.new("RGBA", (SIDE, SIDE), lit + (0,)); hl.putalpha(Image.fromarray(hi))
    sh = Image.new("RGBA", (SIDE, SIDE), shade + (0,)); sh.putalpha(Image.fromarray(lo))
    sheet.alpha_composite(sh)
    sheet.alpha_composite(hl)
    return sheet


def backlight(im, alpha, warm, cool):
    """The house 3-layer separation: glow behind, vignette, then a rim halo."""
    glow = alpha.filter(ImageFilter.GaussianBlur(SIDE // 16))
    g = Image.new("RGBA", (SIDE, SIDE), warm + (0,))
    g.putalpha(Image.fromarray((np.asarray(glow) * 0.55).astype(np.uint8)))
    out = Image.alpha_composite(im, g)

    yy, xx = np.mgrid[0:SIDE, 0:SIDE].astype(float)
    r = np.hypot(xx - SIDE / 2, yy - SIDE / 2) / (SIDE * 0.72)
    vig = np.clip(r ** 2.1, 0, 1)
    v = Image.new("RGBA", (SIDE, SIDE), (0, 0, 0, 0))
    v.putalpha(Image.fromarray((vig * 205).astype(np.uint8)))
    out = Image.alpha_composite(out, v)

    ring = np.clip(np.asarray(alpha.filter(ImageFilter.GaussianBlur(SIDE // 220))).astype(np.int16)
                   - np.asarray(alpha).astype(np.int16), 0, 255)
    h = Image.new("RGBA", (SIDE, SIDE), cool + (0,))
    h.putalpha(Image.fromarray((ring * 0.85).astype(np.uint8)))
    return Image.alpha_composite(out, h)


def boards(base, tint):
    im = Image.new("RGBA", (SIDE, SIDE), base + (255,))
    d = ImageDraw.Draw(im)
    for i in range(SIDE // 30):
        d.line([(0, i * 30), (SIDE, i * 30)], fill=tint + (255,), width=1)
    return im


def lamp(im, cx, cy, colour, reach):
    pool = Image.new("RGBA", (SIDE, SIDE), (0, 0, 0, 0))
    d = ImageDraw.Draw(pool)
    for r, a in ((reach, 40), (int(reach * 0.6), 58), (int(reach * 0.3), 96)):
        d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=colour + (a,))
    im.alpha_composite(pool.filter(ImageFilter.GaussianBlur(SIDE // 34)))
    d2 = ImageDraw.Draw(im)
    d2.ellipse([cx - 20, cy - 13, cx + 20, cy + 13], fill=(255, 232, 250, 255))
    return im


def save(im, name):
    p = os.path.join(COVERS, f"lonerclub-cover-{name}.png")
    im.convert("RGB").save(p)
    print("→", os.path.basename(p))


# j2 · chrome floor — the big metal loner over the magenta lamp
def chrome_floor():
    a = place(int(SIDE * 0.88), cx=0.52, top=0.05)
    im = boards((7, 8, 20), (11, 13, 28))
    im = lamp(im, int(SIDE * 0.24), int(SIDE * 0.78), (236, 58, 168), int(SIDE * 0.40))
    ramp = chrome_bar([(0.00, (250, 250, 255)), (0.16, (150, 162, 190)),
                       (0.34, (250, 248, 255)), (0.52, (86, 96, 130)),
                       (0.70, (226, 232, 248)), (0.86, (60, 68, 98)),
                       (1.00, (198, 206, 230))])
    im = backlight(im, a, (232, 70, 172), (150, 220, 255))
    im.alpha_composite(metal(a, ramp))
    return im


# k · chrome flood — same metal, magenta-to-indigo ground, maximum contrast
def chrome_flood():
    a = place(int(SIDE * 0.92), cx=0.5, top=0.04)
    top, bot = np.array([12, 8, 34]), np.array([104, 14, 84])
    t = np.linspace(0, 1, SIDE)[:, None, None]
    im = Image.fromarray(np.repeat((top * (1 - t) + bot * t).astype(np.uint8), SIDE, axis=1)).convert("RGBA")
    ramp = chrome_bar([(0.00, (255, 252, 245)), (0.20, (196, 168, 210)),
                       (0.38, (255, 246, 250)), (0.56, (118, 78, 128)),
                       (0.74, (246, 226, 240)), (0.90, (74, 48, 90)),
                       (1.00, (232, 214, 236))])
    im = backlight(im, a, (255, 96, 190), (190, 240, 255))
    im.alpha_composite(metal(a, ramp, lit=(255, 255, 252)))
    return im


save(chrome_floor(), "j2-chrome-floor")
save(chrome_flood(), "k-chrome-flood")
