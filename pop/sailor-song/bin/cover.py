#!/usr/bin/env python3
"""cover.py — the Sailor Song cover: one still from the take, mastered.

The source is a 960×540 iPhone HLG (bt2020) clip. The frame is read as
16-bit, taken through the HLG inverse OETF + OOTF, bt2020→bt709, and a
soft Reinhard shoulder (exposure 2.6), then graded for the remix (v8: room
white-balanced near neutral, shirt pushed deep red and the LED string lit
violet through feathered hue masks, hard S, blacks down), upscaled locally
(lanczos + mild unsharp; nothing leaves this machine) and grained to
hide the upscale.

  pop/.venv/bin/python pop/sailor-song/bin/cover.py [--t 11.583] [--x 110] [--src src/take.mov] [--out cover/stem]
  → <stem>.jpg (clean) + <stem>-title.jpg; default stem cover/sailor-song-cover
"""
import argparse, os, subprocess
import numpy as np
from PIL import Image, ImageDraw, ImageFilter, ImageFont
import scipy.ndimage as nd

ap = argparse.ArgumentParser()
ap.add_argument("--t", type=float, default=107.15)   # v6.2: eyes open at the lens, mouth open on a held note (chorus 2)
ap.add_argument("--x", type=int, default=80)         # square crop left edge (of 960)
ap.add_argument("--size", type=int, default=3000)
ap.add_argument("--debug", default="")                # v8: dump the grade masks here
ap.add_argument("--src", default="")                  # the take; default src/take.mov (the Desktop IMG_8699.mov copy is gone)
ap.add_argument("--out", default="")                  # output stem; default cover/sailor-song-cover
a = ap.parse_args()
LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(LANE, "cover"); os.makedirs(OUT, exist_ok=True)
SRC = a.src or os.path.join(LANE, "src", "take.mov")
STEM = a.out or os.path.join(OUT, "sailor-song-cover"); os.makedirs(os.path.dirname(STEM), exist_ok=True)
W, H = 960, 540

raw = subprocess.run(["ffmpeg", "-v", "error", "-ss", str(a.t), "-i", SRC,
    "-vf", "scale=in_color_matrix=bt2020:in_range=tv:out_range=pc,format=rgb48le",
    "-frames:v", "1", "-f", "rawvideo", "-"], capture_output=True, check=True).stdout
im = np.frombuffer(raw, dtype="<u2").reshape(H, W, 3).astype(np.float64) / 65535.0

# ── HLG → SDR ───────────────────────────────────────────────────────────
A, B, C = 0.17883277, 0.28466892, 0.55991073
E = np.where(im <= 0.5, im ** 2 / 3.0, (np.exp((np.clip(im, 0.5, 1) - C) / A) + B) / 12.0)
Y = 0.2627 * E[..., 0] + 0.6780 * E[..., 1] + 0.0593 * E[..., 2]
E = E * (np.maximum(Y, 1e-6) ** 0.2)[..., None]                       # OOTF, γ 1.2
M = np.array([[1.6605, -0.5876, -0.0728], [-0.1246, 1.1329, -0.0083], [-0.0182, -0.1006, 1.1187]])
L = np.clip(np.einsum("hwc,dc->hwd", E, M), 0, None) * 2.6
L = np.clip(L / (1 + L / 2.2) * (1 + 1 / 2.2), 0, 1)                 # linear, 0..1

# ── crop + upscale (in linear light) ────────────────────────────────────
L = L[:, a.x:a.x + H]
chans = []
for c in range(3):
    ch = Image.fromarray((L[..., c] ** (1 / 2.2) * 255).astype(np.float32))
    chans.append(np.asarray(ch.resize((a.size, a.size), Image.LANCZOS)))
g = np.clip(np.stack(chans, -1) / 255.0, 0, 1) ** 2.2                 # back to linear

# ── v7: remove the ceiling light (inpaint from the ceiling around it) ───
# lamp centre / radii in the 540-square, scaled with the output
cy, cx, ry, rx = 0.07 * a.size, 0.43 * a.size, 0.10 * a.size, 0.125 * a.size
yy, xx = np.mgrid[0:a.size, 0:a.size]
d = ((yy - cy) / ry) ** 2 + ((xx - cx) / rx) ** 2
hole = np.clip((1.25 - d) / 0.35, 0, 1)                                # 1 inside, feathered edge
keep = 1 - hole
sig = 0.05 * a.size
fill = np.stack([nd.gaussian_filter(g[..., c] * keep, sig) for c in range(3)], -1) / (nd.gaussian_filter(keep, sig)[..., None] + 1e-6)
fill += (np.random.default_rng(1).standard_normal(fill.shape) * 0.006)  # a breath of texture so it is not plastic
g = g * (1 - hole[..., None]) + fill * hole[..., None]

# ── grade ───────────────────────────────────────────────────────────────
s = a.size / 540
lum = np.einsum("hwc,c->hw", g, np.array([0.2126, 0.7152, 0.0722]))
glow = nd.gaussian_filter(np.clip(g - 0.55, 0, None), sigma=(18 * s, 18 * s, 0))
g = 1 - (1 - g) * (1 - 0.55 * glow)                                  # highlight bloom (screen)
# ── v8 grade: neutral room, targeted shirt / wood / LED / skin pushes (replaces
# the v7 global warm multiply and the "redness" lift that pulled skin along) ──
def _hsv(x):
    """hue (deg), sat, val of a gamma-space rgb array."""
    mx = x.max(-1); mn = x.min(-1); dl = mx - mn + 1e-6
    r_, g_, b_ = x[..., 0], x[..., 1], x[..., 2]
    h = np.where(mx == r_, (g_ - b_) / dl, np.where(mx == g_, 2 + (b_ - r_) / dl, 4 + (r_ - g_) / dl)) * 60 % 360
    return h, dl / (mx + 1e-6), mx
def _ramp(x, lo, hi): return np.clip((x - lo) / (hi - lo), 0, 1)
def _hue_win(h, c, half, soft):
    """1 within ±half° of hue c, feathered to 0 over `soft` degrees outside."""
    dh = np.abs((h - c + 180) % 360 - 180)
    return 1 - _ramp(dh, half, half + soft)
def _luma(x): return np.einsum("hwc,c->hw", x, np.array([0.2126, 0.7152, 0.0722]))[..., None]
def _poly(pts, feather):
    """soft 0..1 region from fractional polygon points."""
    pm = Image.new("L", (a.size, a.size), 0)
    ImageDraw.Draw(pm).polygon([(x * a.size, y * a.size) for x, y in pts], fill=255)
    return nd.gaussian_filter(np.asarray(pm).astype(np.float64) / 255, feather * a.size)

# v8.1 white balance on the right ceiling patch (the left is clipped to R=G=1
# and useless as a reference). 75 % of full neutral: the room reads white,
# the skin keeps a little of the lamp.
ref = g[int(0.08 * a.size):int(0.16 * a.size), int(0.78 * a.size):int(0.95 * a.size)].reshape(-1, 3).mean(0)
wb = (ref.mean() / ref) ** 0.75                                       # gains in linear light
g = g * wb
g = _luma(g) + (g - _luma(g)) * 1.20                                  # v8: moderate global sat (was 1.35)

gam = np.clip(g, 0, 1) ** (1 / 2.2)
hue, sat, val = _hsv(gam)
fy = yy / a.size

# v8.2 the guitar body: a soft polygon around the wood (this frame is fixed, so
# geometry is fair), gated by hue/chroma so strings and the black fretboard
# stay put. In the neutral frame the wood reads pink-brown (hue ~340, sat .27);
# this is where its orange comes from now, instead of from the whole image.
wood = _poly([(0.25, 0.63), (0.30, 0.60), (0.39, 0.585), (0.50, 0.55), (0.58, 0.56), (0.64, 0.60), (0.68, 0.70),
              (0.72, 0.82), (0.70, 1.0), (0.06, 1.0), (0.06, 0.90), (0.12, 0.80), (0.22, 0.72)], 0.006)
wood *= _hue_win(hue, 350, 45, 15) * _ramp(sat, 0.10, 0.25) * _ramp(val, 0.20, 0.35)
wood = nd.gaussian_filter(wood, 0.003 * a.size)
mw = wood[..., None]
g = g * (1 + mw * np.array([0.15, 0.0, -0.50]))                       # pink-brown → orange
g = _luma(g) + (g - _luma(g)) * (1 + 0.35 * mw)

# v8.3 the shirt: red-magenta hue at high chroma (chest sat .55-.73; skin sits
# at hue 0-15 sat .25, the wood at sat .27, both drop out), below the face,
# never inside the wood polygon.
shirt = _hue_win(hue, 338, 24, 10) * _ramp(sat, 0.42, 0.56) * _ramp(val, 0.06, 0.16) * _ramp(fy, 0.30, 0.40)
shirt *= 1 - _poly([(0.25, 0.63), (0.30, 0.60), (0.39, 0.585), (0.50, 0.55), (0.58, 0.56), (0.64, 0.60), (0.68, 0.70),
                    (0.72, 0.82), (0.70, 1.0), (0.06, 1.0), (0.06, 0.90), (0.12, 0.80), (0.22, 0.72)], 0.004)
shirt = nd.gaussian_filter(shirt, 0.004 * a.size)
m = shirt[..., None]
g = _luma(g) + (g - _luma(g)) * (1 + 0.65 * m)                        # richer
g = g * (1 - m * np.array([0.0, 0.10, 0.32]))                         # magenta → red (blue down)
g = g * (1 - 0.14 * m)                                                # deeper

# v8.4 the purple LED string along the wall: violet, bright, on the wall band;
# saturated + lifted, with a soft violet glow bled around each point.
led = _hue_win(hue, 285, 35, 10) * _ramp(sat, 0.15, 0.28) * _ramp(val, 0.35, 0.55) * _ramp(fy, 0.22, 0.27) * (1 - _ramp(fy, 0.49, 0.54))
led = np.clip(nd.gaussian_filter(led, 0.0025 * a.size) * 1.6, 0, 1)   # each point becomes a bulb
ml = led[..., None]
g = _luma(g) + (g - _luma(g)) * (1 + 1.8 * ml)
g = g * (1 + 0.9 * ml)
violet = np.array([0.50, 0.10, 1.0]) * nd.gaussian_filter(led * val, 0.011 * a.size)[..., None] * 1.4
g = 1 - (1 - np.clip(g, 0, 1)) * (1 - np.clip(violet, 0, 1))          # glow, screened

# v8.5 skin: a whisper of warmth back into mid-chroma, mid-bright skin hues
# only (the white balance took the lamp off it), not the shirt, not the wood.
skin = _hue_win(hue, 12, 16, 8) * _ramp(sat, 0.14, 0.26) * _ramp(val, 0.40, 0.55) * (1 - shirt) * (1 - wood)
skin = nd.gaussian_filter(skin, 0.005 * a.size)
ms = skin[..., None]
g = g * (1 + ms * np.array([0.04, 0.0, -0.10]))
g = _luma(g) + (g - _luma(g)) * (1 + 0.10 * ms)
if a.debug:
    for nm, mk in [("shirt", shirt), ("led", led), ("wood", wood), ("skin", skin)]:
        Image.fromarray((np.clip(mk, 0, 1) * 255).astype(np.uint8)).resize((450, 450)).save(os.path.join(a.debug, f"mask-{nm}.png"))
    print("wb gains", wb.round(3), "ref", ref.round(3))
out = np.clip(g, 0, 1) ** (1 / 2.2)
out = 0.02 + 0.96 * out                                              # v7: blacks down
out = out + 0.16 * np.sin(np.pi * out) * (out - 0.5)                 # v7: a hard S
out = np.clip(out, 0, 1) ** 1.12                                      # v7: darker mids
# mild unsharp on the upscale, then grain
blur = nd.gaussian_filter(out, sigma=(1.6 * s / 5.5, 1.6 * s / 5.5, 0))
out = out + 0.75 * (out - blur)                                       # v7: crisp
rng = np.random.default_rng(8699)
grain = nd.gaussian_filter(rng.standard_normal(out.shape[:2]), 1.1) * 0.022
out = np.clip(out + grain[..., None], 0, 1)
# vignette
yy, xx = np.mgrid[0:a.size, 0:a.size] / a.size - 0.5
out *= (1 - 0.22 * np.clip((xx ** 2 + yy ** 2) * 2.2, 0, 1) ** 1.5)[..., None]

img = Image.fromarray((np.clip(out, 0, 1) * 255 + 0.5).astype(np.uint8))
# ── v7: a jpeg decode glitch — a few bands re-decoded at quality 14 with the
# 8×8 block rows slid sideways and the chroma torn off by a block or two
import io
buf = io.BytesIO(); img.save(buf, format="JPEG", quality=14, subsampling=2); buf.seek(0)
low = np.asarray(Image.open(buf).convert("RGB")).copy()
arr = np.asarray(img).copy()
grng = np.random.default_rng(8699)
for y0, h, shift, cshift in [(int(a.size * f), int(a.size * hh), sh, cs) for f, hh, sh, cs in
                             [(0.06, 0.018, 24, 8), (0.47, 0.010, -16, 16), (0.585, 0.026, 40, -8), (0.73, 0.008, -8, 24), (0.86, 0.014, 56, 0)]]:
    y0 -= y0 % 8; h = max(8, h - h % 8)
    band = np.roll(low[y0:y0 + h], shift, axis=1)
    band[..., 0] = np.roll(band[..., 0], cshift, axis=1)                 # red torn sideways
    arr[y0:y0 + h] = band
img = Image.fromarray(arr)
img.save(STEM + ".jpg", quality=92, subsampling=0)

# ── titled variant ──────────────────────────────────────────────────────
t = img.copy().convert("RGBA")
layer = Image.new("RGBA", t.size, (0, 0, 0, 0))
d = ImageDraw.Draw(layer)
font = ImageFont.truetype("/System/Library/Fonts/NewYorkItalic.ttf", int(a.size * 0.045))
text = "sailor song"
pad = int(a.size * 0.05)
bb = d.textbbox((0, 0), text, font=font)
pos = (pad, a.size - pad - (bb[3] - bb[1]) - bb[1])
shadow = Image.new("RGBA", t.size, (0, 0, 0, 0))
ImageDraw.Draw(shadow).text(pos, text, font=font, fill=(40, 20, 10, 150))
shadow = shadow.filter(ImageFilter.GaussianBlur(a.size * 0.004))
d.text(pos, text, font=font, fill=(255, 246, 236, 235))
t = Image.alpha_composite(Image.alpha_composite(t, shadow), layer).convert("RGB")
t.save(STEM + "-title.jpg", quality=92, subsampling=0)
print("✓", STEM + ".jpg")
