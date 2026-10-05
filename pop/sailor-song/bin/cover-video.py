#!/usr/bin/env python3
"""cover-video.py — a cover from the RELIT performance video (v120: "a fresh cover from the current video vibe").
  pop/.venv/bin/python pop/sailor-song/bin/cover-video.py --t 139.0 [--src out/sailor-song-v119-relight.mp4] [--x 420] [--out cover/stem]
The video is already graded (room light, string lights, the finale wash), so this only crops square, upsamples in
linear light, adds a breath of bloom and a vignette, then the jpeg-decode glitch bands (cover.py's recipe) kept OFF her
face, and writes <stem>.jpg + <stem>-title.jpg ("Sage's Sailor Song").
"""
import argparse, io, os, subprocess
import numpy as np
from PIL import Image, ImageDraw, ImageFont, ImageFilter
from scipy import ndimage as nd
ap = argparse.ArgumentParser()
ap.add_argument("--t", type=float, default=139.0)
ap.add_argument("--src", default="")
ap.add_argument("--x", type=int, default=420)        # square crop left edge (of the video's width)
ap.add_argument("--size", type=int, default=3000)
ap.add_argument("--face", default="0.08,0.48")       # fraction of the height kept free of glitch bands
ap.add_argument("--out", default="")
a = ap.parse_args()
LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = a.src or os.path.join(LANE, "out", "sailor-song-v119-relight.mp4")
STEM = a.out or os.path.join(LANE, "cover", "sailor-song-cover-video")
probe = subprocess.run(["ffprobe", "-v", "error", "-select_streams", "v:0", "-show_entries", "stream=width,height", "-of", "csv=p=0", SRC], capture_output=True, text=True, check=True).stdout.strip().split(",")
W, H = int(probe[0]), int(probe[1])
raw = subprocess.run(["ffmpeg", "-v", "error", "-ss", str(a.t), "-i", SRC, "-vf", "format=rgb24", "-frames:v", "1", "-f", "rawvideo", "-"], capture_output=True, check=True).stdout
im = np.frombuffer(raw, dtype=np.uint8).reshape(H, W, 3).astype(np.float64) / 255.0
L = im[:, a.x:a.x + H] ** 2.2                                           # square, linear light
chans = [np.asarray(Image.fromarray((L[..., c] ** (1 / 2.2) * 255).astype(np.float32)).resize((a.size, a.size), Image.LANCZOS)) for c in range(3)]
g = np.clip(np.stack(chans, -1) / 255.0, 0, 1) ** 2.2
s = a.size / H
glow = nd.gaussian_filter(np.clip(g - 0.6, 0, None), sigma=(16 * s, 16 * s, 0))
g = 1 - (1 - g) * (1 - 0.45 * glow)                                     # a breath of bloom
out = g ** (1 / 2.2)
yy, xx = np.mgrid[0:a.size, 0:a.size] / a.size - 0.5
out *= (1 - 0.2 * np.clip((xx ** 2 + yy ** 2) * 2.2, 0, 1) ** 1.5)[..., None]
img = Image.fromarray((np.clip(out, 0, 1) * 255 + 0.5).astype(np.uint8))
# the glitch — a few bands re-decoded at quality 14, block rows slid sideways, red torn off (cover.py v7), never across her face
buf = io.BytesIO(); img.save(buf, format="JPEG", quality=14, subsampling=2); buf.seek(0)
low = np.asarray(Image.open(buf).convert("RGB")).copy(); arr = np.asarray(img).copy()
f0, f1 = [float(v) for v in a.face.split(",")]
for f, hh, sh, cs in [(0.04, 0.016, 24, 8), (0.52, 0.010, -16, 16), (0.60, 0.026, 40, -8), (0.73, 0.008, -8, 24), (0.86, 0.014, 56, 0)]:
    y0, h = int(a.size * f), int(a.size * hh); y0 -= y0 % 8; h = max(8, h - h % 8)
    if y0 + h > a.size * f0 and y0 < a.size * f1: continue
    band = np.roll(low[y0:y0 + h], sh, axis=1); band[..., 0] = np.roll(band[..., 0], cs, axis=1); arr[y0:y0 + h] = band
img = Image.fromarray(arr); img.save(STEM + ".jpg", quality=92, subsampling=0)
t = img.copy().convert("RGBA"); layer = Image.new("RGBA", t.size, (0, 0, 0, 0)); d = ImageDraw.Draw(layer)
font = ImageFont.truetype("/System/Library/Fonts/NewYorkItalic.ttf", int(a.size * 0.045)); text = "Sage's Sailor Song"
pad = int(a.size * 0.05); bb = d.textbbox((0, 0), text, font=font); pos = (pad, a.size - pad - (bb[3] - bb[1]) - bb[1])
shadow = Image.new("RGBA", t.size, (0, 0, 0, 0)); ImageDraw.Draw(shadow).text(pos, text, font=font, fill=(40, 20, 10, 150))
shadow = shadow.filter(ImageFilter.GaussianBlur(a.size * 0.004)); d.text(pos, text, font=font, fill=(255, 246, 236, 235))
Image.alpha_composite(Image.alpha_composite(t, shadow), layer).convert("RGB").save(STEM + "-title.jpg", quality=92, subsampling=0)
print("✓", STEM + ".jpg")
