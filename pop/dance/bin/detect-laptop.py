#!/usr/bin/env python3
"""detect-laptop.py — find jeffrey's chartreuse-green macbook neo bbox
   in an illustration and write a sidecar JSON. Used by cover-video.mjs
   for the lane-driven backlight effect (hats power the laptop region).

Strategy:
  1. Convert to HSV.
  2. Mask the chartreuse-green range (roughly H 50-85, S 100-255,
     V 100-255 — narrow band around the AC laptop color #aef240).
  3. Morphology (close → open) to consolidate the laptop body and
     drop small specks (pixie laptop doodles, leaves, fabric noise).
  4. Find the largest connected component → its bounding box is the
     laptop. Filter: area >= 1% of frame, aspect roughly horizontal
     or square (lid open vs closed).
  5. Sidecar JSON shape:
     {"x": int, "y": int, "w": int, "h": int, "imgW": int, "imgH": int}
     or {"detected": false, "imgW": int, "imgH": int} on miss.

Usage:
  recap/.venv/bin/python3 pop/dance/bin/detect-laptop.py <image.png>
"""

from __future__ import annotations
import json
import sys
from pathlib import Path

import cv2
import numpy as np

if len(sys.argv) < 2:
    print("usage: detect-laptop.py <image.png>", file=sys.stderr)
    sys.exit(1)

img_path = Path(sys.argv[1]).expanduser().resolve()
if not img_path.exists():
    print(f"image not found: {img_path}", file=sys.stderr)
    sys.exit(1)

img = cv2.imread(str(img_path))
if img is None:
    print(f"failed to read: {img_path}", file=sys.stderr)
    sys.exit(1)

h, w = img.shape[:2]
hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)

# Chartreuse-green band — H tuned to #aef240's hue (~75 in OpenCV's
# 0-180 range). Two-pass tighter band: the laptop body is a HIGH-
# saturation chartreuse-yellow-green; foliage tends to be lower
# saturation OR more pure green. We bias toward the yellow-leaning
# chartreuse range and require high S+V to skip both shadowed leaves
# and pale highlights.
H_LOW, H_HIGH = 35, 75
S_LOW, V_LOW = 140, 150
lower = np.array([H_LOW, S_LOW, V_LOW], dtype=np.uint8)
upper = np.array([H_HIGH, 255, 255], dtype=np.uint8)
mask = cv2.inRange(hsv, lower, upper)

# Morphology: close gaps inside the laptop body, drop tiny specks.
kClose = cv2.getStructuringElement(cv2.MORPH_RECT, (9, 9))
kOpen  = cv2.getStructuringElement(cv2.MORPH_RECT, (3, 3))
mask = cv2.morphologyEx(mask, cv2.MORPH_CLOSE, kClose, iterations=2)
mask = cv2.morphologyEx(mask, cv2.MORPH_OPEN,  kOpen,  iterations=1)

# Connected components → keep big-enough green blobs.
n, labels, stats, _ = cv2.connectedComponentsWithStats(mask, 8, cv2.CV_32S)

min_area = int(0.005 * w * h)
max_area = int(0.30 * w * h)  # bigger than this = sky / foliage, not a laptop
candidates = []
for i in range(1, n):
    x, y, ww, hh, area = stats[i]
    if area < min_area or area > max_area:
        continue
    if ww < 40 or hh < 30:
        continue
    aspect = ww / max(1, hh)
    if aspect < 0.55 or aspect > 5.5:
        continue
    # Bias toward laptop-positioned blobs: jeffrey holds the laptop in
    # the lower 2/3 of the frame, mid-horizontally. A blob whose top
    # edge is in the upper 15 % is almost certainly background/sky.
    if y < int(0.15 * h):
        continue
    candidates.append((area, x, y, ww, hh))

out_path = img_path.with_suffix(img_path.suffix + ".laptop.json")

if not candidates:
    out_path.write_text(json.dumps({"detected": False, "imgW": w, "imgH": h}, indent=2))
    print(f"no laptop detected · wrote {out_path}", file=sys.stderr)
    sys.exit(0)

# Largest qualifying green blob = jeffrey's laptop.
candidates.sort(key=lambda c: c[0], reverse=True)
_, x, y, ww, hh = candidates[0]
out_path.write_text(
    json.dumps({"x": int(x), "y": int(y), "w": int(ww), "h": int(hh),
                "imgW": w, "imgH": h}, indent=2)
)
print(f"laptop at ({x},{y}) size ({ww}x{hh}) · wrote {out_path}", file=sys.stderr)
