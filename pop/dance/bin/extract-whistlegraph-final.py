#!/usr/bin/env python3
"""extract-whistlegraph-final.py — find the LAST drawing in a
   whistlegraph score and crop tightly to its bounding box.

Steps per score image:
  1. grayscale + threshold (anything below ~190 = "ink")
  2. dilate vertically to fuse drawing strokes that are close
  3. connected components → bounding boxes
  4. filter out tiny boxes (lyric / handwriting text fragments) by
     keeping only boxes whose height >= 220 px (drawings are tall)
     and area >= 30k px (drawings are big)
  5. drop the very-top row (title + composer-name) and very-bottom
     row (composer credit) by ignoring boxes near the page edges
  6. sort surviving boxes by (y, x) — top-to-bottom, left-to-right
  7. take the LAST one — the canonical final drawing
  8. crop with a small white-border margin and write to <out>

Usage:
  python3 extract-whistlegraph-final.py <score.png> <out.png>
"""
from __future__ import annotations
import sys
from pathlib import Path
import cv2
import numpy as np

if len(sys.argv) < 3:
    print("usage: extract-whistlegraph-final.py <score.png> <out.png>", file=sys.stderr)
    sys.exit(1)

src = Path(sys.argv[1]).expanduser().resolve()
dst = Path(sys.argv[2]).expanduser().resolve()
img = cv2.imread(str(src))
if img is None:
    print(f"failed to read {src}", file=sys.stderr)
    sys.exit(1)
H, W = img.shape[:2]

gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
# Ink mask — pixels darker than ~190 (most drawings are pencil/red lines)
_, ink = cv2.threshold(gray, 190, 255, cv2.THRESH_BINARY_INV)
# Dilate to fuse drawing strokes that are close to each other, but
# keep the kernel skinny vertically so we don't accidentally merge
# a drawing with its lyric label below it (loner / etc.). We keep
# the un-dilated `ink` around for the post-crop row-gap scan since
# dilation closes the small gap between figure and label.
kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (12, 4))
mask = cv2.dilate(ink, kernel, iterations=1)

# Connected components.
n, labels, stats, _ = cv2.connectedComponentsWithStats(mask, 8, cv2.CV_32S)

candidates = []
for i in range(1, n):
    x, y, w, h, area = stats[i]
    # Drop title row (very top) and composer credit (very bottom).
    if y < int(H * 0.05):
        continue
    if y + h > int(H * 0.95):
        continue
    # Drawings are tall + biggish.
    if h < 200:
        continue
    if area < 25000:
        continue
    # Skip absurdly thin components (likely a single horizontal line of text).
    if w / h < 0.30:
        continue
    candidates.append((x, y, w, h, area))

if not candidates:
    print("no candidate drawings found", file=sys.stderr)
    sys.exit(2)

# Sort top-to-bottom, then left-to-right (with some y-tolerance for row
# grouping — components within 100 px vertically are "same row").
candidates.sort(key=lambda b: (b[1] // 200, b[0]))
# The LAST one is the canonical final drawing.
fx, fy, fw, fh, _ = candidates[-1]

# Add a small margin (5% of the drawing's max dim).
margin = int(max(fw, fh) * 0.08)
x0 = max(0, fx - margin)
y0 = max(0, fy - margin)
x1 = min(W, fx + fw + margin)
y1 = min(H, fy + fh + margin)

# Keep the entire bbox from the connected-component pass — labels
# that fall inside the bbox are tolerated. Earlier trim attempts
# were also lopping off legs/feet of figures (butterfly, mommy-wow
# WOW lettering, people-pleaser bottom). The OpenAI prompt instructs
# the model to read the glyph and ignore any visible label text.
crop = img[y0:y1, x0:x1]

# White-border padding for cleaner ref-image presentation.
pad = 60
crop = cv2.copyMakeBorder(crop, pad, pad, pad, pad, cv2.BORDER_CONSTANT, value=(255, 255, 255))
cv2.imwrite(str(dst), crop)
print(f"  cropped to {x1-x0}x{y1-y0} → {dst}", file=sys.stderr)
