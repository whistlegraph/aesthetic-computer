#!/usr/bin/env python3
"""detect-face.py — find the dominant face bbox in an image and write
   a sidecar JSON. Used by cover-video.mjs to ken-burns into jeffrey's
   face for a more dynamic illustration animation.

Usage:
  recap/.venv/bin/python3 pop/dance/bin/detect-face.py <image.png>

Writes <image>.face.json next to the input image, with shape:
  {"x": int, "y": int, "w": int, "h": int, "imgW": int, "imgH": int}
or {"detected": false, "imgW": int, "imgH": int} when no face is found.
"""

from __future__ import annotations
import json
import sys
from pathlib import Path

import cv2

if len(sys.argv) < 2:
    print("usage: detect-face.py <image.png>", file=sys.stderr)
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
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
gray = cv2.equalizeHist(gray)

cascade_path = Path(cv2.data.haarcascades) / "haarcascade_frontalface_default.xml"
cascade = cv2.CascadeClassifier(str(cascade_path))
if cascade.empty():
    print(f"failed to load haar cascade: {cascade_path}", file=sys.stderr)
    sys.exit(2)

faces = cascade.detectMultiScale(
    gray,
    scaleFactor=1.10,
    minNeighbors=5,
    minSize=(int(0.08 * w), int(0.08 * h)),
)

out_path = img_path.with_suffix(img_path.suffix + ".face.json")

if len(faces) == 0:
    out_path.write_text(json.dumps({"detected": False, "imgW": w, "imgH": h}, indent=2))
    print(f"no face detected · wrote {out_path}", file=sys.stderr)
    sys.exit(0)

# Pick the largest face (likely the subject in a portrait).
faces_sorted = sorted(faces, key=lambda f: int(f[2]) * int(f[3]), reverse=True)
fx, fy, fw, fh = [int(v) for v in faces_sorted[0]]
out_path.write_text(
    json.dumps({"x": fx, "y": fy, "w": fw, "h": fh, "imgW": w, "imgH": h}, indent=2)
)
print(f"face at ({fx},{fy}) size ({fw}x{fh}) · wrote {out_path}", file=sys.stderr)
