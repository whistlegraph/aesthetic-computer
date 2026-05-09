#!/usr/bin/env python3
"""debug-composition.py — detect face + shirt-logo bboxes on chapter
portraits and render debug stills so the slide layouter can avoid them.

For each PNG in recap/out/jeffrey-photos/:
  - detect face bbox via OpenCV Haar cascade (bundled, no external model)
  - detect text regions in the chest band (below face) via tesseract OCR
  - write recap/out/cv/<basename>.json with the bboxes
  - write recap/out/debug/<basename>.png with overlaid colored boxes:
      red    = face (avoid)
      yellow = shirt logo text (avoid)
      cyan   = recommended type-safe band (top + below shirt logos)

Usage:
  .venv/bin/python3 bin/debug-composition.py [photo_dir] [debug_dir]
  (defaults: out/jeffrey-photos out/debug)
"""

from __future__ import annotations

import json
import os
import sys
from pathlib import Path

import cv2
import numpy as np
import pytesseract
from PIL import Image, ImageDraw, ImageFont


ROOT = Path(__file__).resolve().parent.parent
PHOTO_DIR = Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT / "out" / "jeffrey-photos"
DEBUG_DIR = Path(sys.argv[2]) if len(sys.argv) > 2 else ROOT / "out" / "debug"
CV_DIR = ROOT / "out" / "cv"

DEBUG_DIR.mkdir(parents=True, exist_ok=True)
CV_DIR.mkdir(parents=True, exist_ok=True)

# OpenCV's bundled Haar cascade — good enough for centered portrait shots.
HAAR_PATH = Path(cv2.data.haarcascades) / "haarcascade_frontalface_default.xml"
face_cascade = cv2.CascadeClassifier(str(HAAR_PATH))
if face_cascade.empty():
    raise RuntimeError(f"failed to load Haar cascade at {HAAR_PATH}")


def detect_face(bgr: np.ndarray) -> tuple[int, int, int, int] | None:
    """Return the largest face bbox (x, y, w, h) or None."""
    gray = cv2.cvtColor(bgr, cv2.COLOR_BGR2GRAY)
    faces = face_cascade.detectMultiScale(gray, scaleFactor=1.1, minNeighbors=5, minSize=(120, 120))
    if len(faces) == 0:
        return None
    # Pick the largest by area (handles multi-jeffrey title slide too).
    return tuple(max(faces, key=lambda f: f[2] * f[3]).tolist())


def detect_shirt_logos(bgr: np.ndarray, face: tuple | None) -> list[tuple[int, int, int, int]]:
    """OCR the chest band (below the face, above mid-thigh) for printed
    text regions. Returns list of bboxes in original image coords."""
    h, w = bgr.shape[:2]
    if face is None:
        # No face = guess chest band as 35–80% of frame height
        y0, y1 = int(h * 0.35), int(h * 0.80)
    else:
        fx, fy, fw, fh = face
        y0 = fy + fh                     # just below the chin
        y1 = min(h, fy + fh + int(fh * 3.0))  # ~3 face-heights down
    if y1 <= y0:
        return []

    crop = bgr[y0:y1, :]
    rgb = cv2.cvtColor(crop, cv2.COLOR_BGR2RGB)
    pil = Image.fromarray(rgb)
    # Upscale 2x for better OCR on small shirt prints
    pil = pil.resize((pil.width * 2, pil.height * 2), Image.LANCZOS)
    data = pytesseract.image_to_data(pil, output_type=pytesseract.Output.DICT, config="--psm 11")

    boxes: list[tuple[int, int, int, int]] = []
    for i, txt in enumerate(data["text"]):
        if not txt or not txt.strip():
            continue
        try:
            conf = float(data["conf"][i])
        except (TypeError, ValueError):
            continue
        if conf < 50:
            continue
        # Need at least 2 alpha chars to count as a logo (skip noise glyphs)
        if sum(c.isalpha() for c in txt) < 2:
            continue
        x = data["left"][i] // 2
        y = data["top"][i] // 2 + y0
        bw = data["width"][i] // 2
        bh = data["height"][i] // 2
        boxes.append((int(x), int(y), int(bw), int(bh)))
    return merge_close_boxes(boxes)


def merge_close_boxes(boxes: list[tuple[int, int, int, int]], gap: int = 30):
    """Merge horizontally-adjacent OCR fragments into single logo bboxes."""
    if not boxes:
        return []
    boxes = sorted(boxes, key=lambda b: (b[1] // 50, b[0]))
    merged = [list(boxes[0])]
    for b in boxes[1:]:
        last = merged[-1]
        if abs(b[1] - last[1]) < 25 and b[0] - (last[0] + last[2]) < gap:
            x = min(last[0], b[0])
            y = min(last[1], b[1])
            x2 = max(last[0] + last[2], b[0] + b[2])
            y2 = max(last[1] + last[3], b[1] + b[3])
            last[:] = [x, y, x2 - x, y2 - y]
        else:
            merged.append(list(b))
    return [tuple(b) for b in merged]


def detect_laptop(bgr: np.ndarray, face: tuple | None) -> tuple[int, int, int, int] | None:
    """Find the largest chartreuse / dark-rect region in the lower 2/3 of the
    frame — that's almost always the laptop (chartreuse Neo or black ThinkPad).
    Returns (x, y, w, h) of the laptop bbox, or None if nothing convincing.

    The chartreuse pass dominates when present (Apple's bright yellow-green
    is unmistakable in HSV). For ThinkPad scenes (no chartreuse) we fall back
    to a dark-rect detector inside the lower-2/3 band: closed contour, large
    area, mostly horizontal aspect.
    """
    h_img, w_img = bgr.shape[:2]
    # Search band: from face_bottom (or 1/3) down to 95% of frame height.
    if face is not None:
        fx, fy, fw, fh = face
        y0 = max(int(h_img * 0.30), fy + int(fh * 0.6))
    else:
        y0 = int(h_img * 0.30)
    y1 = int(h_img * 0.95)
    if y1 <= y0 + 100:
        return None
    crop = bgr[y0:y1, :]

    hsv = cv2.cvtColor(crop, cv2.COLOR_BGR2HSV)
    # Chartreuse / Apple bright-yellow-green range. H is OpenCV 0–179.
    chartreuse_mask = cv2.inRange(hsv, (28, 100, 110), (60, 255, 255))
    chartreuse_area = int(chartreuse_mask.sum() // 255)
    crop_area = crop.shape[0] * crop.shape[1]

    if chartreuse_area > crop_area * 0.04:
        # Strong chartreuse signal — that's the Neo.
        contours, _ = cv2.findContours(chartreuse_mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        if contours:
            biggest = max(contours, key=cv2.contourArea)
            x, y, bw, bh = cv2.boundingRect(biggest)
            return (int(x), int(y + y0), int(bw), int(bh))

    # No chartreuse — try dark-rectangle (ThinkPad / closed lid).
    gray = cv2.cvtColor(crop, cv2.COLOR_BGR2GRAY)
    # Threshold on darkness — anything below 60 luma is "very dark."
    _, dark = cv2.threshold(gray, 60, 255, cv2.THRESH_BINARY_INV)
    # Clean up noise.
    kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (15, 15))
    dark = cv2.morphologyEx(dark, cv2.MORPH_OPEN, kernel)
    dark = cv2.morphologyEx(dark, cv2.MORPH_CLOSE, kernel)
    contours, _ = cv2.findContours(dark, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    candidates = []
    for c in contours:
        x, y, bw, bh = cv2.boundingRect(c)
        if bw < w_img * 0.25 or bh < 80:
            continue
        # Prefer wide-ish (laptop-shaped) rects.
        aspect = bw / max(1, bh)
        if aspect < 0.7 or aspect > 4.0:
            continue
        area = bw * bh
        candidates.append((area, x, y + y0, bw, bh))
    if candidates:
        _, x, y, bw, bh = max(candidates, key=lambda t: t[0])
        return (int(x), int(y), int(bw), int(bh))
    return None


def safe_bands(face: tuple | None, logos: list, w: int, h: int):
    """Return cyan bands that are clear of face + logos.
    Currently: top band above face, bottom band below logos (or mid-thigh)."""
    bands = []
    if face is None:
        bands.append((0, 0, w, int(h * 0.18)))
        bands.append((0, int(h * 0.85), w, int(h * 0.15)))
        return bands

    fx, fy, fw, fh = face
    # Top band above face top
    top_h = max(0, fy - 30)
    if top_h > 60:
        bands.append((0, 0, w, top_h))
    # Bottom band below the lowest logo (or below face+2*fh if no logos)
    if logos:
        lowest = max(b[1] + b[3] for b in logos)
    else:
        lowest = fy + fh + int(fh * 2.0)
    if h - lowest > 80:
        bands.append((0, lowest + 10, w, h - lowest - 10))
    return bands


def draw_debug(bgr: np.ndarray, face, logos, bands, label: str, dst: Path, laptop=None):
    pil = Image.fromarray(cv2.cvtColor(bgr, cv2.COLOR_BGR2RGB)).convert("RGB")
    draw = ImageDraw.Draw(pil)

    # Cyan safe bands first (semi-transparent fill via outline-only here)
    for x, y, bw, bh in bands:
        for off in range(3):
            draw.rectangle([x + off, y + off, x + bw - off, y + bh - off], outline=(0, 220, 220))

    # Yellow shirt logos
    for x, y, bw, bh in logos:
        draw.rectangle([x, y, x + bw, y + bh], outline=(255, 220, 0), width=4)

    # Red face
    if face is not None:
        x, y, bw, bh = face
        draw.rectangle([x, y, x + bw, y + bh], outline=(255, 50, 80), width=5)

    # Magenta laptop bbox (the chartreuse Neo or ThinkPad — chrome MUST stay
    # off this rectangle)
    if laptop is not None:
        x, y, bw, bh = laptop
        draw.rectangle([x, y, x + bw, y + bh], outline=(255, 0, 200), width=4)

    # Caption strip
    cap = f"{label}  face={'yes' if face else 'no'}  logos={len(logos)}  safe-bands={len(bands)}"
    try:
        font = ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", 24)
    except Exception:
        font = ImageFont.load_default()
    draw.rectangle([0, 0, pil.width, 36], fill=(20, 10, 20))
    draw.text((12, 4), cap, fill=(245, 247, 252), font=font)

    pil.save(dst)


def run():
    pngs = sorted(p for p in PHOTO_DIR.glob("*.png"))
    if not pngs:
        print(f"no PNGs in {PHOTO_DIR}", file=sys.stderr)
        return 1

    summary = []
    for png in pngs:
        bgr = cv2.imread(str(png))
        if bgr is None:
            print(f"  ✗ skip (unreadable): {png.name}")
            continue
        h, w = bgr.shape[:2]
        face = detect_face(bgr)
        laptop = detect_laptop(bgr, face)
        logos = detect_shirt_logos(bgr, face)
        bands = safe_bands(face, logos, w, h)

        cv_path = CV_DIR / f"{png.stem}.json"
        cv_path.write_text(json.dumps({
            "image": png.name,
            "size": [w, h],
            "face": list(face) if face else None,
            "laptop": list(laptop) if laptop else None,
            "shirtLogos": [list(b) for b in logos],
            "safeBands": [list(b) for b in bands],
        }, indent=2))

        dbg_path = DEBUG_DIR / f"{png.stem}.png"
        draw_debug(bgr, face, logos, bands, png.stem, dbg_path, laptop=laptop)

        summary.append((png.name, "face" if face else "no-face", "laptop" if laptop else "no-laptop", len(logos), len(bands)))
        print(f"  ✓ {png.name}: face={'yes' if face else 'no'}  laptop={'yes' if laptop else 'no'}  logos={len(logos)}  safe={len(bands)}")

    print()
    print(f"→ wrote {len(summary)} cv/*.json + debug/*.png")
    print(f"   {CV_DIR}")
    print(f"   {DEBUG_DIR}")
    return 0


if __name__ == "__main__":
    sys.exit(run())
