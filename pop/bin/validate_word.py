#!/usr/bin/env python3
"""
validate_word.py — sanity-check + score a generated word image.

Re-uses the CC-based extract_glyphs() from render_frames.py. Reports:

  expected_n   — letter count from the expected word
  found_n      — number of glyph runs detected (post merge)
  height_cv    — coefficient of variation of glyph heights (0=uniform)
  baseline_cv  — coefficient of variation of glyph y1 (bottom)
  ok           — boolean: passes all hard checks
  score        — 0..5 quality score (higher = better extraction)
  diagnostic   — one-line reason if not ok

Score components:
  +2 if found_n == expected_n
  +1 if height_cv < 0.35  (uniform letter heights)
  +1 if baseline_cv < 0.05 (clean baseline)
  +1 if no slivers (every glyph w >= 6, h >= median*0.45)

Used by tiktok.mjs to (a) reject FLUX outputs that won't extract
cleanly and (b) pick the best of 5 attempts probabilistically.

Usage:
  validate_word.py <image_path> <expected_word>
"""
import json
import sys
from pathlib import Path

import numpy as np
from scipy.ndimage import binary_fill_holes

sys.path.insert(0, str(Path(__file__).parent))
from render_frames import extract_glyphs

# Letter-aware topology: lowercase letters that have an enclosed counter.
# Threshold = minimum hole-area ratio (hole_pixels / bbox_area) the letter
# must show. Tuned from the FLUX corpus: real 'a' counters clock in
# around 5–15% of bbox; a fake "block-shape a" comes in under 2%.
LETTER_HOLE_MIN = {
    'a': 0.018, 'b': 0.040, 'd': 0.040, 'e': 0.015,
    'g': 0.035, 'o': 0.050, 'p': 0.035, 'q': 0.035,
    'A': 0.025, 'B': 0.025, 'D': 0.040, 'O': 0.050,
    'P': 0.025, 'Q': 0.035, 'R': 0.018,
}
# Per-letter shape constraints — catches malformed letters that pass
# topology (because they're not supposed to have a counter) but are
# rendered as solid blocks or wrong proportions.
#
# Aspect = w / h. Density = foreground_pixels / bbox_area.
# Tuned permissively so chunky pixel fonts pass; only egregious
# block-shapes get rejected. Pixel art legitimately ranges high in
# density for some letters (e, o), so we set per-letter ceilings.
# Conservative — only catch egregious shape cases. Pixel-art chunky
# fonts produce wider variation than non-pixel typography, so most
# letter ranges have to be very permissive or we get false positives.
# 'w' is the most distinctively-shaped letter (4 strokes with V-gaps)
# so it gets the cleanest density+aspect signals.
LETTER_ASPECT = {
    'i': (0.08, 0.55),
    'j': (0.15, 0.65),
    'l': (0.08, 0.60),
    'm': (0.80, 1.80),
    'w': (0.85, 1.80),
}
LETTER_DENSITY_MAX = {
    'w': 0.62,
}


def hole_ratio(glyph_img):
    """Hole pixels / bbox area — measures how much of the bounding box is
    empty space enclosed inside the glyph (the counter)."""
    arr = np.array(glyph_img)
    mask = (arr[:, :, 3] > 128).astype(np.uint8)
    if mask.sum() == 0:
        return 0.0
    filled = binary_fill_holes(mask).astype(np.uint8)
    hole_px = int(filled.sum() - mask.sum())
    bbox_area = int(mask.shape[0] * mask.shape[1])
    return hole_px / max(1, bbox_area)


def density(glyph_img):
    """Foreground pixels / bbox area."""
    arr = np.array(glyph_img)
    mask = (arr[:, :, 3] > 128).astype(np.uint8)
    return float(mask.sum()) / max(1, mask.shape[0] * mask.shape[1])


def disconnected_pieces(glyph_img):
    """Count un-dilated connected components in the glyph's alpha
    mask. The dilation step in extract_glyphs merges fragments into
    one bounding box, but the underlying mask retains the original
    structure. A clean letter is exactly 1 piece (for 'i'/'j' it's
    2: dot + body). >2 means a broken glyph — e.g. a 't' with the
    crossbar floating detached from the stem and a chunk missing
    from the stem (seen on amazing slide 18 'but'). Small spurious
    pieces (<8% of the glyph's total mask area) don't count — they
    might be anti-aliasing residue."""
    from scipy.ndimage import label
    arr = np.array(glyph_img)
    mask = (arr[:, :, 3] > 128).astype(np.uint8)
    if mask.sum() == 0:
        return 0
    labeled, n = label(mask)
    if n <= 1:
        return n
    # Drop tiny components (anti-aliasing or stray pixels)
    sizes = [(i, int((labeled == i).sum())) for i in range(1, n + 1)]
    total = sum(s for _, s in sizes)
    big = [i for i, sz in sizes if sz >= max(8, total * 0.06)]
    return len(big)


def stroke_balance(glyph_img):
    """For letters with an enclosed counter (a/b/d/e/g/o/p/q): the
    left and right vertical strokes flanking the counter should be
    roughly equal thickness. Broken counter shapes (e.g. an 'o' that's
    actually a 'C' with the inner area filled solid) produce 3-4×
    stroke imbalance — topology check passes but the letter is wrong.
    Returns max/min stroke width ratio across middle rows of the
    glyph; ~1.0 = balanced, ≥2.0 = broken."""
    arr = np.array(glyph_img)
    mask = (arr[:, :, 3] > 128).astype(int)
    h, w = mask.shape
    if h < 12 or w < 6:
        return 1.0
    mid = mask[h // 4: 3 * h // 4]
    left_widths = []
    right_widths = []
    for row in mid:
        s = int(row.sum())
        # Skip rows that are top/bottom bars (mostly fg)
        if s > w * 0.85 or s == 0:
            continue
        # Walk in from left
        lw = 0; in_run = False
        for x in range(w):
            if row[x]:
                in_run = True; lw += 1
            elif in_run:
                break
        # Walk in from right
        rw = 0; in_run = False
        for x in range(w - 1, -1, -1):
            if row[x]:
                in_run = True; rw += 1
            elif in_run:
                break
        # Only count rows with TWO separate strokes
        if lw > 0 and rw > 0 and lw + rw < s + 2:
            left_widths.append(lw)
            right_widths.append(rw)
    if len(left_widths) < 3:
        return 1.0  # not enough two-stroke rows; can't judge
    import statistics as st
    ml = st.median(left_widths)
    mr = st.median(right_widths)
    return max(ml, mr) / max(1, min(ml, mr))


def bg_uniformity(img_path):
    """Sample 8 16x16 patches around the perimeter and return the
    average per-channel std of the combined samples. Solid backgrounds
    score ~1; textured ones (wood grain, gradients) score 5-15+."""
    from PIL import Image
    arr = np.array(Image.open(img_path).convert("RGB"))
    h, w, _ = arr.shape
    s = 16
    positions = [
        (0, 0), (0, w // 2 - s // 2), (0, w - s),
        (h // 2 - s // 2, 0), (h // 2 - s // 2, w - s),
        (h - s, 0), (h - s, w // 2 - s // 2), (h - s, w - s),
    ]
    patches = [arr[y0:y0 + s, x0:x0 + s].reshape(-1, 3) for y0, x0 in positions]
    samples = np.concatenate(patches)
    return float(np.std(samples, axis=0).mean())


def coef_of_var(values):
    if not values:
        return 0.0
    mean = sum(values) / len(values)
    if mean == 0:
        return 0.0
    var = sum((v - mean) ** 2 for v in values) / len(values)
    return (var ** 0.5) / abs(mean)


def main():
    if len(sys.argv) < 3:
        print(json.dumps({"ok": False, "diagnostic": "usage error", "score": 0}))
        return
    img_path = sys.argv[1]
    expected_word = sys.argv[2].lower()
    expected_n = sum(1 for c in expected_word if c.isalpha())
    try:
        glyphs = extract_glyphs(img_path)
    except Exception as e:
        print(json.dumps({
            "ok": False, "score": 0,
            "diagnostic": f"extract failed: {e}",
            "expected_n": expected_n, "found_n": 0,
        }))
        return

    found_n = len(glyphs)
    widths = [int(g["w"]) for g in glyphs]
    heights = [int(g["h"]) for g in glyphs]
    bottoms = [int(g["y1"]) for g in glyphs]

    height_cv = coef_of_var(heights)
    # Baseline CV: variation in bottom-y across glyphs, normalized by
    # median height (so it's scale-invariant).
    if heights:
        median_h = sorted(heights)[len(heights) // 2]
        if bottoms and median_h > 0:
            mean_b = sum(bottoms) / len(bottoms)
            var_b = sum((b - mean_b) ** 2 for b in bottoms) / len(bottoms)
            baseline_cv = (var_b ** 0.5) / median_h
        else:
            baseline_cv = 0.0
    else:
        median_h = 1
        baseline_cv = 1.0

    # Sliver check (very thin glyphs are usually FLUX artifacts)
    slivers = []
    for i, (w, h) in enumerate(zip(widths, heights)):
        if w < 6 or h < median_h * 0.45:
            slivers.append(i)

    # Topology check: per-glyph hole ratio vs the letter at the
    # corresponding position in the expected word. This catches
    # block-shape glyphs that FLUX produced when the letterform was
    # supposed to have an enclosed counter.
    expected_letters = [c for c in expected_word if c.isalpha()]
    topology_failures = []  # (idx, letter, hole_ratio, expected_min)
    shape_failures = []
    # 'i' and 'j' legitimately have 2 disconnected pieces (dot + body);
    # everything else should be exactly 1.
    EXPECTED_PIECES = {"i": 2, "j": 2, ":": 2, ";": 2}
    if found_n == expected_n:
        for i, (g, letter) in enumerate(zip(glyphs, expected_letters)):
            ll = letter.lower()
            # Disconnection check: catches glyphs where dilation merged
            # broken fragments into one bbox but the un-dilated mask is
            # actually multiple pieces (e.g. 't' with detached crossbar).
            pieces = disconnected_pieces(g["img"])
            expected_pieces = EXPECTED_PIECES.get(ll, 1)
            if pieces > expected_pieces:
                topology_failures.append(
                    (i, letter, round(pieces, 1), expected_pieces))
            if letter in LETTER_HOLE_MIN:
                r = hole_ratio(g["img"])
                lo = LETTER_HOLE_MIN[letter]
                if r < lo:
                    topology_failures.append((i, letter, round(r, 4), lo))
                # Stroke-balance check for counter letters: catches the
                # 'C-shape with filled left half' case where topology
                # alone says "has hole" but the letter is broken.
                bal = stroke_balance(g["img"])
                if bal >= 2.2:
                    topology_failures.append((i, letter, round(-bal, 4), -2.2))
            # Aspect ratio: catches "letter rendered too square" (block w)
            if ll in LETTER_ASPECT:
                aspect = g["w"] / max(1, g["h"])
                lo, hi = LETTER_ASPECT[ll]
                if aspect < lo or aspect > hi:
                    shape_failures.append((i, letter, "aspect", round(aspect, 3), lo, hi))
            # Density ceiling: catches "letter rendered as solid block"
            if ll in LETTER_DENSITY_MAX:
                d = density(g["img"])
                dmax = LETTER_DENSITY_MAX[ll]
                if d > dmax:
                    shape_failures.append((i, letter, "density", round(d, 3), 0.0, dmax))

    # Bg uniformity: reject FLUX outputs with textured/non-solid bg
    # (the wood-grain "saved" case where the alpha mask wraps the
    # texture instead of just the letters).
    bg_std = bg_uniformity(img_path)

    # Hard checks
    ok = True
    diag = "ok"
    if bg_std > 5.0:
        ok = False
        diag = f"non-uniform bg (std={bg_std:.1f}, expected <5) — bg is textured"
    elif found_n != expected_n:
        ok = False
        diag = f"glyph count mismatch (found {found_n}, expected {expected_n})"
    elif slivers:
        ok = False
        diag = f"{len(slivers)} sliver glyph(s) — likely artifacts"
    elif baseline_cv > 0.18:
        ok = False
        diag = f"baseline misaligned (cv={baseline_cv:.2f})"
    elif height_cv > 0.55:
        ok = False
        diag = f"letter heights wildly inconsistent (cv={height_cv:.2f})"
    elif topology_failures:
        f = topology_failures[0]
        ok = False
        diag = (f"topology fail: pos {f[0]} '{f[1]}' hole_ratio={f[2]:.3f} "
                f"(expected {'<=' if f[3] < 0 else '>='} {abs(f[3]):.3f})")
    elif shape_failures:
        f = shape_failures[0]
        ok = False
        diag = (f"shape fail: pos {f[0]} '{f[1]}' {f[2]}={f[3]} "
                f"(expected {f[4]}..{f[5]})")

    # Score (0..8) — used for probabilistic best-pick
    score = 0
    if found_n == expected_n:
        score += 2
    if height_cv < 0.35:
        score += 1
    if baseline_cv < 0.05:
        score += 1
    if not slivers:
        score += 1
    if not topology_failures:
        score += 1
    if not shape_failures:
        score += 1
    if bg_std <= 5.0:
        score += 1

    print(json.dumps({
        "ok": ok,
        "diagnostic": diag,
        "expected_n": expected_n,
        "found_n": found_n,
        "height_cv": round(height_cv, 3),
        "baseline_cv": round(baseline_cv, 3),
        "slivers": len(slivers),
        "topology_failures": topology_failures,
        "shape_failures": shape_failures,
        "bg_std": round(bg_std, 2),
        "widths": widths,
        "heights": heights,
        "score": score,
    }))


if __name__ == "__main__":
    main()
