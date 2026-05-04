#!/usr/bin/env python3
"""
repair_letter.py — patch a single bad letter into an existing word image.

Given: the original word image, the bad-letter index, and a freshly-
generated single-letter FLUX image (same color scheme as the word),
extract the new letter, scale it to the size of the bad letter's
bounding box, and composite it back into the word image at that spot.

The replacement keeps the FLUX aesthetic (no font fallback) — we just
swap pixel patches.

Usage:
  repair_letter.py \\
      --word-img big-pictures/out/.../word-001.jpg \\
      --letter-img /tmp/replacement-a.jpg \\
      --slot 2 \\
      --expected-word grace \\
      --bg-color peachpuff \\
      --letters-color saddlebrown
"""
import argparse
import json
import sys
from pathlib import Path

import numpy as np
from PIL import Image

sys.path.insert(0, str(Path(__file__).parent))
from render_frames import extract_glyphs, color_rgb


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--word-img", required=True)
    ap.add_argument("--letter-img", required=True)
    ap.add_argument("--slot", type=int, required=True,
                    help="Index (0-based) of the bad letter in the word")
    ap.add_argument("--expected-word", required=True)
    ap.add_argument("--bg-color", required=True,
                    help="CSS named color for the slide's background")
    ap.add_argument("--letters-color", required=True,
                    help="CSS named color for the slide's letters")
    ap.add_argument("--out", help="Output path (default: overwrites --word-img)")
    args = ap.parse_args()

    out_path = args.out or args.word_img

    # 1. Extract glyphs from the original word image to find the bad
    #    letter's source bounding box.
    word_glyphs = extract_glyphs(args.word_img)
    if args.slot >= len(word_glyphs):
        print(json.dumps({"ok": False, "reason": f"slot {args.slot} out of range"}))
        return
    target = word_glyphs[args.slot]
    target_box = (target["x0"], target["y0"],
                  target["x0"] + target["w"], target["y1"])
    target_w = target["w"]
    target_h = target["h"]

    # 2. Extract the letter from the replacement single-letter image.
    repl_glyphs = extract_glyphs(args.letter_img)
    if not repl_glyphs:
        print(json.dumps({"ok": False, "reason": "no glyph extracted from replacement"}))
        return
    # If multiple components were detected, pick the largest by area —
    # FLUX may render speech bubbles or stray dots around a single letter.
    repl_glyphs.sort(key=lambda g: g["w"] * g["h"], reverse=True)
    repl = repl_glyphs[0]

    # 3. Scale the replacement glyph to match the target bounding box's
    #    HEIGHT (preserve aspect ratio so the letter doesn't stretch).
    #    NEAREST instead of LANCZOS — the source is pixel art (no
    #    smoothing wanted) AND lanczos turns binary alpha into
    #    fractional values, which PIL's paste then mixes the FLUX bg
    #    color into the slide bg, creating a visible rectangular halo.
    scale = target_h / max(1, repl["h"])
    new_w = max(1, int(repl["w"] * scale))
    new_h = max(1, int(repl["h"] * scale))
    repl_img = repl["img"].resize((new_w, new_h), Image.NEAREST)

    # 4. Recolor + hard-binarize alpha. Belt-and-suspenders against
    #    halo bleed: where the alpha mask says "not letter", explicitly
    #    overwrite RGB to the slide bg color so even partial-opacity
    #    pixels render as bg, not as FLUX's original bg.
    target_rgb = color_rgb(args.letters_color)
    bg_rgb = color_rgb(args.bg_color)
    arr = np.array(repl_img)
    raw_alpha = arr[:, :, 3]
    binary_mask = np.where(raw_alpha > 128, 255, 0).astype(np.uint8)
    arr[:, :, 0] = np.where(binary_mask > 0, target_rgb[0], bg_rgb[0])
    arr[:, :, 1] = np.where(binary_mask > 0, target_rgb[1], bg_rgb[1])
    arr[:, :, 2] = np.where(binary_mask > 0, target_rgb[2], bg_rgb[2])
    arr[:, :, 3] = binary_mask
    repl_img = Image.fromarray(arr, "RGBA")

    # 5. Composite into the original word image. Sample the ACTUAL bg
    #    color from the word image's corners — FLUX often renders the
    #    spec'd bg as a different shade, so painting with the spec color
    #    creates a visible rectangular patch. Sampled bg = same color
    #    as the surrounding pixels = invisible patch.
    word_img = Image.open(args.word_img).convert("RGB")
    word_arr = np.array(word_img)
    s = 16
    corners = np.concatenate([
        word_arr[:s, :s, :].reshape(-1, 3),
        word_arr[:s, -s:, :].reshape(-1, 3),
        word_arr[-s:, :s, :].reshape(-1, 3),
        word_arr[-s:, -s:, :].reshape(-1, 3),
    ])
    actual_bg = tuple(int(c) for c in np.median(corners, axis=0))
    # Also rebuild the replacement's non-letter pixels with this sampled
    # bg (replacing the slide-spec bg we used earlier as defense).
    repl_arr = np.array(repl_img)
    repl_mask = repl_arr[:, :, 3]
    repl_arr[:, :, 0] = np.where(repl_mask > 0, target_rgb[0], actual_bg[0])
    repl_arr[:, :, 1] = np.where(repl_mask > 0, target_rgb[1], actual_bg[1])
    repl_arr[:, :, 2] = np.where(repl_mask > 0, target_rgb[2], actual_bg[2])
    repl_img = Image.fromarray(repl_arr, "RGBA")

    # Expand the patch rectangle slightly so we cover the dilation halo.
    pad = 4
    x0 = max(0, target_box[0] - pad)
    y0 = max(0, target_box[1] - pad)
    x1 = min(word_img.width, target_box[2] + pad)
    y1 = min(word_img.height, target_box[3] + pad)
    # Paint over the old letter with the sampled bg (invisible patch)
    overlay = Image.new("RGB", (x1 - x0, y1 - y0), actual_bg)
    word_img.paste(overlay, (x0, y0))
    # Paste the new letter centered horizontally inside the original
    # bbox; bottom-aligned to the original baseline (y1).
    new_letter_x = x0 + pad + (target_w - new_w) // 2
    new_letter_y = target_box[3] - new_h
    word_img.paste(repl_img, (new_letter_x, new_letter_y), repl_img)

    # Save as PNG (lossless) to preserve the small letter counter that
    # JPEG would smear into oblivion. PIL on read auto-detects format
    # from file content, not extension, so the .jpg-named path still
    # works downstream.
    word_img.save(out_path, format="PNG")
    print(json.dumps({
        "ok": True,
        "out": out_path,
        "target_box": [int(x) for x in target_box],
        "patch_box": [x0, y0, x1, y1],
        "new_letter_size": [new_w, new_h],
    }))


if __name__ == "__main__":
    main()
