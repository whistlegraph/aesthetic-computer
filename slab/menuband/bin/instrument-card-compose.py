#!/usr/bin/env python3
"""instrument-card-compose.py — compose a 1080×1080 Menu Band instrument card.

The illustration is full-bleed; all chrome overlays it as translucent
gradients with luma-aware drop shadows.

    ┌─── family-color outer frame ───┐
    │ MENU BAND                      │  ← translucent header gradient
    │                                │     (family color, fades down)
    │            2 4                 │  ← MASSIVE program number
    │            ──                  │
    │      ACOUSTIC GUITAR (NYLON)   │
    │                                │
    │   full-bleed illustration       │
    │                                │
    │                                │
    │  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░  │  ← translucent footer gradient
    │  enter menuband and type 24    │     (dark, fades up)
    │  to play acoustic guitar (nylon)…       [QR] │
    │  aesthetic.computer/menuband           │
    └────────────────────────────────┘
"""

from __future__ import annotations

import argparse
from pathlib import Path

import qrcode
from PIL import Image, ImageDraw, ImageFont


CARD = 1080
PAD = 36

# ── color helpers ─────────────────────────────────────────────────────────


def rgb(s: str) -> tuple[int, int, int]:
    return tuple(int(x) for x in s.split(","))


def lighten(c, amt=0.85):
    return tuple(int(round(v + (255 - v) * amt)) for v in c)


def darken(c, amt=0.3):
    return tuple(int(round(v * (1 - amt))) for v in c)


# ── typography ────────────────────────────────────────────────────────────


def fit_font(path, text, max_width, max_size, min_size=12):
    size = max_size
    while size > min_size:
        f = ImageFont.truetype(path, size)
        bbox = f.getbbox(text)
        if (bbox[2] - bbox[0]) <= max_width:
            return f
        size -= 2
    return ImageFont.truetype(path, min_size)


def text_with_shadow(layer, xy, text, font, fill, shadow_fill, anchor="la",
                     shadow_offset=(4, 4), shadow_blur=False):
    draw = ImageDraw.Draw(layer)
    sx, sy = shadow_offset
    draw.text((xy[0] + sx, xy[1] + sy), text, font=font, fill=shadow_fill, anchor=anchor)
    draw.text(xy, text, font=font, fill=fill, anchor=anchor)


# ── illustration framing ─────────────────────────────────────────────────


def fit_illustration_full(raw_path: Path, target: int) -> Image.Image:
    img = Image.open(raw_path).convert("RGB")
    iw, ih = img.size
    scale = max(target / iw, target / ih)
    new = (int(iw * scale), int(ih * scale))
    img = img.resize(new, Image.LANCZOS)
    nx, ny = new
    return img.crop((
        (nx - target) // 2,
        (ny - target) // 2,
        (nx + target) // 2,
        (ny + target) // 2,
    ))


# ── gradients ────────────────────────────────────────────────────────────


def linear_alpha_band(width, height, start_alpha, end_alpha):
    """Create a single-channel L-mode mask, alpha varying top-to-bottom."""
    mask = Image.new("L", (width, height), 0)
    pixels = mask.load()
    for y in range(height):
        t = y / max(1, height - 1)
        a = int(round(start_alpha + (end_alpha - start_alpha) * t))
        for x in range(width):
            pixels[x, y] = a
    return mask


def overlay_color_band(card, rect, color, start_alpha, end_alpha):
    """Composite a vertical-gradient color band onto card (RGB)."""
    x0, y0, x1, y1 = rect
    w, h = x1 - x0, y1 - y0
    band = Image.new("RGB", (w, h), color)
    mask = linear_alpha_band(w, h, start_alpha, end_alpha)
    base = card.crop(rect).convert("RGB")
    blended = Image.composite(band, base, mask)
    card.paste(blended, (x0, y0))


# ── QR ───────────────────────────────────────────────────────────────────


def make_qr(target_url: str, size: int, fg=(20, 20, 24), bg=(255, 255, 255)) -> Image.Image:
    qr = qrcode.QRCode(
        version=None,
        error_correction=qrcode.constants.ERROR_CORRECT_M,
        box_size=10,
        border=2,
    )
    qr.add_data(target_url)
    qr.make(fit=True)
    return qr.make_image(fill_color=fg, back_color=bg).convert("RGB").resize(
        (size, size), Image.NEAREST
    )


# ── composer ─────────────────────────────────────────────────────────────


def compose(args):
    fam_rgb = rgb(args.family_rgb)
    fam_dark = darken(fam_rgb, 0.55)
    fam_light = lighten(fam_rgb, 0.86)
    white = (252, 248, 240)
    near_black = (16, 14, 18)

    program_num = int(args.num)
    num_str = str(program_num)  # no leading zeros, per request

    # ── 1. full-bleed illustration ───────────────────────────────────────
    card = fit_illustration_full(Path(args.raw), CARD).convert("RGB")
    draw = ImageDraw.Draw(card)

    # ── 2. translucent header gradient (family color, fades down) ────────
    header_h = 360
    overlay_color_band(
        card, (0, 0, CARD, header_h), fam_dark,
        start_alpha=190, end_alpha=0,
    )

    # ── 3. translucent footer gradient (dark, fades up) ──────────────────
    footer_h = 280
    overlay_color_band(
        card, (0, CARD - footer_h, CARD, CARD), (8, 6, 14),
        start_alpha=0, end_alpha=210,
    )

    # ── 4. brand pill top-left ───────────────────────────────────────────
    brand_font = ImageFont.truetype(args.font_bold, 26)
    brand_text = "MENU BAND"
    bb = brand_font.getbbox(brand_text)
    bw = bb[2] - bb[0] + 26
    bh = bb[3] - bb[1] + 16
    bx = PAD
    by = 28
    pill = Image.new("RGBA", (bw, bh), fam_rgb + (235,))
    pdraw = ImageDraw.Draw(pill)
    pdraw.text((bw / 2, bh / 2), brand_text, font=brand_font, fill=white, anchor="mm")
    card.paste(pill, (bx, by), pill)

    # ── 5. MASSIVE program number, white with family-dark shadow ─────────
    num_max_w = CARD - 2 * PAD
    num_font = fit_font(args.font_bold, num_str, num_max_w, 380, min_size=160)
    nb = num_font.getbbox(num_str)
    num_w = nb[2] - nb[0]
    num_h = nb[3] - nb[1]
    num_x = (CARD - num_w) // 2 - nb[0]
    num_y = 96
    # heavy reverse-color drop shadow for legibility against any background
    for dx, dy, alpha in [(8, 8, 200), (5, 5, 230)]:
        shadow = Image.new("RGBA", card.size, (0, 0, 0, 0))
        sdraw = ImageDraw.Draw(shadow)
        sdraw.text(
            (num_x + dx, num_y + dy),
            num_str,
            font=num_font,
            fill=fam_dark + (alpha,),
        )
        card.paste(shadow, (0, 0), shadow)
    draw.text((num_x, num_y), num_str, font=num_font, fill=white)

    # ── 6. instrument name centered under number ─────────────────────────
    name_upper = args.name.upper()
    name_max_w = CARD - 2 * (PAD + 30)
    name_font = fit_font(args.font_bold, name_upper, name_max_w, 56, min_size=24)
    nmb = name_font.getbbox(name_upper)
    name_w = nmb[2] - nmb[0]
    name_h = nmb[3] - nmb[1]
    name_y = num_y + nb[3] + 16
    name_x = (CARD - name_w) // 2 - nmb[0]
    # dark drop shadow then white fill
    for dx, dy in [(3, 3), (2, 2)]:
        draw.text((name_x + dx, name_y + dy), name_upper, font=name_font, fill=near_black)
    draw.text((name_x, name_y), name_upper, font=name_font, fill=white)

    # ── 7. footer text (left side) ───────────────────────────────────────
    name_lc = args.name.lower()
    line1 = f"enter menuband and type {num_str} to play"
    line1_font = ImageFont.truetype(args.font_bold, 32)
    instr_max_w = CARD - 2 * PAD - 200  # reserve QR area
    line2 = f"{name_lc}..."
    line2_font = fit_font(args.font_bold, line2, instr_max_w, 50, min_size=22)
    url_font = ImageFont.truetype(args.font_regular, 22)

    fx = PAD
    line1_y = CARD - footer_h + 70
    line2_y = line1_y + 44
    url_y = CARD - 50

    for txt, fnt, y, fill in [
        (line1, line1_font, line1_y, (240, 236, 230)),
        (line2, line2_font, line2_y, fam_light),
        (args.url, url_font, url_y, white),
    ]:
        # subtle drop shadow on translucent footer
        draw.text((fx + 2, y + 2), txt, font=fnt, fill=(0, 0, 0), anchor="la")
        draw.text((fx, y), txt, font=fnt, fill=fill, anchor="la")

    # ── 8. QR code, bottom-right ─────────────────────────────────────────
    qr_size = 168
    qr_x = CARD - PAD - qr_size
    qr_y = CARD - PAD - qr_size
    qr_img = make_qr(args.qr_target, qr_size)
    pad_q = 8
    draw.rectangle(
        [qr_x - pad_q, qr_y - pad_q, qr_x + qr_size + pad_q, qr_y + qr_size + pad_q],
        fill=white,
        outline=fam_dark,
        width=3,
    )
    card.paste(qr_img, (qr_x, qr_y))

    # ── 9. outer family-color frame ──────────────────────────────────────
    frame_w = 12
    draw.rectangle([0, 0, CARD - 1, CARD - 1], outline=fam_dark, width=frame_w)

    # ── save ─────────────────────────────────────────────────────────────
    out = Path(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    card.save(out, "PNG", optimize=True)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--raw", required=True)
    p.add_argument("--out", required=True)
    p.add_argument("--num", required=True)
    p.add_argument("--name", required=True)
    p.add_argument("--family", required=True)
    p.add_argument("--family-rgb", required=True)
    p.add_argument("--font-regular", required=True)
    p.add_argument("--font-bold", required=True)
    p.add_argument("--url", required=True)
    p.add_argument("--qr-target", required=True)
    args = p.parse_args()
    compose(args)


if __name__ == "__main__":
    main()
