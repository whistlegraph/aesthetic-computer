#!/usr/bin/env python3
"""composite-screen.py — paste a REAL KidLisp screenshot onto the felt laptop's
green-screen, perspective-correct.

Detects the chroma-green screen quad in a felt still, warps the screenshot to
that quad, and replaces ONLY the green pixels (so a bunny ear in front of the
screen is preserved).

  python3 composite-screen.py --felt gens/green-a.png \
      --screen ../../captures/platter/kidlisp-roz.png --out gens/shot-a.png
"""
import argparse
import numpy as np
from PIL import Image


def find_coeffs(out_pts, in_pts):
    # PIL transform maps OUTPUT(x,y) → INPUT. Returns coeffs so each out_pts[i]
    # samples in_pts[i]. To spread a full screenshot across a quad, call with
    # out_pts = the quad, in_pts = the screenshot's full corners.
    m = []
    for (ox, oy), (ix, iy) in zip(out_pts, in_pts):
        m.append([ox, oy, 1, 0, 0, 0, -ix * ox, -ix * oy])
        m.append([0, 0, 0, ox, oy, 1, -iy * ox, -iy * oy])
    A = np.array(m, dtype=float)
    B = np.array(in_pts, dtype=float).reshape(8)
    return np.linalg.solve(A, B)


def green_mask(rgb):
    r, g, b = rgb[..., 0].astype(int), rgb[..., 1].astype(int), rgb[..., 2].astype(int)
    return (g > 110) & (g - r > 45) & (g - b > 45)


def largest_blob(mask):
    # keep only the biggest 4-connected green region (drop stray green specks)
    from collections import deque
    h, w = mask.shape
    seen = np.zeros_like(mask, dtype=bool)
    best = None
    best_n = 0
    ys, xs = np.where(mask)
    for sy, sx in zip(ys, xs):
        if seen[sy, sx]:
            continue
        q = deque([(sy, sx)])
        seen[sy, sx] = True
        comp = []
        while q:
            y, x = q.popleft()
            comp.append((y, x))
            for dy, dx in ((1, 0), (-1, 0), (0, 1), (0, -1)):
                ny, nx = y + dy, x + dx
                if 0 <= ny < h and 0 <= nx < w and mask[ny, nx] and not seen[ny, nx]:
                    seen[ny, nx] = True
                    q.append((ny, nx))
        if len(comp) > best_n:
            best_n = len(comp)
            best = comp
    out = np.zeros_like(mask, dtype=bool)
    for y, x in best:
        out[y, x] = True
    return out


def quad_corners(mask):
    ys, xs = np.where(mask)
    s, d = xs + ys, xs - ys
    tl = (int(xs[s.argmin()]), int(ys[s.argmin()]))
    br = (int(xs[s.argmax()]), int(ys[s.argmax()]))
    tr = (int(xs[d.argmax()]), int(ys[d.argmax()]))
    bl = (int(xs[d.argmin()]), int(ys[d.argmin()]))
    return [tl, tr, br, bl]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--felt", required=True)
    ap.add_argument("--screen", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--dim", type=float, default=0.92, help="brightness of pasted screen (felt-light realism)")
    a = ap.parse_args()

    felt = Image.open(a.felt).convert("RGB")
    fw, fh = felt.size
    arr = np.asarray(felt)

    mask = largest_blob(green_mask(arr))
    if mask.sum() < 200:
        raise SystemExit("✗ no green screen detected — is this a green-screen still?")
    dst = quad_corners(mask)
    print(f"  screen quad: {dst}  ({int(mask.sum())} px)")

    shot = Image.open(a.screen).convert("RGBA")
    sw, sh = shot.size
    src = [(0, 0), (sw, 0), (sw, sh), (0, sh)]
    coeffs = find_coeffs(dst, src)  # out_pts=quad, in_pts=screenshot corners
    warped = shot.transform((fw, fh), Image.PERSPECTIVE, coeffs, resample=Image.BICUBIC, fillcolor=(0, 0, 0, 0))

    if a.dim != 1.0:
        wa = np.asarray(warped).astype(float)
        wa[..., :3] *= a.dim
        warped = Image.fromarray(np.clip(wa, 0, 255).astype(np.uint8), "RGBA")

    # replace ONLY the green pixels (preserve anything in front of the screen)
    paste_mask = Image.fromarray((mask * 255).astype(np.uint8), "L")
    out = felt.convert("RGBA")
    out.paste(warped, (0, 0), paste_mask)
    out.convert("RGB").save(a.out)
    print(f"✓ {a.out}")


if __name__ == "__main__":
    main()
