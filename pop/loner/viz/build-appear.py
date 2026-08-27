#!/usr/bin/env python3
# Build the ink appearance-time map for the loner whistlegraph source video.
# Frames: wg-gray.raw, 452x698 gray u8, 12 fps, cropped to the drawing area.
import numpy as np
from PIL import Image

WW, WH, VFPS = 452, 698, 12.0
S = "/private/tmp/claude-501/-Users-jas-aesthetic-computer/df296e24-513a-488b-b96b-31cb958c1bda/scratchpad"

d = np.fromfile(f"{S}/wg-gray.raw", np.uint8)
NFR = len(d) // (WW * WH)
F = d.reshape(NFR, WH, WW)

# final ink mask from the mean of the last 6 clean frames (no hand present)
final = F[-6:].mean(axis=0)
paper = np.median(final)
FINAL_T = paper * 0.80          # generous: catches faint stroke edges
final_mask = final < FINAL_T
print(f"paper={paper:.0f} FINAL_T={FINAL_T:.0f} final ink px={final_mask.sum()}"
      f" ({final_mask.mean()*100:.2f}%)")

# The first ~2.2s are a nib close-up that zooms out: geometry does not match
# the settled wide view, so skip those frames entirely.
I0 = 26

def erode(mask, r):
    out = mask
    for axis in (0, 1):
        acc = out
        for s in range(1, r + 1):
            acc = acc & np.roll(out, s, axis=axis) & np.roll(out, -s, axis=axis)
        out = acc
    return out

def dilate(mask, r):
    out = mask
    for axis in (0, 1):
        acc = out
        for s in range(1, r + 1):
            acc = acc | np.roll(out, s, axis=axis) | np.roll(out, -s, axis=axis)
        out = acc
    return out

# Ink candidates per frame: dark pixels MINUS the big blob (hand + pen).
# Strokes are 3-7 px wide at this scale; the hand/pen barrel is 60+ px, so
# erode(r=6) keeps only the blob core, dilate(r=12) re-covers its margins.
def candidates(th):
    cand = np.empty(F.shape, bool)
    for i in range(NFR):
        dk = F[i] < th
        blob = dilate(erode(dk, 6), 12)
        cand[i] = dk & ~blob
    return cand

print("computing candidate masks...", flush=True)
candS = candidates(paper * 0.62)     # strict: confident ink
candL = candidates(paper * 0.78)     # loose: faint stroke edges

def first_stamp(cand):
    first = np.full((WH, WW), -1, np.int32)
    for i in range(I0, NFR - 4):
        hit = cand[i] & cand[i + 2] & cand[i + 4] & (first < 0)
        first[hit] = i
    return first

first = first_stamp(candS)
strict_cov = ((first >= 0) & final_mask).sum() / final_mask.sum()
firstL = first_stamp(candL)

appear = np.where(first >= 0, first, firstL).astype(np.float32)
missing = (appear < 0) & final_mask
loose_cov = (~missing & final_mask).sum() / final_mask.sum()

# Fill missing pixels (dense knot core the blob filter ate) from the mean
# appear time of stamped neighbors, expanding outward.
known = (appear >= 0) & final_mask
vals = np.where(known, appear, 0.0)
for _ in range(8):
    if not (missing & ~known).any() and missing.sum() == known[missing].sum():
        break
    ksum = np.zeros_like(vals)
    kcnt = np.zeros((WH, WW), np.float32)
    for dy in (-2, -1, 0, 1, 2):
        for dx in (-2, -1, 0, 1, 2):
            ksum += np.roll(np.roll(vals, dy, 0), dx, 1)
            kcnt += np.roll(np.roll(known.astype(np.float32), dy, 0), dx, 1)
    fill = missing & ~known & (kcnt > 0)
    vals[fill] = ksum[fill] / kcnt[fill]
    known |= fill
    if not (missing & ~known).any():
        break
appear = np.where(known, vals, np.float32(NFR - 1))
still = (missing & ~known).sum()
appear = np.where(final_mask, appear, np.float32(1e9))  # non-ink: never
print(f"strict coverage {strict_cov*100:.1f}%  after loose {loose_cov*100:.1f}%"
      f"  missing filled {missing.sum() - still}, to-last {still}")

T_LAST = appear[final_mask].max() / VFPS
print(f"T_LAST={T_LAST:.2f}s of {NFR/VFPS:.2f}s video")

np.savez_compressed(f"{S}/wg-appear.npz", appear=appear,
                    final=final.astype(np.float32), paper=paper,
                    final_mask=final_mask)

# --- visual QA -------------------------------------------------------------
# 1. appearance map colored by time (blue early -> red late), grey = missing
t = appear / (T_LAST * VFPS)
vis = np.full((WH, WW, 3), 255, np.uint8)
m = final_mask & (appear < 1e8)
tt = np.clip(t, 0, 1)
vis[m] = np.stack([(tt[m] * 255), np.full(m.sum(), 40), (255 - tt[m] * 255)],
                  axis=1).astype(np.uint8)
vis[missing] = (0, 200, 0)
Image.fromarray(vis).save(f"{S}/qa-appear-map.png")

# 2. reveal snapshots at several fractions of T_LAST
ink_alpha = np.clip((paper - final) / (paper - 60.0), 0, 1)
for frac in (0.1, 0.3, 0.5, 0.75, 1.0):
    cut = frac * T_LAST * VFPS
    rev = (appear <= cut)
    a = np.where(rev, ink_alpha, 0.0)
    img = (245 - a * (245 - 42)).astype(np.uint8)
    Image.fromarray(img).convert("RGB").save(f"{S}/qa-reveal-{int(frac*100):03d}.png")
print("QA images written")
