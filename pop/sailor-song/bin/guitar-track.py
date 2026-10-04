#!/usr/bin/env python
# guitar-track.py — where her guitar is, frame by frame, so the rays can come off it (perf-rays.mjs).
#
# The camera is fixed but she sways, and her strumming hand sits over the sound hole half the time, so a
# template on the hole alone drops out. The guitar is rigid, though: SIFT features on the rosette, bridge,
# frets and tuners are matched from one reference frame to every frame of the take, a similarity transform
# is fit with RANSAC (her hands are the outliers), and the reference's sound-hole centre and a point on the
# string line near the nut ride that transform. Frames with too few inliers are holes in the track, filled
# by interpolation; then median + low-pass smoothing and a per-frame jump clamp. Take clock, 960×540.
#
#   pop/.venv/bin/python pop/sailor-song/bin/guitar-track.py [--ref 5.0] [--check DIR]
#     → src/guitar-track.json  [{t, hole:[x,y], head:[x,y]}] at the take's 30 fps
import cv2, json, sys, os, numpy as np
from scipy.signal import medfilt, butter, filtfilt

HERE = os.path.dirname(os.path.abspath(__file__)); LANE = os.path.dirname(HERE)
SRC = os.path.join(LANE, "src/take.mov"); OUT = os.path.join(LANE, "src/guitar-track.json")
arg = lambda k, d=None: sys.argv[sys.argv.index(f"--{k}") + 1] if f"--{k}" in sys.argv else d
REF_T = float(arg("ref", 5.0)); CHECK = arg("check")
HOLE0 = np.float32([242, 402]); HEAD0 = np.float32([900, 318])      # measured on the reference frame (take 5 s)

cap = cv2.VideoCapture(SRC); FPS = cap.get(cv2.CAP_PROP_FPS) or 30.0; N = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
W, H = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH)), int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
assert (W, H) == (960, 540), (W, H)

# the guitar in the reference frame: body + neck, as one polygon; the per-frame search mask is this, dilated
GUITAR = np.int32([[(40, 470), (120, 350), (260, 300), (420, 300), (960, 215), (960, 380), (430, 440), (300, 540), (60, 540)]])
def grab(i):
    cap.set(cv2.CAP_PROP_POS_FRAMES, i); ok, f = cap.read(); return cv2.cvtColor(f, cv2.COLOR_BGR2GRAY) if ok else None
sift = cv2.SIFT_create(nfeatures=3000)
refMask = np.zeros((H, W), np.uint8); cv2.fillPoly(refMask, GUITAR, 255)
searchMask = cv2.dilate(refMask, np.ones((121, 121), np.uint8))
ref = grab(int(round(REF_T * FPS))); rk, rd = sift.detectAndCompute(ref, refMask)
bf = cv2.BFMatcher(cv2.NORM_L2)
print(f"▸ reference at {REF_T}s: {len(rk)} features; tracking {N} frames", flush=True)

cap.set(cv2.CAP_PROP_POS_FRAMES, 0)
raw = np.full((N, 4), np.nan, np.float32); inl = np.zeros(N, np.int32)
for i in range(N):
    ok, f = cap.read()
    if not ok: break
    g = cv2.cvtColor(f, cv2.COLOR_BGR2GRAY); k, d = sift.detectAndCompute(g, searchMask)
    if d is None or len(k) < 20: continue
    good = [m for m, n in bf.knnMatch(rd, d, k=2) if m.distance < 0.75 * n.distance]
    if len(good) < 12: continue
    src = np.float32([rk[m.queryIdx].pt for m in good]); dst = np.float32([k[m.trainIdx].pt for m in good])
    M, ok = cv2.estimateAffinePartial2D(src, dst, method=cv2.RANSAC, ransacReprojThreshold=4.0, confidence=0.995, maxIters=3000)
    if M is None or ok.sum() < 12: continue
    s = np.hypot(M[0, 0], M[0, 1])
    if not (0.8 < s < 1.25): continue                                 # the guitar does not grow; a wild fit is a miss
    pts = (M[:, :2] @ np.stack([HOLE0, HEAD0], 1)).T + M[:, 2]
    raw[i] = pts.ravel(); inl[i] = int(ok.sum())
    if i % 300 == 0: print(f"\r  {100 * i // N}%  inliers {inl[i]}", end="", flush=True)
cap.release()
have = ~np.isnan(raw[:, 0]); print(f"\r▸ fit on {have.sum()}/{N} frames; lost runs:", end=" ")
runs, j = [], 0
while j < N:
    if not have[j]:
        a = j
        while j < N and not have[j]: j += 1
        if j - a >= 6: runs.append((a / FPS, j / FPS))
    else: j += 1
print(", ".join(f"{a:.1f}–{b:.1f}s" for a, b in runs) or "none")

# fill the misses, then settle: median (9 frames) kills single bad fits, 2 Hz low-pass takes out jitter, and
# no point may move more than 12 px per frame (she sways, she does not teleport)
t = np.arange(N) / FPS; idx = np.where(have)[0]
sm = np.stack([np.interp(t, t[idx], raw[idx, c]) for c in range(4)], 1)
sm = np.stack([medfilt(sm[:, c], 9) for c in range(4)], 1)
b, a = butter(2, 2.0 / (FPS / 2)); sm = filtfilt(b, a, sm, axis=0)
for c in (0, 2):
    for i in range(1, N):
        d = sm[i, c:c + 2] - sm[i - 1, c:c + 2]; n = np.hypot(*d)
        if n > 12: sm[i, c:c + 2] = sm[i - 1, c:c + 2] + d * (12 / n)
json.dump([{"t": round(float(t[i]), 4), "hole": [round(float(sm[i, 0]), 1), round(float(sm[i, 1]), 1)], "head": [round(float(sm[i, 2]), 1), round(float(sm[i, 3]), 1)]} for i in range(N)],
          open(OUT, "w"), separators=(",", ":"))
print(f"✓ {OUT}  ({N} frames; hole drifts x {sm[:, 0].min():.0f}–{sm[:, 0].max():.0f}, y {sm[:, 1].min():.0f}–{sm[:, 1].max():.0f})")

# --check DIR: six frames across the take with the track drawn on, to look at
if CHECK:
    os.makedirs(CHECK, exist_ok=True); cap = cv2.VideoCapture(SRC)
    for tt in np.linspace(3, N / FPS - 3, 6):
        i = int(round(tt * FPS)); cap.set(cv2.CAP_PROP_POS_FRAMES, i); ok, f = cap.read()
        hx, hy, ex, ey = sm[i]; cv2.line(f, (int(hx), int(hy)), (int(ex), int(ey)), (0, 255, 255), 2)
        cv2.circle(f, (int(hx), int(hy)), 22, (0, 255, 0), 2); cv2.circle(f, (int(ex), int(ey)), 8, (255, 0, 255), 2)
        cv2.putText(f, f"t={tt:.1f}s inl={inl[i]}", (10, 530), cv2.FONT_HERSHEY_SIMPLEX, 0.7, (255, 255, 255), 2)
        cv2.imwrite(os.path.join(CHECK, f"track-{int(tt):03d}.png"), f)
    print(f"▸ check frames in {CHECK}")
