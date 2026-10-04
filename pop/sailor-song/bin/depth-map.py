#!/usr/bin/env python
# depth-map.py — a depth map of every take frame, for perf-relight.mjs's stage light (v103: "front lighting on her
# face — depth model her face, bump-mapped style — to bring more stage lighting"). Depth Anything V2 (small) on the
# GPU, every second frame in batches (lighting is low-frequency; the frame between is the mean of its neighbours),
# relative depth normalised against a running range so it does not flicker, EMA'd, and written at half resolution
# as the luma of an H.264 clip the compositor streams (closer = brighter). Take clock, 30 fps.
#
#   pop/.venv-align/bin/python pop/sailor-song/bin/depth-map.py   → src/depth-take.mp4  (480×270, ~15 min)
import os, subprocess, sys, time, numpy as np, torch, cv2
from PIL import Image
from transformers import pipeline

LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__))); SRC = os.path.join(LANE, "src")
OUT = os.path.join(SRC, "depth-take.mp4"); STEP, BATCH, QW, QH = 2, 8, 480, 270
pipe = pipeline("depth-estimation", model="depth-anything/Depth-Anything-V2-Small-hf", device="mps")
cap = cv2.VideoCapture(os.path.join(SRC, "take.mov")); N = int(cap.get(7)); FPS = cap.get(5)
enc = subprocess.Popen(["ffmpeg", "-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "gray", "-s", f"{QW}x{QH}", "-r", str(FPS), "-i", "-",
                        "-c:v", "libx264", "-crf", "12", "-preset", "fast", "-pix_fmt", "yuv420p", OUT], stdin=subprocess.PIPE)
lo = hi = None; prev = None; pending = []; t0 = time.time(); done = 0
def emit(q):
    enc.stdin.write(q.tobytes())
def infer(frames):
    global lo, hi, prev
    outs = pipe([Image.fromarray(cv2.cvtColor(f, cv2.COLOR_BGR2RGB)) for f in frames])
    res = []
    for o in outs:
        d = np.array(o["predicted_depth"], np.float32); d = cv2.resize(d, (QW, QH), interpolation=cv2.INTER_AREA)
        a, b = np.percentile(d, 1), np.percentile(d, 99)
        lo = a if lo is None else lo * 0.95 + a * 0.05; hi = b if hi is None else hi * 0.95 + b * 0.05          # a running range: no per-frame flicker
        q = np.clip((d - lo) / max(1e-6, hi - lo), 0, 1)
        q = q if prev is None else prev * 0.4 + q * 0.6; prev = q; res.append(q)
    return res
frames = []; i = 0
while True:
    ok, f = cap.read()
    if not ok: break
    if i % STEP == 0: frames.append(f)
    i += 1
    if len(frames) == BATCH or (not ok):
        for q in infer(frames):
            if pending: emit(((pending[-1] + q) / 2 * 255).astype(np.uint8))                                    # the skipped frame: the mean of its neighbours
            emit((q * 255).astype(np.uint8)); pending = [q]; done += STEP
        frames = []
        if done % 240 < STEP: print(f"\r  {100 * done // N}%  {done / (time.time() - t0):.1f} fps", end="", flush=True)
if frames:
    for q in infer(frames):
        if pending: emit(((pending[-1] + q) / 2 * 255).astype(np.uint8))
        emit((q * 255).astype(np.uint8)); pending = [q]; done += STEP
while done < N and pending: emit((pending[-1] * 255).astype(np.uint8)); done += 1
enc.stdin.close(); enc.wait(); print(f"\r✓ {OUT}  {N} frames at {QW}×{QH} in {(time.time() - t0) / 60:.1f} min")
