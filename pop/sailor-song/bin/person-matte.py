#!/usr/bin/env python
# person-matte.py — her and the guitar, cut from the room, frame by frame, so perf-relight.mjs can light the room
# behind her and leave her natural. MediaPipe's selfie segmenter (CPU, ~5 ms a frame at 960×540) gives her; the
# guitar is unioned in as a body ellipse + neck quad + headstock around src/guitar-track.json's per-frame hole and
# string line (it already catches most of the guitar, the union makes the headstock and lower bout sure). Threshold
# high, erode 3 px so no lit wall rides along her hair, feather ~8 px, store at quarter res as one raw 8-bit sequence (255 = her). Take clock, like the track.
#
#   pop/.venv/bin/python pop/sailor-song/bin/person-matte.py [--model PATH] [--check DIR]
#     → src/matte.json {w,h,n,fps,file} + src/matte-240x135.raw (n·w·h bytes)
import cv2, json, os, sys, urllib.request, numpy as np, mediapipe as mp
from mediapipe.tasks import python as mpp; from mediapipe.tasks.python import vision

HERE = os.path.dirname(os.path.abspath(__file__)); LANE = os.path.dirname(HERE)
arg = lambda k, d=None: sys.argv[sys.argv.index(f"--{k}") + 1] if f"--{k}" in sys.argv else d
MODEL = arg("model", os.path.join(LANE, "src/selfie_segmenter.tflite")); CHECK = arg("check")
if not os.path.exists(MODEL):
    urllib.request.urlretrieve("https://storage.googleapis.com/mediapipe-models/image_segmenter/selfie_segmenter/float16/latest/selfie_segmenter.tflite", MODEL)
TRACK = json.load(open(os.path.join(LANE, "src/guitar-track.json")))
seg = vision.ImageSegmenter.create_from_options(vision.ImageSegmenterOptions(base_options=mpp.BaseOptions(model_asset_path=MODEL),
      running_mode=vision.RunningMode.IMAGE, output_confidence_masks=True, output_category_mask=False))
cap = cv2.VideoCapture(os.path.join(LANE, "src/take.mov")); N = int(cap.get(cv2.CAP_PROP_FRAME_COUNT)); FPS = cap.get(cv2.CAP_PROP_FPS)
W, H = 960, 540; QW, QH = 240, 135
out = open(os.path.join(LANE, "src/matte-240x135.raw"), "wb"); keep = {}

def guitarMask(k):
    hx, hy = k["hole"]; ex, ey = k["head"]; L = np.hypot(ex - hx, ey - hy); ux, uy = (ex - hx) / L, (ey - hy) / L; nx, ny = uy, -ux
    m = np.zeros((H, W), np.uint8); ang = np.degrees(np.arctan2(uy, ux))
    cv2.ellipse(m, (int(hx + ux * 15 - nx * 25), int(hy + uy * 15 - ny * 25)), (205, 128), ang, 0, 360, 255, -1)      # the body
    P = lambda s, o: (int(hx + ux * s + nx * o), int(hy + uy * s + ny * o))
    cv2.fillPoly(m, [np.int32([P(140, 34), P(L * 0.98, 30), P(L * 0.98, -30), P(140, -34)])], 255)                      # the neck
    cv2.fillPoly(m, [np.int32([P(L * 0.95, 40), P(L * 1.14, 36), P(L * 1.14, -36), P(L * 0.95, -40)])], 255)             # the headstock
    return m
for i in range(N):
    ok, f = cap.read()
    if not ok: break
    res = seg.segment(mp.Image(image_format=mp.ImageFormat.SRGB, data=cv2.cvtColor(f, cv2.COLOR_BGR2RGB)))
    her = res.confidence_masks[0].numpy_view()[:, :, 0] > 0.7
    m = np.maximum(her.astype(np.uint8) * 255, guitarMask(TRACK[min(i, len(TRACK) - 1)]))
    m = cv2.GaussianBlur(cv2.erode(m, np.ones((7, 7), np.uint8)), (0, 0), 3.2)                                           # pull the edge in 3 px (no halo), then feather
    q = cv2.resize(m, (QW, QH), interpolation=cv2.INTER_AREA); out.write(q.tobytes())
    if CHECK and i in (900, 2400, 3600, 4800): keep[i] = (f, q)
    if i % 300 == 0: print(f"\r  {100 * i // N}%", end="", flush=True)
out.close(); json.dump({"w": QW, "h": QH, "n": i + 1 if not ok else N, "fps": FPS, "file": "matte-240x135.raw"}, open(os.path.join(LANE, "src/matte.json"), "w"))
print(f"\r✓ src/matte-240x135.raw  {N} frames at {QW}×{QH}")
if CHECK:
    os.makedirs(CHECK, exist_ok=True); rows = []
    for i, (f, q) in keep.items():
        a = cv2.resize(q, (W, H), interpolation=cv2.INTER_LINEAR).astype(np.float32) / 255
        dark = (f * (0.25 + 0.75 * a[:, :, None])).astype(np.uint8); rows.append(np.hstack([f, dark]))
    cv2.imwrite(os.path.join(CHECK, "matte.png"), cv2.resize(np.vstack(rows), None, fx=0.5, fy=0.5))
