#!/usr/bin/env python
# room-lights.py — a map of the lights in her room, once, for perf-relight.mjs: the round ceiling lamp top-centre, the
# fairy lights strung along the ceiling edge (small blue/purple points: blue-excess top-hat on a median frame of the
# take, kept only near the ceiling line so her hair and the shark poster don't count), the window on the left.
# The camera is fixed, so one map serves the whole take. Take frame space, 960×540.
#
#   pop/.venv/bin/python pop/sailor-song/bin/room-lights.py [--frames out/.take-frames-30-540] [--check DIR]
#     → src/room-lights.json  {lamp:{x,y,r}, window:{x0,y0,x1,y1}, fairy:[[x,y],…], ceiling:[[x,y],…]}
import cv2, glob, json, os, sys, numpy as np

HERE = os.path.dirname(os.path.abspath(__file__)); LANE = os.path.dirname(HERE)
arg = lambda k, d=None: sys.argv[sys.argv.index(f"--{k}") + 1] if f"--{k}" in sys.argv else d
FRAMES = arg("frames", os.path.join(LANE, "out/.take-frames-30-540")); CHECK = arg("check")
fs = sorted(glob.glob(os.path.join(FRAMES, "f*.jpg")))
if fs: med = np.median(np.stack([cv2.imread(f) for f in fs[600::180]]), 0).astype(np.uint8)
else:                                                                  # no cache: read the take itself
    cap = cv2.VideoCapture(os.path.join(LANE, "src/take.mov")); fr = []
    for i in range(600, 5300, 180): cap.set(cv2.CAP_PROP_POS_FRAMES, i); ok, f = cap.read(); fr.append(f)
    med = np.median(np.stack(fr), 0).astype(np.uint8)

# the ceiling edge the string follows: left wall corner, back wall, up the right corner (measured on the median)
CEILING = [(0, 112), (230, 256), (700, 222), (718, 0)]
m = med.astype(int); b, g, r = m[:, :, 0], m[:, :, 1], m[:, :, 2]
blue = (b - (r + g) / 2).astype(np.float32)
th = cv2.morphologyEx(blue, cv2.MORPH_TOPHAT, np.ones((9, 9), np.uint8))
cand = (th > 10) & (b > 150); cand[300:, :] = False
n, lab, st, cen = cv2.connectedComponentsWithStats(cand.astype(np.uint8))
def nearLine(p):
    x, y = p; best = 1e9
    for (ax, ay), (bx, by) in zip(CEILING, CEILING[1:]):
        t = max(0, min(1, ((x - ax) * (bx - ax) + (y - ay) * (by - ay)) / ((bx - ax) ** 2 + (by - ay) ** 2)))
        best = min(best, np.hypot(x - (ax + t * (bx - ax)), y - (ay + t * (by - ay))))
    return best
fairy = [[round(float(cen[i][0]), 1), round(float(cen[i][1]), 1)] for i in range(1, n) if st[i][4] <= 40 and nearLine(cen[i]) < 14]
fairy.sort()
lights = {"lamp": {"x": 318, "y": 42, "r": 62}, "window": {"x0": 0, "y0": 225, "x1": 215, "y1": 470}, "fairy": fairy, "ceiling": CEILING}
json.dump(lights, open(os.path.join(LANE, "src/room-lights.json"), "w"))
print(f"✓ src/room-lights.json  lamp ({lights['lamp']['x']},{lights['lamp']['y']}) r{lights['lamp']['r']} · {len(fairy)} fairy lights · window {lights['window']}")
if CHECK:
    os.makedirs(CHECK, exist_ok=True); vis = med.copy()
    for x, y in fairy: cv2.circle(vis, (int(x), int(y)), 5, (0, 255, 0), 1)
    L, Wn = lights["lamp"], lights["window"]; cv2.circle(vis, (L["x"], L["y"]), L["r"], (0, 200, 255), 2)
    cv2.rectangle(vis, (Wn["x0"], Wn["y0"]), (Wn["x1"], Wn["y1"]), (255, 200, 0), 2); cv2.polylines(vis, [np.int32(CEILING)], False, (255, 0, 255), 1)
    cv2.imwrite(os.path.join(CHECK, "room-lights.png"), vis)
