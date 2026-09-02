#!/usr/bin/env python3
# tempo.py — the pick-up: "as the swing is added we pick up the bpm even, so
# we can keep up the energy" (@jeffrey). The score is a fixed 120 grid; this
# warps the finished pre-master through rubberband-r3 (pitch preserved) on a
# time map whose tempo follows the SAME curve as the swing — 120 through bar
# 44, a smoothstep to 128 at bar 104 — and prints the warped seconds of the
# release edit's trim points so cut-release.sh can cut the warped file.
#
#   python3 bin/tempo.py <in.wav> <out.wav> [--print-env]
import sys, subprocess, math
SR, BAR, BARS = 48000, 2.0, 112
src, dst = sys.argv[1], sys.argv[2]
def smooth(u): u = max(0.0, min(1.0, u)); return u * u * (3 - 2 * u)
def bpm(bar): return 120.0 + 8.0 * smooth((bar - 44) / 60.0)
# warped time of every source time, integrated bar by bar (tempo is per bar)
edges = [0.0]
for b in range(BARS + 2):
    edges.append(edges[-1] + BAR * 120.0 / bpm(b))
def warp(s):
    b = int(s // BAR); f = s - b * BAR
    if b >= len(edges) - 1: b = len(edges) - 2; f = s - b * BAR
    return edges[b] + f * 120.0 / bpm(b)
total = 227.2
mapfile = dst + ".map"
with open(mapfile, "w") as m:
    for b in range(BARS + 2):
        m.write(f"{int(b * BAR * SR)} {int(edges[b] * SR)}\n")
D = warp(total)
subprocess.run(["rubberband-r3", "--timemap", mapfile, "-D", f"{D:.4f}", src, dst], check=True,
               stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
if "--print-env" in sys.argv:
    for k, v in {"T_A0": 15.95, "T_A1": 20, "T_B1S": 58, "T_B1E": 74, "T_B2S": 79.76, "T_B2E": 120,
                 "T_CS": 127.76, "T_CE": 136, "T_DS": 143.76, "T_DE": 167.95,
                 "T_E1S": 183.71, "T_E1E": 192, "T_E2S": 207.76, "T_E2E": 224.80}.items():
        print(f"export {k}={warp(v):.4f}")
    print(f"# warped total {D:.3f}s; bpm 120 → {bpm(104):.1f} by bar 104", file=sys.stderr)
