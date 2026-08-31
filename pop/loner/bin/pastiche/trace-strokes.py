"""Trace the loner drawing into named vector strokes.

Geometry comes from SPACE, timing from TIME: Zhang-Suen thinning of
the finished ink mask (viz/wg-appear.npz final_mask) yields the
drawing's skeleton; the skeleton graph splits into branch segments at
junctions and endpoints; each segment inherits the appear-times of its
ink (12 fps stamps of when each pixel was drawn in the source take),
which names it — median time picks its wordclock mark/syllable — and
orients it (time increases the way the pen moved). Spurs shorter than
7px are pruned; segments smooth and resample to ~5px spacing.

Writes viz/wg-strokes.json:
  {w, h, strokes:[{name, word, v0, v1, sub:[[[x,y],...],...]}]}
sub-paths sit in drawing order within each mark.
"""
import json
import os

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
LONER = os.path.dirname(os.path.dirname(HERE))

z = np.load(f"{LONER}/viz/wg-appear.npz")
appear = z["appear"]
mask = z["final_mask"].astype(bool)
H, W = mask.shape
clock = json.load(open(f"{LONER}/viz/wordclock.json"))[:20]  # pass one


def thin(m):
    """Zhang-Suen thinning, vectorized over shifted views."""
    m = m.copy()
    while True:
        changed = False
        for phase in (0, 1):
            p = np.pad(m, 1)
            P2 = p[:-2, 1:-1]; P3 = p[:-2, 2:]; P4 = p[1:-1, 2:]
            P5 = p[2:, 2:]; P6 = p[2:, 1:-1]; P7 = p[2:, :-2]
            P8 = p[1:-1, :-2]; P9 = p[:-2, :-2]
            ring = [P2, P3, P4, P5, P6, P7, P8, P9]
            B = sum(x.astype(int) for x in ring)
            A = sum(((~ring[i]) & ring[(i + 1) % 8]).astype(int)
                    for i in range(8))
            if phase == 0:
                cond = (~(P2 & P4 & P6)) & (~(P4 & P6 & P8))
            else:
                cond = (~(P2 & P4 & P8)) & (~(P2 & P6 & P8))
            kill = m & (B >= 2) & (B <= 6) & (A == 1) & cond
            if kill.any():
                m &= ~kill
                changed = True
        if not changed:
            return m


skel = thin(mask)
sy, sx = np.where(skel)
print(f"skeleton: {sy.size} px from {mask.sum()} ink px")

# 8-connected graph over skeleton pixels
index = {(int(x), int(y)): i for i, (x, y) in enumerate(zip(sx, sy))}
NBR = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
deg = np.zeros(sy.size, int)
for (x, y), i in index.items():
    deg[i] = sum((x + dx, y + dy) in index for dx, dy in NBR)

# walk branch segments between nodes (endpoints / junctions)
visited = set()
segments = []
nodes = [p for p, i in index.items() if deg[i] != 2]
for start in nodes + list(index):  # loops (all-deg-2) picked up second
    si = index[start]
    if deg[si] == 2 and any(start in s for s in ()):  # placeholder no-op
        pass
    for dx, dy in NBR:
        nxt = (start[0] + dx, start[1] + dy)
        if nxt not in index:
            continue
        edge = frozenset((start, nxt))
        if edge in visited:
            continue
        path = [start, nxt]
        visited.add(edge)
        while deg[index[path[-1]]] == 2:
            cx, cy = path[-1]
            step = None
            for ex, ey in NBR:
                cand = (cx + ex, cy + ey)
                if cand in index and cand != path[-2]:
                    e2 = frozenset((path[-1], cand))
                    if e2 not in visited:
                        step = cand
                        visited.add(e2)
                        break
            if step is None:
                break
            path.append(step)
        if len(path) >= 2:
            segments.append(path)

segments = [s for s in segments if len(s) >= 4]  # prune spurs
print(f"{len(segments)} branch segments")

# name + orient each segment by the appear-times along it
bounds = np.array([e["v0"] for e in clock] + [clock[-1]["v1"]])
named = {k: [] for k in range(len(clock))}
for path in segments:
    t = np.array([appear[y, x] / 12.0 for x, y in path])
    t = np.clip(t, 0, 26)
    med = float(np.median(t))
    if med <= 2.26:  # first-burst ink (pre-zoom-out) = the head arc
        k = 0
    else:
        k = int(np.searchsorted(bounds, med, "right")) - 1
        k = max(0, min(len(clock) - 1, k))
    u = np.arange(len(t))
    if np.corrcoef(u, t)[0, 1] < 0:  # pen moved the other way
        path = path[::-1]
        t = t[::-1]
    named[k].append((float(np.median(t[: max(3, len(t) // 4)])), path))

strokes = []
for k, e in enumerate(clock):
    subs = []
    for _, path in sorted(named[k], key=lambda r: r[0]):
        P = np.array(path, float)
        if len(P) >= 3:
            P[1:-1] = (P[:-2] + P[1:-1] + P[2:]) / 3
        d = np.concatenate([[0],
                            np.cumsum(np.hypot(*np.diff(P, axis=0).T))])
        if d[-1] > 5:
            u = np.arange(0, d[-1] + 2.5, 5.0)
            P = np.stack([np.interp(u, d, P[:, 0]),
                          np.interp(u, d, P[:, 1])], 1)
        subs.append([[round(float(x), 1), round(float(y), 1)]
                     for x, y in P])
    if not subs:
        continue
    strokes.append({
        "name": e["mark"], "word": e["word"],
        "v0": e["v0"], "v1": e["v1"], "sub": subs,
    })
    print(f'{e["mark"]:16s} {e["word"]:10s} v {e["v0"]:5.2f}-{e["v1"]:5.2f}'
          f'  {len(subs)} sub, {sum(len(s) for s in subs):3d} pts')

json.dump({"w": W, "h": H, "strokes": strokes},
          open(f"{LONER}/viz/wg-strokes.json", "w"))
print(f"-> viz/wg-strokes.json ({len(strokes)} strokes)")
