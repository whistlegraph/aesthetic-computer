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


# ROUTE the pen along the drawing: geometry is the global skeleton
# graph (bridged across small ink gaps), order is the per-slice pen
# tip tracked through time, and segmentation is the GESTURE TABLE read
# visually off the footage — one whistlegraph gesture can span six
# words, each syllable of "patiently" is its own hair strand. Between
# waypoints the pen walks the skeleton's shortest path, so every line
# is continuous real ink in real drawing order.
from collections import deque

NBR = [(-1, -1), (0, -1), (1, -1), (-1, 0),
       (1, 0), (-1, 1), (0, 1), (1, 1)]

skel = thin(mask)
ky, kx = np.where(skel)
pix = list(zip(kx.tolist(), ky.tolist()))
pixset = set(pix)
adj = {p: [q for dx, dy in NBR
           if (q := (p[0] + dx, p[1] + dy)) in pixset] for p in pix}

# bridge skeleton components across small marker gaps (<14px)
comp_id = {}
for p in pix:
    if p in comp_id:
        continue
    cid = len(set(comp_id.values()))
    q = deque([p])
    comp_id[p] = cid
    while q:
        u = q.popleft()
        for v in adj[u]:
            if v not in comp_id:
                comp_id[v] = cid
                q.append(v)
P_ARR = np.array(pix, float)
C_ARR = np.array([comp_id[p] for p in pix])
merged = True
while merged:
    merged = False
    for ca in np.unique(C_ARR):
        A = P_ARR[C_ARR == ca]
        B = P_ARR[C_ARR != ca]
        if not len(B):
            continue
        d2 = ((A[:, None, :] - B[None, :, :]) ** 2).sum(-1)
        i, j = np.unravel_index(d2.argmin(), d2.shape)
        if d2[i, j] < 14 ** 2:
            a = tuple(int(v) for v in A[i])
            b = tuple(int(v) for v in B[j])
            adj[a].append(b)
            adj[b].append(a)
            C_ARR[C_ARR == C_ARR[np.where((P_ARR == B[j]).all(1))[0][0]]]                 = ca
            merged = True
            break

# pen-tip waypoints: per 1/12s slice, ink centroid tracked near the
# previous tip (occlusion reveals get ignored)
ys, xs = np.where(mask)
stamps_all = appear[ys, xs].astype(int)
way = []
# the head arc is one stamp burst (the take opens on a zooming nib
# close-up), so no time order exists inside it — walk it by angle and
# spread synthetic waypoints across "sitting"'s window instead
burst = stamps_all <= 27
bx, by = xs[burst].astype(float), ys[burst].astype(float)
rad = np.hypot(bx - bx.mean(), by - by.mean())
ring = np.abs(rad - np.median(rad)) < 20
bx, by = bx[ring], by[ring]
ang = np.arctan2(by - by.mean(), bx - bx.mean())
o = np.argsort(ang)
a_sorted = ang[o]
gaps = np.diff(np.concatenate([a_sorted, [a_sorted[0] + 2 * np.pi]]))
o = np.roll(o, -((gaps.argmax() + 1) % len(o)))
bins = [b for b in np.array_split(o, 14) if b.size]
for i, b in enumerate(bins):
    t = 0.33 + (1.74 - 0.33) * i / max(1, len(bins) - 1)
    way.append((t, (float(bx[b].mean()), float(by[b].mean()))))
prev = way[-1][1]
for st in np.unique(stamps_all):
    if st <= 27:
        continue
    m = stamps_all == st
    sx, sy = xs[m].astype(float), ys[m].astype(float)
    if prev is not None:
        near = np.hypot(sx - prev[0], sy - prev[1]) < 75
        if near.any():
            sx, sy = sx[near], sy[near]
    c = (float(sx.mean()), float(sy.mean()))
    way.append((st / 12.0, c))
    prev = c

# snap each waypoint to the skeleton
KD = P_ARR
def snap(c):
    return pix[int(((KD - c) ** 2).sum(1).argmin())]

def route(a, b):
    """Shortest path a->b along the (bridged) skeleton."""
    if a == b:
        return [a]
    par = {a: None}
    q = deque([a])
    while q:
        u = q.popleft()
        for v in adj[u]:
            if v not in par:
                par[v] = u
                if v == b:
                    path = [b]
                    while path[-1] != a:
                        path.append(par[path[-1]])
                    return path[::-1]
                q.append(v)
    return None

# The gesture code, read off the footage (pen position + lifts per
# word): spans of wordclock entries.
GESTURES = [
    (0, 5, "the loner line"),    # sitting curled up in myself i
    (6, 6, "inner legs"),        # think
    (7, 9, "knee to base"),      # of a stone
    (10, 12, "arm wrap"),        # just waiting very
    (13, 13, "hair line one"),   # pa
    (14, 14, "hair line two"),   # tient
    (15, 15, "hair line three"), # ly
    (16, 16, "cross strand"),    # for
    (17, 17, "cross strands"),   # time
    (18, 18, "last strands"),    # to
    (19, 19, "the eyes"),        # pass
]

strokes = []
for k0, k1, name in GESTURES:
    v0 = clock[k0]["v0"] if k0 else 0.0
    v1 = clock[k1]["v1"]
    pts = [snap(c) for t, c in way if v0 - 0.04 <= t < v1 + 0.04]
    subs, cur = [], []
    for i, p in enumerate(pts):
        if not cur:
            cur = [p]
            continue
        if p == cur[-1]:
            continue
        leg = route(cur[-1], p)
        euclid = np.hypot(p[0] - cur[-1][0], p[1] - cur[-1][1])
        if leg is None or len(leg) > 5 * euclid + 40:
            subs.append(cur)      # pen lift
            cur = [p]
        else:
            cur += leg[1:]
    if cur:
        subs.append(cur)
    out = []
    for sub in subs:
        P = np.array(sub, float)
        if len(P) >= 7:
            P[3:-3] = sum(P[i:len(P) - 6 + i] for i in range(7)) / 7
        d = np.concatenate([[0],
                            np.cumsum(np.hypot(*np.diff(P, axis=0).T))])
        if d[-1] > 5:
            u = np.arange(0, d[-1] + 2.5, 5.0)
            P = np.stack([np.interp(u, d, P[:, 0]),
                          np.interp(u, d, P[:, 1])], 1)
        out.append([[round(float(x), 1), round(float(y), 1)]
                    for x, y in P])
    if not out:
        continue
    wordspan = " ".join(e["word"] for e in clock[k0:k1 + 1])
    strokes.append({
        "name": name, "word": wordspan,
        "v0": clock[k0]["v0"], "v1": v1, "sub": out,
    })
    print(f'{name:16s} [{wordspan:28s}] v {clock[k0]["v0"]:5.2f}-{v1:5.2f}'
          f'  {len(out)} line, {sum(len(s) for s in out):3d} pts')

json.dump({"w": W, "h": H, "strokes": strokes},
          open(f"{LONER}/viz/wg-strokes.json", "w"))
print(f"-> viz/wg-strokes.json ({len(strokes)} strokes)")
