#!/usr/bin/env python
# relight4.py (relight3 with a plain start and a 30 s climb) — the YC room bumping to the deep-house record, after pop/sailor-song/bin/perf-relight.mjs.
#
# Taken from perf-relight.mjs (same shapes, same constants unless noted):
#   · env(): every kick an attack-less decay (dec to 1/e, a short hold), summed — the lamp bloom is
#     min(0.5, env(kicks, .08, .03)), the radial halo BEHIND HIS HEAD is min(1, env(kicks, .12, .02)) × (.04 + .26·dance),
#     room only, R0 = (.26 + .08·halo)·W, falloff (1 − d²/R²)², its colour the chord hue ×.9 + 25.
#   · the ceiling lamp's two glows: radius 1.6r at .18·bloom and 2.4× that at .04·bloom.
#   · ENERGY per section → ambient, tint and hue eased toward the chord's with a 0.7 s glide, DANCE stepping with 2 s ramps.
#   · roomHue: a new colour each beat, a soft pulse on the beat (the 'long' moments there; the finale here).
# Changed for this room: ambient = .55 + .35·energy (sailor: .72 + .28; this is a club, it may go darker).
# New here: the walls and ceiling carry projected visuals, drawn in each plane's own metres through a homography so
# they sit on the wall in perspective: rings out of the back wall that a kick sends outward, light bars sweeping the
# right wall, a turning gobo on the ceiling. He is composited back on top from vision-matte, untouched but for a
# little of the room's colour.
#
#   pop/.venv/bin/python -I relight2.py in.mp4 matte.mp4 out.mp4 [--jobs 5] [--only 5,30,40,70 --png DIR]
#   (internal) --part a:b out.mp4   render frames [a, b)
import os, subprocess, sys, numpy as np
from scipy import ndimage

W, H, FPS = 1280, 720, 30
w, h = W // 2, H // 2                                                      # the light fields live at half res
BPM = 120; BEAT = 60 / BPM; BAR = 4 * BEAT; ENDBAR = 49
A = sys.argv[1:]
inp, matp, outp = A[0], A[1], A[2]
opt = lambda k, d=None: A[A.index(f"--{k}") + 1] if f"--{k}" in A else d
NF = int(subprocess.run(["ffprobe", "-v", "error", "-select_streams", "v", "-show_entries", "stream=nb_frames", "-of", "csv=p=0", inp], capture_output=True, text=True).stdout)

def sec(b): return "in" if b < 4 else "A" if b < 16 else "air" if b < 20 else "B" if b < 34 else "air2" if b < 38 else "C" if b < ENDBAR else "end"
ENERGY = {"in": 0.3, "A": 0.62, "air": 0.1, "B": 0.85, "air2": 0.12, "C": 1.0, "end": 0.04}
DANCE = {"in": 0.15, "A": 0.5, "air": 0.25, "B": 0.85, "air2": 0.3, "C": 1.0, "end": 0.2}
# chords one a bar: Bm9 · Gmaj9 · Dmaj9 · A9sus — the room tint (multiplied) and the light hue (added)
TINT = [np.array(c) for c in ([0.80, 0.74, 1.00], [0.70, 0.95, 1.00], [1.00, 0.84, 0.70], [1.00, 0.72, 0.92])]
HUE = [np.array(c, float) for c in ([150, 90, 255], [40, 200, 230], [255, 150, 60], [255, 70, 190])]
KICKS = [b * BAR + q * BEAT for b in range(ENDBAR) if sec(b) not in ("air", "air2") for q in range(4)] + [ENDBAR * BAR]
KICKS = np.array(KICKS); DOWN = np.array([abs((k / BAR) - round(k / BAR)) < 1e-6 for k in KICKS])

def env(t, dec, hold, gain=None):                                           # perf-relight.mjs env(): attack-less decays, summed
    dt = t - KICKS; ok = (dt >= 0) & (dt < hold + dec * 5); g = np.where(DOWN, 1.0, 0.75)[ok]   # downbeats bigger (sailor: "bigger on downbeats")
    d = dt[ok]; return float(np.sum(g * np.where(d < hold, 1.0, np.exp(-(d - hold) / dec))))
def ramp(table, t):                                                         # section value with a 2 s ramp from the last one
    b = int(t // BAR); cur = table[sec(b)]; start = b
    while start > 0 and sec(start - 1) == sec(b): start -= 1
    prev = table[sec(start - 1)] if start > 0 else cur
    k = min(1, (t - start * BAR) / 2.0); return prev + (cur - prev) * k
def wheel(ph):                                                              # perf-relight.mjs wheel()
    hh = ph % 1; i6 = int(hh * 6); fr = hh * 6 - i6; Q, T, lo = 255 * (1 - 0.9 * fr), 255 * (1 - 0.9 * (1 - fr)), 25
    return np.array([[255, T, lo], [Q, 255, lo], [lo, 255, T], [lo, Q, 255], [T, lo, 255], [255, lo, Q]][i6], float)

# per-frame state, simulated from frame 0 so any chunk can start anywhere (the glides are EMAs)
STATE = []; tint = TINT[0].copy(); hue = HUE[0].copy(); amb = ENERGY["in"]; party = 0.0; a = min(1, (1 / FPS) / 0.7)
for f in range(NF):
    t = f / FPS; b = int(t // BAR); c = min(b, ENDBAR) % 4 if b < ENDBAR else 2
    tint += (TINT[c] - tint) * a; hue += (HUE[c] - hue) * a; amb += (ramp(ENERGY, t) - amb) * min(1, (1 / FPS) / 0.5)
    if b >= ENDBAR: amb = ENERGY["end"] + (ENERGY["C"] - ENERGY["end"]) * np.exp(-(t - ENDBAR * BAR) / 1.5)
    # PARTY: how much of the club look is on. Plain at 0 s, a slow climb to full by 30 s (eased in, so the
    # first bars stay nearly natural), held there; the breakdowns calm to a third; the closing chord swells it
    # past full, then it fades.
    u = min(1, t / 30); goal = u * u * (3 - 2 * u)
    if sec(min(b, ENDBAR)) in ("air", "air2"): goal *= 0.32
    if b >= ENDBAR: goal = 1.35 * np.exp(-(t - ENDBAR * BAR) / 2.2)
    party += (goal - party) * min(1, (1 / FPS) / 0.6)
    STATE.append((tint.copy(), hue.copy(), amb, ramp(DANCE, t), party))

# ── the room: planes (image px at full res → the plane's own metres), lamp, masks ─────────────────────────────
def homog(src, dst):                                                        # DLT: H maps src (x,y) → dst
    M = []
    for (x, y), (u, v) in zip(src, dst): M += [[x, y, 1, 0, 0, 0, -u * x, -u * y, -u], [0, 0, 0, x, y, 1, -v * x, -v * y, -v]]
    _, _, V = np.linalg.svd(np.array(M, float)); return V[-1].reshape(3, 3)
yy, xx = np.mgrid[0:h, 0:w].astype(np.float32) * 2 + 0.5                    # half-res grid in full-res px
def plane(img_quad, world_quad):
    Hm = homog(img_quad, world_quad); den = Hm[2, 0] * xx + Hm[2, 1] * yy + Hm[2, 2]
    return (Hm[0, 0] * xx + Hm[0, 1] * yy + Hm[0, 2]) / den, (Hm[1, 0] * xx + Hm[1, 1] * yy + Hm[1, 2]) / den
# measured on the frame: the back wall's ceiling seam runs (400,0)→(905,200), the corner drops from (905,200) to the
# floor below frame, the right wall's seam (905,200)→(1280,30); walls 2.6 m tall, the back wall ~3.4 m wide
BU, BV = plane([(0, -165), (905, 200), (915, 660), (0, 905)], [(0, 0), (3.4, 0), (3.4, 2.6), (0, 2.6)])
RU, RV = plane([(905, 200), (1280, 32), (1280, 815), (915, 660)], [(0, 0), (1.9, 0), (1.9, 2.6), (0, 2.6)])
CU, CV = plane([(0, -165), (905, 200), (1280, 32), (760, -420)], [(0, 0), (3.4, 0), (3.4, 2.0), (0, 2.0)])
def side_of(p, q):                                                          # >0 left of the line p→q
    return (q[0] - p[0]) * (yy - p[1]) - (q[1] - p[1]) * (xx - p[0])
below_back = side_of((0, -165), (905, 200)) > 0                              # under the back seam
below_right = side_of((905, 200), (1280, 32)) > 0
left_of_corner = xx < 905 + (yy - 200) * (10 / 460)
soft = lambda m: ndimage.gaussian_filter(m.astype(np.float32), 2.0)
M_BACK = soft(below_back & left_of_corner); M_RIGHT = soft(below_right & ~left_of_corner); M_CEIL = soft(~(below_back & left_of_corner) & ~(below_right & ~left_of_corner))
LAMP = (890, 14, 70)                                                         # the ceiling fixture: x, y, r (full px)
D_LAMP = np.exp(-((xx - LAMP[0]) ** 2 + (yy - LAMP[1]) ** 2) / (2 * 300 ** 2))[..., None]
# depth falloff for the projection: brighter near the corner and lamp, fading toward the camera-side edges
FALL = (0.55 + 0.45 * np.exp(-((xx - 760) ** 2 / (2 * 520 ** 2) + (yy - 260) ** 2 / (2 * 420 ** 2))))[..., None]

def glowfield(cx, cy, R0, k):                                               # perf-relight.mjs glow(): a soft disc, added
    d2 = ((xx - cx) ** 2 + (yy - cy) ** 2) / (R0 * R0); return (np.clip(1 - d2, 0, 1) ** 2 * k)[..., None]

def projection(t, hue, amb, dance, kick, face, party):
    """the projected visuals, per plane, in the plane's own metres. Returns an RGB add field (0..1)."""
    b = int(t // BAR); s = sec(min(b, ENDBAR)); calm = s in ("air", "air2", "in")
    # the look climbs with the party ramp: soft rings → tighter rings by 15 s → the full per-beat finale look from 30 s
    var = 0 if calm and t >= 4 else 3 if t >= 30 or s in ("C", "end") else 2 if t >= 15 else 1
    lvl = (0.25 if calm else 0.55 + 0.45 * amb) * (0.6 + 0.4 * dance) * party
    # back wall: rings out of a point behind his left shoulder, a kick sends a bright ring outward
    # centred on his head (looked up on the back-wall plane), so they ring out around him inside the 9:16 crop
    hx = int(np.clip(face[0] / 2, 0, w - 1)); hy = int(np.clip((face[1] - 0.06 * H) / 2, 0, h - 1))
    cx, cy = float(BU[hy, hx]), float(BV[hy, hx]); r = np.hypot(BU - cx, BV - cy)
    drift = t * (0.08 if calm else 0.22)
    rings = (0.5 + 0.5 * np.cos(2 * np.pi * (r / (0.42 if var != 2 else 0.3) - drift))) ** (8 if var == 3 else 5)
    ring_kick = np.zeros_like(r)
    for k in KICKS[(KICKS <= t) & (KICKS > t - 1.2)] if not calm else []:
        rad = (t - k) * 2.2; ring_kick += np.exp(-((r - rad) / 0.07) ** 2) * np.exp(-(t - k) / 0.45)
    back = (rings * 0.55 + ring_kick * 1.1) * np.clip(1 - r / 2.4, 0, 1)
    # the light bars come round onto the back wall too, a band either side of him (the right wall is outside the crop)
    near = np.exp(-((BU - cx) / 0.95) ** 2) * np.clip(1 - np.abs(BV - cy) / 1.8, 0, 1)
    bars_b = (0.5 + 0.5 * np.cos(2 * np.pi * (BU / 0.26 + t * (0.05 if calm else 0.3)))) ** 8 * near * (0.35 + 0.65 * kick)
    # right wall: vertical bars sweeping across, thinner and faster as the sections lift
    bars = (0.5 + 0.5 * np.cos(2 * np.pi * (RU / (0.32 - 0.06 * min(var, 3) / 3) - t * (0.06 if calm else 0.35)))) ** 6
    right = bars * (0.5 + 0.5 * kick)
    # ceiling: a turning gobo of dots
    ang = t * (0.05 if calm else 0.18); cu, cv = CU - 1.7, CV - 0.6
    ru, rv = cu * np.cos(ang) - cv * np.sin(ang), cu * np.sin(ang) + cv * np.cos(ang)
    dots = ((0.5 + 0.5 * np.cos(2 * np.pi * ru / 0.28)) * (0.5 + 0.5 * np.cos(2 * np.pi * rv / 0.28))) ** 10
    ceil = dots * (0.4 + 0.6 * kick)
    # colour: the chord's hue; in the finale the beat steps the hue (perf-relight roomHue)
    beat = int(t // BEAT); f = (t % BEAT) / BEAT
    c_back = (wheel((beat % 12) / 12) if var == 3 else hue) / 255
    c_right = (wheel(((beat + 4) % 12) / 12) if var == 3 else hue[::-1] * 0.6 + np.array([40, 200, 230]) * 0.4) / 255
    c_ceil = np.array([0.75, 0.85, 1.0])
    pulse = 0.75 + 0.25 * (1 - f) ** 2 if var == 3 else 1
    return ((back * M_BACK)[..., None] * c_back + (bars_b * 0.55 * M_BACK)[..., None] * c_right + (right * M_RIGHT)[..., None] * c_right + (ceil * M_CEIL)[..., None] * c_ceil) * FALL * lvl * pulse

def up(a):                                                                  # half → full res
    return a.repeat(2, axis=0).repeat(2, axis=1)
def blur_q(img):                                                            # a wide, cheap bloom at 1/8 res
    s = img.reshape(H // 8, 8, W // 8, 8, 3).mean(axis=(1, 3))
    s = ndimage.gaussian_filter(s, (3, 3, 0)); return s.repeat(8, axis=0).repeat(8, axis=1)

FACE = [W * 0.46, H * 0.36]
def frame(img, m, fi):
    t = fi / FPS; tint, hue, amb, dance, party = STATE[fi]
    bloom = min(0.5, env(t, 0.08, 0.03)) * (0.3 + 0.7 * min(1, party)); halo = min(1, env(t, 0.12, 0.02)) * (0.04 + 0.26 * dance) * party
    kick = min(1, env(t, 0.18, 0.02))
    mh = m[::2, ::2]
    # his head: top of the matte, eased (perf-relight findFace, simply)
    rows = np.where(mh.max(axis=1) > 0.5)[0]
    if len(rows):
        top = rows[0] * 2; cols = np.where(mh[rows[0] + 20 if rows[0] + 20 < h else rows[0]] > 0.5)[0]
        if len(cols): FACE[0] += ((cols.mean() * 2) - FACE[0]) * 0.15; FACE[1] += ((top + 130) - FACE[1]) * 0.15
    room = 1 - min(1, party) * (1 - (0.42 + 0.33 * amb))                                                 # ambient (sailor: .72 + .28·energy; a club goes darker)
    T = 1 - min(1, party) * (1 - (0.3 + 0.7 * tint))                                                     # the chord tint, stronger than sailor's half
    lamp_on = (0.12 * amb + 0.2 * bloom) * min(1, party)
    gain = (room + lamp_on * D_LAMP) * T
    add = glowfield(LAMP[0], LAMP[1], LAMP[2] * 1.6, 0.18 * bloom) * np.array([1.0, 0.89, 0.7]) \
        + glowfield(LAMP[0], LAMP[1], LAMP[2] * 3.8, 0.04 * bloom) * np.array([1.0, 0.86, 0.67])
    if halo > 0.01:                                                          # the radial halo behind his head, room only
        add = add + glowfield(FACE[0], FACE[1] - 0.06 * H, (0.26 + 0.08 * halo) * W, halo) * ((hue * 0.9 + 25) / 255)
    lum = img[::2, ::2].mean(axis=2, keepdims=True)
    add = add + projection(t, hue, amb, dance, kick, FACE, party) * (0.35 + 0.9 * lum)   # projected light shows more on a bright wall
    add = add + lum * (hue / 255) * (0.18 + 0.22 * amb + 0.25 * kick * dance) * min(1.2, party) \
        + glowfield(LAMP[0], LAMP[1], LAMP[2] * 5, 0.05 * (1 - min(1, party))) * np.array([1.0, 0.8, 0.55])   # the hue reflected off the walls, bumping with the kick
    G, Ad = up(gain), up(add)
    mm = m[..., None]
    fgt = 0.85 + 0.15 * T                                                    # him: nearly natural, a little of the room's colour
    out = img * (G * (1 - mm) + fgt * (0.94 + 0.06 * room) * mm) + Ad * (1 - mm)
    hi = np.clip(out - 0.7, 0, 1)                                            # halation over the highlights
    out = out + blur_q(hi) * (0.5 + 0.3 * bloom)
    return np.clip(out, 0, 1)

def reader(path, fmt, ch, start):
    p = subprocess.Popen(["ffmpeg", "-v", "error", "-ss", f"{start / FPS:.6f}", "-i", path, "-f", "rawvideo", "-pix_fmt", fmt, "-"], stdout=subprocess.PIPE)
    n = W * H * ch
    def nxt():
        bb = p.stdout.read(n); return None if len(bb) < n else np.frombuffer(bb, np.uint8).reshape(H, W, ch)
    return nxt, p

if "--only" in A:
    os.makedirs(opt("png"), exist_ok=True)
    for s in [float(x) for x in opt("only").split(",")]:
        fi = int(round(s * FPS)); rv, p1 = reader(inp, "rgb24", 3, fi); rm, p2 = reader(matp, "gray", 1, fi)
        o = frame(rv().astype(np.float32) / 255, rm()[..., 0].astype(np.float32) / 255, fi); p1.kill(); p2.kill()
        subprocess.run(["ffmpeg", "-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-i", "-", f"{opt('png')}/v2-{s:g}.png"], input=(o * 255).astype(np.uint8).tobytes())
    sys.exit(0)

if "--part" in A:
    a0, b0 = map(int, opt("part").split(":")); dst = A[A.index("--part") + 2]
    rv, _ = reader(inp, "rgb24", 3, a0); rm, _ = reader(matp, "gray", 1, a0)
    enc = subprocess.Popen(["ffmpeg", "-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
                            "-c:v", "libx264", "-crf", "15", "-preset", "medium", "-pix_fmt", "yuv420p", dst], stdin=subprocess.PIPE)
    for fi in range(a0, b0):
        f = rv(); m = rm()
        if f is None: break
        o = frame(f.astype(np.float32) / 255, (m[..., 0] if m is not None else np.zeros((H, W), np.uint8)).astype(np.float32) / 255, fi)
        enc.stdin.write((o * 255 + 0.5).astype(np.uint8).tobytes())
        if (fi - a0) % 150 == 0: print(f"  part {a0}: {fi - a0}/{b0 - a0}", flush=True)
    enc.stdin.close(); enc.wait(); sys.exit(0)

# the whole film, in parallel chunks, then joined and given the original audio
J = int(opt("jobs", 5)); cuts = [round(NF * i / J) for i in range(J + 1)]; tmp = outp + ".parts"; os.makedirs(tmp, exist_ok=True)
procs = [subprocess.Popen([sys.executable, "-I", __file__, inp, matp, outp, "--part", f"{cuts[i]}:{cuts[i + 1]}", f"{tmp}/p{i}.mp4"]) for i in range(J)]
codes = [p.wait() for p in procs]
if any(codes): sys.exit(f"✗ a part failed: {codes}")
open(f"{tmp}/list.txt", "w").write("".join(f"file 'p{i}.mp4'\n" for i in range(J)))
subprocess.run(["ffmpeg", "-v", "error", "-y", "-f", "concat", "-safe", "0", "-i", f"{tmp}/list.txt", "-i", inp, "-map", "0:v", "-map", "1:a?", "-c", "copy", outp], check=True)
n = subprocess.run(["ffprobe", "-v", "error", "-select_streams", "v", "-count_frames", "-show_entries", "stream=nb_read_frames", "-of", "csv=p=0", outp], capture_output=True, text=True).stdout.strip()
print(f"✓ {outp} · {n}/{NF} frames")
