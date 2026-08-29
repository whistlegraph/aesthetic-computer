"""assemble-floor.py — the lonerclub extended floor mix (~5:57).

The record (assemble.py) is a 94.2 s arrangement of finalized, bar-locked
layers. This sequencer bakes each layer EXACTLY as the record does — same
place(), same envelopes, same breath gates — then copies bar-aligned windows
of the baked layers onto a longer timeline: a 48-bar DJ intro, the record
intact, a floorless break and a second drop, a vocal reprise, and a 32-bar
strip-down outro. Nothing is resynthesized; the floor mix is the record,
given room.

Memory: one layer is baked, sequenced onto the output, and freed before the
next — neo has 8 GB and this must never hold the whole stage at once.
"""
import numpy as np, subprocess, os

S = os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
sr = 48000
BEAT = 60.0 / 122; BAR = 4 * BEAT
GRID0 = 0.3654               # the record's first downbeat
SRC_DUR = 94.2
NT = int(SRC_DUR * sr)       # source timeline (the bake happens here)

def raw2(p): return np.fromfile(p, np.float32).reshape(-1, 2).astype(np.float64)
def wav2(p):
    r = subprocess.run(["ffmpeg", "-v", "error", "-i", p, "-ar", str(sr),
                        "-ac", "2", "-f", "f32le", "-"], capture_output=True).stdout
    return np.frombuffer(r, np.float32).reshape(-1, 2).astype(np.float64)
def envelope(pts):
    t = np.arange(NT) / sr
    return np.interp(t, [p[0] for p in pts], [p[1] for p in pts])[:, None]

def place(x, deg=0.0, depth=0.0):
    m = x.mean(axis=1)
    itd = int(abs(deg) / 40.0 * 0.0006 * sr)
    ild = 10 ** (-abs(deg) / 40.0 * 3.0 / 20.0)
    L = m.copy(); R = m.copy()
    if deg > 0:
        L = np.concatenate([np.zeros(itd), m[:-itd]]) if itd else m.copy()
        L *= ild
    elif deg < 0:
        R = np.concatenate([np.zeros(itd), m[:-itd]]) if itd else m.copy()
        R *= ild
    out = np.stack([L, R], 1)
    if depth > 0:
        a = 1 - np.exp(-2 * np.pi * (9000 - 6500 * depth) / sr)
        acc = np.zeros(2)
        y = np.empty_like(out)
        for i in range(len(out)):
            acc += a * (out[i] - acc); y[i] = acc
        out = y * (1 - 0.25 * depth)
    return out

P2, P3 = 31.83, 63.30
ENVS = {
 "st-kick":[(0,0.72),(29.9,0.77),(P2,0.84),(P3,0.84),(P3+2*BAR,0.90),(SRC_DUR,0.90)],
 "st-hats":[(0,0),(15.7,0),(19.7,0.50),(P2,0.55),(P3,0.55),(P3+2*BAR,0.72),(SRC_DUR,0.72)],
 "bass":[(0,0.70),(P2,0.75),(P3,0.75),(P3+2*BAR,0.95),(SRC_DUR,0.95)],
 "st-pluck":[(0,0.18),(15.7,0.18),(19.7,0.55),(P2,0.58),(P3,0.58),(P3+2*BAR,0.90),(SRC_DUR,0.90)],
 "st-pads":[(0,0.22),(P2-2*BAR,0.25),(P2,0.78),(P3,0.78),(P3+2*BAR,0.92),(SRC_DUR,0.92)],
 "st-bells":[(0,0),(P3-0.05,0),(P3+2*BAR,0.52),(SRC_DUR,0.52)],
}
def gate(windows, floor=0.0, ramp_s=0.05):
    g = np.ones(NT)
    ramp = int(ramp_s * sr)
    for (t0, t1, depth) in windows:
        lo = max(0, int(t0 * sr)); hi = min(NT, int(t1 * sr))
        if hi <= lo: continue
        seg = np.full(hi - lo, depth)
        r = min(ramp, (hi - lo) // 2)
        if r:
            seg[:r] = np.linspace(1, depth, r); seg[-r:] = np.linspace(depth, 1, r)
        g[lo:hi] = np.minimum(g[lo:hi], seg)
    return g[:, None]

RESTS = []
t8 = GRID0 + 8 * BAR; k = 0
while t8 < 93.0:
    RESTS.append((t8 - BAR, t8 - BAR / 2, 0.34 if k % 2 == 0 else 0.52))
    t8 += 8 * BAR; k += 1
BREATH = gate(RESTS)
THIN_VERSE = gate([(P2, P2 + 4 * BAR, 0.0)])
THIN_PADS  = gate([(P3 + 4 * BAR, P3 + 6 * BAR, 0.12)])
THIN_TOP   = gate([(P2, P2 + 4 * BAR, 0.45)])
VOXENV = [(0, 0.92), (SRC_DUR, 0.92)]

def fit(x):
    """Pad or trim to the source timeline — the record's add() tolerated
    short stems, so the bake must too."""
    if len(x) == NT: return x
    out = np.zeros((NT, 2))
    n = min(len(x), NT)
    out[:n] = x[:n]
    return out

# each layer: a thunk that bakes it to its finalized, record-identical form
BAKE = {
 "kick":  lambda: fit(place(raw2(f"{S}/st-kick.raw"),0,0))*envelope(ENVS["st-kick"]),
 "hats":  lambda: fit(place(raw2(f"{S}/st-hats.raw"),+20,0.1))*envelope(ENVS["st-hats"])*BREATH*THIN_TOP,
 "bass":  lambda: fit(place(wav2(f"{S}/sep4/htdemucs/v4pid-trim/bass.wav"),0,0))*envelope(ENVS["bass"]),
 "pluck": lambda: fit(place(raw2(f"{S}/st-pluck.raw"),-25,0.15))*envelope(ENVS["st-pluck"])*BREATH*THIN_TOP,
 "pads":  lambda: fit(place(raw2(f"{S}/st-pads.raw"),0,0.35))*envelope(ENVS["st-pads"])*BREATH*THIN_PADS*0.95,
 "bells": lambda: fit(place(raw2(f"{S}/st-bells.raw"),+30,0.25))*envelope(ENVS["st-bells"]),
 "vox":   lambda: fit(wav2(f"{S}/vocalsFX.wav"))*envelope(VOXENV)*1.02,
 "piano": lambda: fit(place(raw2(f"{S}/st-piano.raw"),-15,0.20))*BREATH*THIN_VERSE,
 "swing": lambda: fit(place(raw2(f"{S}/st-swing.raw"),+12,0.05))*BREATH*THIN_VERSE,
 "fills": lambda: fit(place(raw2(f"{S}/st-fills.raw"),-8,0.05)),
 "wub":   lambda: fit(raw2(f"{S}/stem-wub.raw")),
 "gongs": lambda: fit(place(raw2(f"{S}/stem-gongs.raw"),0,0.5)),
 "stamp": lambda: fit(raw2(f"{S}/stem-stamp.raw")),
}
ALL = set(BAKE)

# The floor: (dst_bar, src_bar, nbars, layers). src windows start on the
# record's own 8-bar phrase boundaries so every turnaround fill stays where
# the record put it. src bars: body 0–16 · clock 16–32 · club 32–46 clean.
ROWS = [
 (0,   32, 8,  {"kick"}),
 (8,   32, 8,  {"kick","hats","bass"}),
 (16,  32, 8,  {"kick","hats","bass"}),
 (24,  32, 8,  {"kick","hats","bass","pluck","pads","wub"}),
 (32,  32, 8,  {"kick","hats","bass","pluck","pads","wub"}),
 (40,  0,  16, ALL),                               # the record: body
 (56,  16, 16, ALL),                               # clock — tag at its end
 (72,  32, 14, ALL),                               # first drop
 (86,  16, 8,  {"vox","pads","bells"}),            # floorless break
 (94,  16, 8,  {"vox","pads","bells","pluck","piano"}),
 (102, 24, 8,  {"vox","pads","bells","pluck","piano","hats","swing","stamp"}),
 (110, 32, 14, ALL),                               # second drop
 (124, 32, 8,  ALL),
 (132, 16, 16, ALL),                               # vocal hook reprise
 (148, 32, 8,  {"kick","bass","pluck","pads"}),    # the strip-down
 (156, 32, 8,  {"kick","bass","pluck","pads"}),
 (164, 32, 8,  {"kick","hats"}),
 (172, 32, 8,  {"kick"}),
]
TAIL_DST_BAR = 180
TAIL_SRC_T = GRID0 + 46 * BAR                      # the record's own ring-out
TAIL_LAYERS = {"pads", "bells", "gongs"}

def src_t(bar): return GRID0 + bar * BAR
def dst_t(bar): return bar * BAR

OUT_DUR = dst_t(TAIL_DST_BAR) + (SRC_DUR - TAIL_SRC_T)
NO = int(OUT_DUR * sr) + sr                        # +1 s of silence to breathe
mix = np.zeros((NO, 2), np.float32)

FADE_IN = int(0.005 * sr)                          # keeps the downbeat's attack
FADE_OUT = int(0.040 * sr)                         # outgoing rings past the bar

def copy(layer_buf, s0, d0, n):
    """Overlap-add n samples of the baked layer from src s0 to dst d0."""
    n = min(n, len(layer_buf) - s0, NO - d0)
    if n <= 0: return
    seg = layer_buf[s0:s0 + n].astype(np.float32).copy()
    f = min(FADE_IN, n)
    seg[:f] *= np.linspace(0, 1, f)[:, None]
    mix[d0:d0 + n] += seg
    # the tail past the window, faded — masks the cut without touching the beat
    e = min(FADE_OUT, len(layer_buf) - (s0 + n), NO - (d0 + n))
    if e > 0:
        tail = layer_buf[s0 + n:s0 + n + e].astype(np.float32) \
             * np.linspace(1, 0, e)[:, None]
        mix[d0 + n:d0 + n + e] += tail

for name in sorted(ALL):
    buf = BAKE[name]()
    for (db, sb, nbars, layers) in ROWS:
        if name not in layers: continue
        copy(buf, int(src_t(sb) * sr), int(dst_t(db) * sr), int(nbars * BAR * sr))
    if name in TAIL_LAYERS:
        s0 = int(TAIL_SRC_T * sr)
        copy(buf, s0, int(dst_t(TAIL_DST_BAR) * sr), len(buf) - s0)
    del buf
    print(f"  {name} sequenced")

# a settle fade over the very end, in case a layer still rings
end = np.arange(NO) / sr
mix *= np.clip((OUT_DUR + 0.6 - end) / 1.5, 0, 1)[:, None].astype(np.float32)

pk = float(np.abs(mix).max())
if pk > 0.85: mix *= 0.85 / pk
print(f"floor premaster peak {pk:.3f} -> normalized to 0.85, {OUT_DUR:.1f}s")
mix.astype(np.float32).tofile(f"{S}/premaster-floor.raw")
print("floor assembled")
