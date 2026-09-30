#!/usr/bin/env python3
# measures.py — the guitar's own clock, strum by strum, so the drums hit
# where her hand hits.
#
#   1. STRUMS    onsets on the guitar stem (backtracked to the attack),
#                strength, and a down/up guess (downstrokes hit the low
#                strings first → lower spectral centroid in the first 30 ms).
#   2. BEATS     take.analysis.json's beat track, each beat pulled onto the
#                nearest strong strum within ±70 ms. A beat with no strum
#                nearby is interpolated between its neighbours, never left
#                where the tracker guessed.
#   3. GRID      where strums land inside a beat (phase histogram) → what
#                subdivision she plays (8ths vs 16ths) — and so which pulse
#                is the quarter note.
#   4. CHORDS    per beat, template-matched against the song's real
#                vocabulary only. The tabs agree (capo 4: Cmaj7 Em G G →
#                Emaj7 G#m B B), so the choice is between three chords.
#   5. BARS      the bar length + phase that puts the most chord changes
#                on downbeats; each bar gets its chord and strum pattern.
#
#   pop/.venv/bin/python pop/sailor-song/bin/measures.py

import json, os
import numpy as np
import librosa

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
GUITAR = os.path.join(os.path.dirname(LANE), "samples/sailor-song-take/stems/htdemucs/guitar.wav")
A = json.load(open(os.path.join(LANE, "take.analysis.json")))
SR, HOP = 22050, 256

y, _ = librosa.load(GUITAR, sr=SR, mono=True)
tuning = A["tuningCents"] / 100.0

# ── 1. strums ──────────────────────────────────────────────────────────
env = librosa.onset.onset_strength(y=y, sr=SR, hop_length=HOP, aggregate=np.median)
on = librosa.onset.onset_detect(onset_envelope=env, sr=SR, hop_length=HOP,
                                backtrack=True, units="frames", delta=0.06)
on_t = librosa.frames_to_time(on, sr=SR, hop_length=HOP)
peak = np.array([env[f:f + 4].max() if f < len(env) else 0 for f in on])
peak = peak / (np.percentile(peak, 95) + 1e-9)
cent = []
for t in on_t:
    a = int(t * SR); seg = y[a:a + int(0.03 * SR)]
    cent.append(float(librosa.feature.spectral_centroid(y=seg, sr=SR, n_fft=512).mean()) if len(seg) > 512 else 0)
cent = np.array(cent)
med_c = np.median(cent[cent > 0])
strums = [{"t": round(float(t), 3), "amp": round(float(p), 2),
           "dir": "D" if c <= med_c else "U"} for t, p, c in zip(on_t, peak, cent)]
strong = on_t[peak > 0.35]

# ── 2. beats snapped to strums ─────────────────────────────────────────
# The tracker's beats sit on her strums but run ~35 ms early; pull each
# onto the nearest strong strum.
raw = np.array(A["pulse"]["beats"])
snapped, hit = raw.copy(), np.zeros(len(raw), bool)
for i, b in enumerate(raw):
    if len(strong):
        j = np.argmin(np.abs(strong - b))
        if abs(strong[j] - b) <= 0.07:
            snapped[i] = strong[j]; hit[i] = True
for i in range(len(raw)):                       # fill misses by interpolation
    if not hit[i]:
        lo = max([k for k in range(i) if hit[k]], default=None)
        hi = min([k for k in range(i + 1, len(raw)) if hit[k]], default=None)
        if lo is not None and hi is not None:
            snapped[i] = snapped[lo] + (snapped[hi] - snapped[lo]) * (i - lo) / (hi - lo)
snapped = np.maximum.accumulate(snapped)
offs = (snapped - raw)[hit] * 1000

# ── 3. subdivision ─────────────────────────────────────────────────────
phases = []
for t in on_t:
    k = np.searchsorted(snapped, t) - 1
    if 0 <= k < len(snapped) - 1:
        phases.append((t - snapped[k]) / (snapped[k + 1] - snapped[k]))
phases = np.array(phases)
hist16 = np.histogram(phases, bins=8, range=(-1 / 16, 1 - 1 / 16))[0]   # 32nd-centred 8 bins
per_beat = len(on_t) / max(1, len(snapped))

# ── 4. chords per beat (real vocabulary only) ──────────────────────────
N = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
VOCAB = {"Emaj7": [4, 8, 11, 3], "G#m": [8, 11, 3], "B": [11, 3, 6]}
yh = librosa.effects.harmonic(y, margin=4)
chroma = librosa.feature.chroma_cqt(y=yh, sr=SR, hop_length=512, tuning=tuning, bins_per_octave=36)
bass = librosa.feature.chroma_cqt(y=yh, sr=SR, hop_length=512, tuning=tuning, fmin=librosa.note_to_hz("E2"), n_octaves=2, bins_per_octave=36)
fr = librosa.time_to_frames(snapped, sr=SR, hop_length=512)
C = librosa.util.sync(chroma, fr, aggregate=np.median)[:, 1:]    # column k = beat k → k+1
B = librosa.util.sync(bass, fr, aggregate=np.median)[:, 1:]
labels = []
for k in range(C.shape[1]):
    c = C[:, k] / (np.linalg.norm(C[:, k]) + 1e-9)
    b = B[:, k] / (B[:, k].max() + 1e-9)
    best = None
    for name, pcs in VOCAB.items():
        tpl = np.zeros(12); tpl[pcs] = 1; tpl /= np.linalg.norm(tpl)
        root = {"Emaj7": 4, "G#m": 8, "B": 11}[name]
        s = float(c @ tpl) + 0.35 * float(b[root])      # the bass note decides G#m vs B
        if best is None or s > best[0]:
            best = (s, name)
    labels.append(best[1])
labels = labels[:len(snapped) - 1]

# ── 5. bars, anchored on her strum motif ───────────────────────────────
# Her hand plays a 2-beat motif X..X (8ths): the beat, then the "and" of
# the next beat. The tracker gains/drops a beat in places (measured: the
# motif reads .XX. through ~29–60 s), so bars are NOT every 4 tracked
# beats — they start where the motif starts. Motif-start score for beat k:
#   h(beat k) + h(and of k+1) − h(and of k) − h(beat k+1)
def h(t):
    d = np.abs(on_t - t)
    j = np.argmin(d)
    return float(peak[j]) if d[j] < 0.06 else 0.0


nb = len(snapped)
mid = np.append((snapped[:-1] + snapped[1:]) / 2, snapped[-1])
sk = np.array([h(snapped[k]) + h(mid[k + 1]) - h(mid[k]) - h(snapped[k + 1])
               if k + 1 < nb - 1 else 0.0 for k in range(nb)])
# parity per 8-beat window, then a motif start every 2 beats of that parity
W = 8
par = []
for w0 in range(0, nb, W):
    e = sk[w0:w0 + W][0::2].sum() if (w0 % 2 == 0) else sk[w0:w0 + W][1::2].sum()
    o = sk[w0:w0 + W][1::2].sum() if (w0 % 2 == 0) else sk[w0:w0 + W][0::2].sum()
    par.append(0 if e >= o else 1)
starts = [k for k in range(nb) if k % 2 == par[k // W]]
starts = [k for i, k in enumerate(starts) if i == 0 or k - starts[i - 1] >= 2 or True]
dedup = []
for k in starts:
    if dedup and k - dedup[-1] < 2:
        continue
    dedup.append(k)
starts = dedup
flips = [round(float(snapped[starts[i]]), 2) for i in range(1, len(starts)) if starts[i] - starts[i - 1] != 2]

# downbeat = every other motif start; pick the parity that puts chord changes on it
chg = [k for k in range(1, len(labels)) if labels[k] != labels[k - 1]]
def near(k, pool): return min(abs(k - p) for p in pool) <= 1
best = None
for ph in (0, 1):
    downs = starts[ph::2]
    sc = sum(1 for c in chg if near(c, downs)) / max(1, len(chg))
    if best is None or sc > best[0]:
        best = (sc, ph)
L, PH, GRID_SHIFT = 4, best[1], f"motif-anchored, {len(flips)} beat-count flips at {flips}"
downs = starts[PH::2]
bars = []
for d0, d1 in zip(downs, downs[1:]):
    seg = labels[d0:d1] or [labels[min(d0, len(labels) - 1)]]
    chord = max(set(seg), key=seg.count)
    t0, t1 = snapped[d0], snapped[d1]
    nbeat = d1 - d0
    pat = ["."] * (2 * nbeat)
    for st in strums:
        if t0 - 0.03 <= st["t"] < t1 - 0.03 and st["amp"] > 0.2:
            k = int(round((st["t"] - t0) / ((t1 - t0) / (2 * nbeat))))
            if 0 <= k < 2 * nbeat:
                pat[k] = "X"
    bars.append({"n": len(bars) + 1, "t": round(float(t0), 3), "dur": round(float(t1 - t0), 3),
                 "beats": [round(float(x), 3) for x in snapped[d0:d1 + 1]],
                 "bpm": round(60 * nbeat / float(t1 - t0), 1), "chord": chord,
                 "agree": round(seg.count(chord) / len(seg), 2), "strum": "".join(pat)})

res = {
    "strums": {"count": len(strums), "perBeat": round(per_beat, 2),
               "phaseHist8": hist16.tolist()},
    "beats": {"snapped": [round(float(b), 3) for b in snapped],
              "hitFraction": round(float(hit.mean()), 3),
              "trackerOffsetMs": {"median": round(float(np.median(offs)), 1),
                                  "p90abs": round(float(np.percentile(np.abs(offs), 90)), 1)}},
    "meter": {"gridShift": GRID_SHIFT, "beatsPerBar": L, "phase": PH, "downbeatChangeFraction": round(best[0], 2)},
    "bars": bars,
    "strumList": strums,
}
json.dump(res, open(os.path.join(LANE, "measures.json"), "w"), indent=1)

print(f"strums {len(strums)}  ({per_beat:.2f} per beat)   phase hist (8 slots/beat) {hist16.tolist()}")
print(f"beats snapped to a strum: {hit.mean():.0%}   tracker was off by median "
      f"{np.median(offs):+.0f} ms, p90 |{np.percentile(np.abs(offs), 90):.0f}| ms")
print(f"grid shift: {GRID_SHIFT}   meter: {L} beats/bar, phase {PH}, {best[0]:.0%} of chord changes on downbeats")
print("bar  time    bpm   chord  agree strum(8ths)")
for b in bars:
    print(f"{b['n']:3d} {b['t']:7.2f} {b['bpm']:6.1f}  {b['chord']:6s} {b['agree']:.2f}  {b['strum']}")
