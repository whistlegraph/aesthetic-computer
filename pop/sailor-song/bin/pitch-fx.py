#!/usr/bin/env python3
"""pitch-fx.py — play with her voice's octave (v33): WORLD resynthesis of a window of her lead with a
controlled f0 curve, her own spectral envelope and aperiodicity untouched. Two shapes:

  slide   f0 holds for the first `hold` of the window, then glides by `semis` (−12 = an octave down)
          over the rest — "slide it for effect in 'loooooong'". The engine crossfades it IN over her lead.
  ghost   the whole window transposed by `semis`, a double the engine lays UNDER her lead (the opening).

Reads src/vox/cut/vocals-natural.wav (the stem clock). Writes src/vox/fx/<name>.wav and src/vox/fx/fx.json
({name, t0, t1, mode, semis}) for the engine.

  pop/.venv/bin/python pop/sailor-song/bin/pitch-fx.py
"""
import json, os, sys
import numpy as np, soundfile as sf, pyworld as pw

LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(LANE, "src/vox/cut/vocals-natural.wav")
OUT = os.path.join(LANE, "src/vox/fx"); os.makedirs(OUT, exist_ok=True)
FRAME_MS = 5.0

# the windows (stem seconds, from src/words-record.json + startSec): her two held "long"s, and her first line
np.random.seed(7)   # v115: deterministic drift — re-rendering one word must not move the others
FX = [
    # v36: the held "long"s RISE an octave and keep going — f0 cleaned, the vowel extended on her own frames, a curve for the engine
    # v37: QUANTIZED — after the hold the pitch climbs the scale one degree per 8th (seven to the octave), each step snapped flat
    {"name": "rise-long-1", "t0": 86.40, "t1": 90.95, "mode": "steps", "semis": 12, "hold": 0.30, "extend": 0, "step": 0.24},   # v42: ends before her "And" (91.03), no extension — it was pitching her "la"
    {"name": "rise-long-2", "t0": 132.68, "t1": 137.08, "mode": "steps", "semis": 12, "hold": 0.30, "extend": 7.0, "step": 0.24, "arp": True},   # v97: up, down to HER note, then held through the break to ~2 bars before the bridge line, ending on her own ng   # v87: then arpeggiates up and down, 8ths → 16ths → 32nds, fading into the break
    # v41: her last word, "out", held — the same pitch, the vowel extended 2.6 s, for the dissolve
    {"name": "hold-out", "t0": 159.40, "t1": 160.97, "mode": "hold", "semis": 0, "hold": 1.0, "extend": 2.6},
    # v55: "only the start of 'kiss'" — the word itself (60.675–60.84) placed where "you" ends (60.07, the strip's 27.4) and its vowel
    # held until "me" (60.84): extend = 0.77 − 0.165. The engine places it at `place` and replaces her lead from there to `orig`.
    {"name": "kiss-hold", "t0": 60.675, "t1": 60.84, "mode": "hold", "semis": 0, "hold": 1.0, "extend": 0.175, "place": 60.50},   # v115: "start kiss earlier but end it at the same time" — placed right after her k-, the i vowel stretched 0.175 s, ends where it ended   # v56: where "you" ends, held to "me"
]

x, fs = sf.read(SRC, dtype="float64", always_2d=True); x = x[:, 0]
manifest = []
for fx in FX:
    a, z = int(fx["t0"] * fs), int(fx["t1"] * fs)
    seg = np.ascontiguousarray(x[a:z])
    f0, t = pw.harvest(seg, fs, f0_floor=120.0, f0_ceil=900.0, frame_period=FRAME_MS)
    f0 = pw.stonemask(seg, f0, t, fs)
    sp = pw.cheaptrick(seg, f0, t, fs); ap = pw.d4c(seg, f0, t, fs)
    # clean f0: bridge unvoiced gaps by interpolation, median-smooth, so the glide has no octave errors
    v = f0 > 0
    if v.sum() > 2:
        idx = np.arange(len(f0)); f0c = np.interp(idx, idx[v], f0[v])
        from scipy.signal import medfilt; f0c = medfilt(f0c, 9)
    else: f0c = f0.copy()
    n = len(f0c); ratio = np.ones(n)
    SCALE = [8, 10, 11, 1, 3, 4, 6]                                     # G# natural minor pitch classes
    def scale_up(m, k):                                                  # k scale degrees above the scale tone at/under m
        tones = [x for x in range(int(m) - 24, int(m) + 36) if x % 12 in SCALE]; i = max(j for j, x in enumerate(tones) if x <= m); return tones[min(len(tones) - 1, i + k)]
    if fx["mode"] == "steps":
        h = int(n * fx["hold"]); base = float(np.median(69 + 12 * np.log2(f0c[max(0, h - 40):h + 1] / 440)))
        base_t = scale_up(base, 0); perStep = int(fx["step"] * 1000 / FRAME_MS); midi = np.full(n, base)
        for j in range(h, n): k = min(7, 1 + (j - h) // perStep); midi[j] = scale_up(base_t, k)       # flat on each step
        # a 12 ms glide between steps so it reads as sung, not switched
        gl = max(1, int(12 / FRAME_MS)); sm = midi.copy()
        for j in range(h + 1, n):
            if midi[j] != midi[j - 1]:
                for q in range(gl): 
                    if j + q < n: sm[j + q] = midi[j - 1] + (midi[j] - midi[j - 1]) * (q + 1) / gl
        ratio = 2 ** ((sm - (69 + 12 * np.log2(f0c / 440))) / 12)        # replace her pitch with the step, keep her vibrato out
        ratio[:h] = 1
    elif fx["mode"] == "hold": pass                                       # ratio stays 1 — her pitch, extended
    elif fx["mode"] in ("slide", "rise"):
        h = int(n * fx["hold"]); k = np.linspace(0, 1, max(1, n - h)); k = k * k * (3 - 2 * k)
        ratio[h:] = 2 ** (fx["semis"] / 12 * k)
    else:
        ratio[:] = 2 ** (fx["semis"] / 12)
    f0n, spn, apn = f0c * ratio, sp, ap
    if fx.get("extend"):
        # v94: a TIME-WARP of the whole word, not a patch — her onset at speed, the vowel body stretched (the frame cursor crawls
        # through the middle of the note with a little drift so it never buzzes), then her own "ng" and release at speed.
        # The pitch curve (ramp, cascade) is laid over the warped timeline; the ending keeps the last pitch.
        n = len(f0c); a_, b_ = int(n * 0.28), int(n * 0.80)                   # onset | vowel body | the ng + release
        ext = int(fx["extend"] * 1000 / FRAME_MS); body_out = (b_ - a_) + ext
        idx = list(range(0, a_))
        drift = 0.0
        for j in range(body_out):
            u = j / max(1, body_out - 1); base = a_ + u * (b_ - a_ - 1); drift = 0.9 * drift + 0.35 * np.random.randn()
            idx.append(min(b_ - 1, max(a_, base + drift)))
        idx += list(range(b_, n)); idx = np.array(idx, dtype=float); nf = len(idx)
        lo = np.floor(idx).astype(int); hi = np.minimum(lo + 1, n - 1); fr = idx - lo
        spn = sp[lo] * (1 - fr)[:, None] + sp[hi] * fr[:, None]; apn = ap[lo] * (1 - fr)[:, None] + ap[hi] * fr[:, None]
        base_hz = f0c[lo] * (1 - fr) + f0c[hi] * fr                              # her own f0 along the warped path
        if fx["mode"] == "steps":
            h = a_ + int(body_out * 0.12); perStep = int(fx["step"] * 1000 / FRAME_MS); midi = np.full(nf, 0.0)
            base = float(np.median(69 + 12 * np.log2(f0c[max(0, a_ - 20):a_ + 20] / 440))); base_t = scale_up(base, 0)
            top_k = 7; up_end = h + top_k * perStep
            for j in range(nf):
                if j < h: midi[j] = np.nan                                      # her own pitch
                elif j < up_end: midi[j] = scale_up(base_t, 1 + (j - h) // perStep)
                elif fx.get("arp"):                                             # v95: the cascade down — accelerating, then the last tones held longer and longer
                    tsec = (j - up_end) * FRAME_MS / 1000
                    SCHED = [fx["step"]] * 4 + [fx["step"] / 2] * 8 + [fx["step"] / 4] * 10 + [0.35, 0.55, 0.9, 1.4]   # 8ths, 16ths, 32nds, then a ritardando
                    k, acc = 0, 0.0
                    for d in SCHED:
                        if tsec >= acc + d: acc += d; k += 1
                        else: break
                    tones = [x for x in range(int(base_t) - 2, int(base_t) + 14) if x % 12 in SCALE]; ti = max(i for i, x in enumerate(tones) if x <= scale_up(base_t, top_k) + 0.1)
                    bi = max(i for i, x in enumerate(tones) if x <= base_t + 0.1)
                    midi[j] = tones[max(bi, ti - k)]                                # v97: the cascade stops on her note and holds it
                else: midi[j] = scale_up(base_t, top_k)
            gl = max(1, int(12 / FRAME_MS)); sm = midi.copy()
            for j in range(1, nf):
                if not np.isnan(midi[j]) and not np.isnan(midi[j - 1]) and midi[j] != midi[j - 1]:
                    for q in range(gl):
                        if j + q < nf: sm[j + q] = midi[j - 1] + (midi[j] - midi[j - 1]) * (q + 1) / gl
            f0n = np.where(np.isnan(sm), base_hz, 440 * 2 ** ((sm - 69) / 12))
        else:
            f0n = base_hz * (2 ** (fx["semis"] / 12))
        wob = 1 + 0.008 * np.sin(np.linspace(0, 2 * np.pi * 5.2 * nf * FRAME_MS / 1000, nf)); f0n = f0n * wob
        tailFade = np.ones(nf)
    else: tailFade = np.ones(len(f0n))
    y = pw.synthesize(np.ascontiguousarray(f0n), np.ascontiguousarray(spn), np.ascontiguousarray(apn), fs, frame_period=FRAME_MS)
    env = np.interp(np.arange(len(y)) / fs * 1000 / FRAME_MS, np.arange(len(tailFade)), tailFade); y = y[: len(env)] * env
    # the curve for the engine: time (stem s) and MIDI, every 25 ms, voiced frames only
    curve = [(fx["t0"] + j * FRAME_MS / 1000, 69 + 12 * np.log2(f0n[j] / 440)) for j in range(0, len(f0n), 5) if f0n[j] > 0]
    with open(os.path.join(OUT, fx["name"] + ".curve"), "w") as cf:
        for t, m in curve: cf.write(f"{t:.3f} {m:.2f}\n")
    seg = np.pad(seg, (0, max(0, len(y) - len(seg))))[: len(y)] if len(y) > len(seg) else seg
    # match the segment's level, then a short fade either end
    g = (np.sqrt(np.mean(seg ** 2)) + 1e-9) / (np.sqrt(np.mean(y ** 2)) + 1e-9); y = y * min(g, 4.0)
    f = int(0.02 * fs); y[:f] *= np.linspace(0, 1, f); y[-f:] *= np.linspace(1, 0, f)
    path = os.path.join(OUT, fx["name"] + ".wav"); sf.write(path, y.astype(np.float32), fs, subtype="FLOAT")
    voiced = float(np.mean(f0 > 0))
    manifest.append({k: v for k, v in fx.items()} | {"file": os.path.relpath(path, LANE), "voiced": round(voiced, 2), "len": round(len(y) / fs, 3)})
    print(f"  {fx['name']:14s} {fx['t0']:.2f}–{fx['t1']:.2f}  {fx['mode']} {fx['semis']:+d} st  voiced {voiced:.0%}")
# v68: HER VOWELS — "ooo" from her held "long", "aaa" from her "saw": WORLD-analyzed once, then synthesized flat at each chord
# tone (a slow vibrato, 10 cents), four seconds long on looped body frames, for the engine's vowel choir (src/vox/vowels/)
VOW = os.path.join(LANE, "src/vox/vowels"); os.makedirs(VOW, exist_ok=True)
SOURCES = {"oo": (86.90, 87.55), "aa": (26.70, 27.20)}
NOTES_V = [56, 59, 63, 66, 68, 71, 75, 78]
for name, (a, b) in SOURCES.items():
    seg = np.ascontiguousarray(x[int(a * fs):int(b * fs)])
    f0, t = pw.harvest(seg, fs, f0_floor=120.0, f0_ceil=900.0, frame_period=FRAME_MS); f0 = pw.stonemask(seg, f0, t, fs)
    sp = pw.cheaptrick(seg, f0, t, fs); ap = pw.d4c(seg, f0, t, fs); v = f0 > 0
    body = np.where(v)[0]; L = max(8, len(body) // 2); mid = body[len(body) // 2]; i0, i1 = max(0, mid - L // 2), min(len(f0) - 1, mid + L // 2)
    n = int(4.0 * 1000 / FRAME_MS); pick = [i0 + int(abs(((j / (i1 - i0)) % 2) - 1) * (i1 - i0 - 1)) for j in range(n)]   # ping-pong through the body
    spn, apn = sp[pick] * (1 + 0.015 * np.sin(np.linspace(0, 9, n)))[:, None], ap[pick]
    for m in NOTES_V:
        hz = 440 * 2 ** ((m - 69) / 12); vib = 1 + 0.006 * np.sin(np.linspace(0, 2 * np.pi * 5.2 * 4.0, n)); f0n = np.full(n, hz) * vib
        y = pw.synthesize(np.ascontiguousarray(f0n), np.ascontiguousarray(spn), np.ascontiguousarray(apn), fs, frame_period=FRAME_MS)
        g = 0.25 / (np.abs(y).max() + 1e-9); y = y * g; fi, fo = int(0.25 * fs), int(0.6 * fs); y[:fi] *= np.linspace(0, 1, fi); y[-fo:] *= np.linspace(1, 0, fo)
        sf.write(os.path.join(VOW, f"{name}-{m}.wav"), y.astype(np.float32), fs, subtype="FLOAT")
    print(f"  vowel {name}: {len(NOTES_V)} notes from {a:.2f}–{b:.2f}")
json.dump(manifest, open(os.path.join(OUT, "fx.json"), "w"), indent=1)
print(f"✓ {OUT}/fx.json")
