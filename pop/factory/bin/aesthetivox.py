# aesthetivox.py — every v2 vocal goes through the WORLD chain.
#
# The rule (established on the cult lane, applied fleet-wide): vocals
# never skip the aesthetivox — an exposed unprocessed take is too raw to
# ship. Same chain as pop/loner/bin/aesthetivox.py (harvest → stonemask →
# cheaptrick → d4c, correct f0, resynthesize, composite the consonants
# back), pressed hard: quantized pitch IS the cookie-cutter concept.
#
# ══ THE GLITCH POST-MORTEM ════════════════════════════════════════════
#
# @jeffrey on the first pressing: "factory has bad glitchy aesthetivox".
# Measured cause, not guessed: these chant hits GLIDE — p95 pitch
# velocity 1–3 semitones per 5 ms frame — so a per-frame nearest-tone
# target flapped between adjacent scale tones 24–71 times per word
# (swings >60 c/frame, max ~187 c), and at strength 0.95 through only
# 18 ms of smoothing that wrote square-wave FM into WORLD's f0: the
# burble. On top, harvest at f0_floor=100 dropped 1–3 octave-class
# tracking errors per word into the track (floor 65 / ceil 400 removes
# cookie's entirely).
#
# The press is still full-strength. What changed is that it now presses
# a STABLE reading of the pitch:
#
#   1. the f0 track is made continuous and DE-SPIKED (frames >6 st from
#      their 45 ms median are tracking errors, not performance);
#   2. the target tone is chosen from a 35 ms median-smoothed contour
#      WITH HYSTERESIS — the target may only change once the smoothed
#      pitch has arrived within 45 cents of the new tone. No flapping;
#   3. the correction (target − smoothed contour, clamped ±250 c) is
#      applied to the de-spiked contour and smoothed 30 ms. Micro-detail
#      rides on top untouched — a pressed machine, not a broken codec;
#   4. consonant composite seams widened 5 → 15 ms;
#   5. takes whose harvest voicing is thin (<60 % voiced) press at 0.75
#      instead of 0.93;
#   6. every stamped/owngrid render is warble-checked after synthesis
#      (pyin, frames jumping >1 st); anything still above 0.12 falls
#      back to PLAIN de-spiked resynthesis — treated, never corrected
#      into artifacts — and the manifest says so.
#
# TUNING. TONIC = 146.83 Hz — D3 in the A440 frame, where the chant
# actually sits. Four modes: stamped (D natural minor), owngrid
# (chromatic — the wrong dies keep their own pitches), spoken (loner's
# treated speech: range ^0.55 toward median, down a semitone, 3.5 kHz
# lid), birdlow (spoken's compression locked to G#2, untransposed).
#
# OUT OF SPEC: copy k's words are rendered against a grid detuned
# ±(k·9) cents (sign alternating per word) — the aesthetivox targets
# out of calibration — as <name>.d<k>.wav. k=5 presses the b die, k=6
# the 2026 "cookay cubber" die (f), k=7 the bright c die.
#
# Reads samples/<name>.wav, writes vox/<name>.wav + vox/.manifest.json
# with pyin before/after and warble receipts.
#
#   cd pop/factory && ../.venv/bin/python bin/aesthetivox.py

import json, os
import numpy as np
import soundfile as sf
import pyworld as pw
import librosa
from scipy.ndimage import median_filter

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
VOX = os.path.join(LANE, "vox")
os.makedirs(VOX, exist_ok=True)

TONIC = 146.83                        # D3 — the chant's measured root
MINOR = np.array([0, 2, 3, 5, 7, 8, 10])
CHROMA = np.arange(12)

STRENGTH = 0.93
STRENGTH_THIN = 0.75                  # when harvest voicing is thin
SMOOTH_MS = 30.0
FORMANT_DB = 2.4
FRAME_MS = 5.0
BIRD_HZ = 103.83                      # G#2, kept
WARBLE_LIMIT = 0.12                   # fallback line, in jump-fraction

# name → (mode, f0_floor, f0_ceil). Only the slices the score plays.
LOW, MID, HIGH = (65.0, 400.0), (75.0, 450.0), (120.0, 550.0)
SLICES = {
    # the stamps and lines — D minor, pressed hard
    "factory":         ("stamped", *LOW),
    "cookie":          ("stamped", *LOW),
    "cutter":          ("stamped", *LOW),
    "personalities":   ("stamped", *LOW),
    "line1":           ("stamped", *LOW),
    "line2":           ("stamped", *LOW),
    "line3":           ("stamped", *LOW),
    "we":              ("stamped", *LOW),
    "break-free":      ("stamped", *LOW),
    "spinning-away":   ("stamped", *LOW),
    "bird":            ("stamped", *LOW),
    # the other dies — exact against their own chromatic pitches
    "factory-b":       ("owngrid", *LOW),
    "cookie-b":        ("owngrid", *LOW),
    "cutter-b":        ("owngrid", *LOW),
    "personalities-b": ("owngrid", *LOW),
    "line2-b":         ("owngrid", *LOW),
    "factory-c":       ("owngrid", *HIGH),
    "cutter-c":        ("owngrid", *HIGH),
    "personalities-c": ("owngrid", *HIGH),
    "spinning-away-c": ("owngrid", *HIGH),
    "bird-c":          ("owngrid", *HIGH),
    "factory-d":       ("owngrid", *HIGH),
    "cookie-d":        ("owngrid", *HIGH),
    "cutter-d":        ("owngrid", *HIGH),
    "personalities-d": ("owngrid", *HIGH),
    "line2-d":         ("owngrid", *HIGH),
    "line3-d":         ("owngrid", *HIGH),
    "factory-e":       ("owngrid", *LOW),
    "cookie-e":        ("owngrid", *LOW),
    "cutter-e":        ("owngrid", *LOW),
    "personalities-e": ("owngrid", *LOW),
    "line3-e":         ("owngrid", *LOW),
    "factory-f":       ("owngrid", 200.0, 700.0),
    "cookie-f":        ("owngrid", 200.0, 700.0),
    "cutter-f":        ("owngrid", 200.0, 700.0),
    "personalities-f": ("owngrid", 200.0, 700.0),
    "spinning-g":      ("owngrid", *LOW),
    "bird-chain-g":    ("owngrid", *LOW),
    "bird-chain-h":    ("owngrid", *LOW),
    # the operator, through the intercom
    "count-in":        ("spoken", *MID),
    "and":             ("spoken", *MID),
    "heres-the-factory": ("spoken", *MID),
    "intercom-wondering": ("spoken", *MID),
    "intercom-sameway":   ("spoken", *MID),
    # the last word
    "bird-b":          ("birdlow", 60.0, 300.0),
}

# OUT OF SPEC: copy k (1..7), the four stamp words, alternating-sign
# detune, and the die swaps: b at k=5, f (cookay cubber) at k=6, c at
# k=7, base fallback where a take lacks a word.
STAMP_WORDS = ["factory", "cookie", "cutter", "personalities"]
DRIFTS = []
for k in range(1, 8):
    take = {5: "-b", 6: "-f", 7: "-c"}.get(k, "")
    for w, base in enumerate(STAMP_WORDS):
        name = base + take
        if not os.path.exists(os.path.join(LANE, "samples", f"{name}.wav")):
            name = base
        DRIFTS.append((name, k, (1 if w % 2 == 0 else -1) * k * 9))


def cents_to_grid(hz, grid_semis, tonic):
    cents = 1200.0 * np.log2(hz / tonic)
    pc = np.mod(cents, 1200.0)
    grid = np.concatenate([grid_semis * 100.0, [1200.0]])
    dev = pc[:, None] - grid[None, :]
    return dev[np.arange(len(pc)), np.argmin(np.abs(dev), axis=1)]


def pyin_grid_dev(y, sr, grid_semis, tonic):
    f0, vf, vp = librosa.pyin(y.astype(np.float32), fmin=60, fmax=700,
                              sr=sr, frame_length=4096)
    v = f0[(vf if vf is not None else np.zeros(0, bool)) & (vp > 0.5)]
    v = v[np.isfinite(v)]
    if v.size < 5:
        return None
    return float(np.median(np.abs(cents_to_grid(v, grid_semis, tonic))))


def warble_of(y, sr):
    """Fraction of confidently-voiced pyin frames jumping >1 st — the
    burble meter that caught the first pressing."""
    f0, vf, vp = librosa.pyin(y.astype(np.float32), fmin=60, fmax=700,
                              sr=sr, frame_length=2048)
    v = f0[(vf) & (vp > 0.5)]
    v = v[np.isfinite(v)]
    if len(v) < 8:
        return None, len(v)
    st = 12 * np.log2(v[1:] / v[:-1])
    return float(np.mean(np.abs(st) > 1.0)), len(v)


def smooth(x, frames):
    if frames <= 1:
        return x
    k = np.hanning(frames * 2 + 1)
    k /= k.sum()
    return np.convolve(x, k, mode="same")


def stable_track(f0, voiced):
    """Continuous, de-spiked log-f0 — the reading the press can trust."""
    vi = np.where(voiced)[0]
    if len(vi) < 2:
        return np.maximum(f0, 1e-6)
    cont = np.exp(np.interp(np.arange(len(f0)), vi,
                            np.log(np.maximum(f0[vi], 1e-6))))
    med = median_filter(np.log2(cont), size=9)          # 45 ms
    spikes = np.abs(np.log2(cont) - med) > 0.5          # >6 st from local median
    cont[spikes] = 2.0 ** med[spikes]
    return cont


def quantize_hysteresis(sm_cents, grid_semis):
    """Nearest-tone with stickiness: the target only moves once the
    smoothed pitch has arrived within 45 c of the new tone."""
    grid = np.concatenate([grid_semis * 100.0, [1200.0]])
    pc = np.mod(sm_cents, 1200.0)
    dev = pc[:, None] - grid[None, :]
    j = np.argmin(np.abs(dev), axis=1)
    prop = sm_cents - dev[np.arange(len(pc)), j]        # per-frame proposal
    tgt = np.copy(prop)
    cur = prop[0]
    for i in range(1, len(prop)):
        if prop[i] != cur and abs(sm_cents[i] - prop[i]) < 45.0:
            cur = prop[i]
        tgt[i] = cur
    return tgt


def press(name, mode, floor, ceil, out_name=None, grid_offset_cents=0.0,
          force_plain=False, force_strength=None):
    src = os.path.join(LANE, "samples", f"{name}.wav")
    if not os.path.exists(src):
        print(f"  ! missing {src} — run bin/harvest.py first")
        return None
    x, fs = sf.read(src, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)

    f0_raw, t = pw.harvest(x, fs, f0_floor=floor, f0_ceil=ceil, frame_period=FRAME_MS)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft_size = pw.get_cheaptrick_fft_size(fs, f0_floor=floor)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft_size, f0_floor=floor)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft_size)
    voiced = f0 > 0
    vfrac = float(voiced.mean())
    freqs = np.linspace(0.0, fs / 2.0, sp.shape[1])

    cont = stable_track(f0, voiced)
    grid_semis = MINOR if mode == "stamped" else CHROMA
    tonic = TONIC * 2.0 ** (grid_offset_cents / 1200.0)
    strength = 0.0 if force_plain else \
        force_strength if force_strength is not None else \
        STRENGTH_THIN if vfrac < 0.60 else STRENGTH

    if mode in ("stamped", "owngrid"):
        sm = 2.0 ** median_filter(np.log2(cont), size=7)          # 35 ms
        sm_cents = 1200.0 * np.log2(sm / tonic)
        tgt = quantize_hysteresis(sm_cents, grid_semis)
        corr = np.clip(tgt - sm_cents, -250.0, 250.0) * strength
        corr = smooth(corr, int(SMOOTH_MS / FRAME_MS))
        f0_synth = cont * 2.0 ** (corr / 1200.0)
        lift_db = FORMANT_DB * np.exp(-((freqs - 2800.0) / 450.0) ** 2)
        sp = sp * (10.0 ** (lift_db / 10.0))[None, :]             # sp is power
    elif mode == "spoken":
        med = np.median(cont[voiced]) if voiced.any() else 150.0
        f0_synth = med * (cont / med) ** 0.55 * 2.0 ** (-1 / 12)
        sp = sp * (1.0 / (1.0 + (freqs / 3500.0) ** 2))[None, :]
    else:                                                          # birdlow
        med = np.median(cont[voiced]) if voiced.any() else BIRD_HZ
        f0_synth = BIRD_HZ * (cont / med) ** 0.55
        sp = sp * (1.0 / (1.0 + (freqs / 3500.0) ** 2))[None, :]

    y = pw.synthesize(np.maximum(f0_synth, 1e-6), sp, ap, fs, frame_period=FRAME_MS)

    # Re-impose v/uv in the time domain; the consonants stay the take's.
    # Seams are 15 ms raised cosine now, not 5 — a seam should breathe.
    spf = int(round(fs * FRAME_MS / 1000.0))
    mask = np.repeat(voiced.astype(np.float64), spf)
    n = min(len(y), len(x))
    mask = np.pad(mask, (0, max(0, n - len(mask))), mode="edge")[:n]
    ramp = int(0.015 * fs)
    edges = np.diff(mask.astype(np.int8))
    for idx in np.where(edges == 1)[0]:
        k = np.arange(min(ramp, n - idx - 1))
        mask[idx + 1 + k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
    for idx in np.where(edges == -1)[0]:
        k = np.arange(min(ramp, idx + 1))
        mask[idx - k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
    out = mask * y[:n] + (1.0 - mask) * x[:n]

    peak = np.max(np.abs(out)) or 1.0
    out *= 0.90 / peak
    tip = int(0.004 * fs)
    w = 0.5 - 0.5 * np.cos(np.pi * np.arange(tip) / tip)
    out[:tip] *= w
    out[-tip:] *= w[::-1]
    out = out.astype(np.float32)

    dst_name = out_name or name
    sf.write(os.path.join(VOX, f"{dst_name}.wav"), out, fs)

    wb, wn = warble_of(out, fs)
    before = pyin_grid_dev(x, fs, grid_semis, tonic)
    after = (pyin_grid_dev(out, fs, grid_semis, tonic)
             if mode in ("stamped", "owngrid") else None)
    return dict(
        mode="plain" if force_plain else mode, f0_floor=floor, f0_ceil=ceil,
        snap_strength=round(strength, 2) if mode in ("stamped", "owngrid") else None,
        formant_db=FORMANT_DB if mode in ("stamped", "owngrid") else None,
        grid=("D natural minor, tonic 146.83 Hz" if mode == "stamped"
              else "chromatic (own pitches)" if mode == "owngrid"
              else "none (treated speech)" if mode == "spoken"
              else "G#2 lock, 103.83 Hz"),
        grid_offset_cents=grid_offset_cents or None,
        harvest_voiced_frac=round(vfrac, 2),
        dur=round(n / fs, 3),
        warble=round(wb, 3) if wb is not None else None,
        warble_frames=wn,
        cents_to_grid_before=round(before, 1) if before is not None else None,
        cents_to_grid_after=round(after, 1) if after is not None else None,
    )


def press_checked(name, mode, floor, ceil, out_name=None, grid_offset_cents=0.0):
    """Press, then hold the render to the warble line. A take that will
    not press cleanly at full strength gets a gentler die (0.72, then
    0.55) before giving up to PLAIN de-spiked resynthesis — the concept
    survives wherever it can; artifacts never ship."""
    m = press(name, mode, floor, ceil, out_name, grid_offset_cents)
    if not m or mode not in ("stamped", "owngrid") or m["warble"] is None \
            or m["warble"] <= WARBLE_LIMIT:
        return m
    first = m["warble"]
    for s in (0.72, 0.55):
        m2 = press(name, mode, floor, ceil, out_name, grid_offset_cents,
                   force_strength=s)
        if m2 and m2["warble"] is not None and m2["warble"] <= WARBLE_LIMIT:
            m2["fallback"] = f"warble {first} at full strength — pressed at {s}"
            return m2
    m3 = press(name, mode, floor, ceil, out_name, grid_offset_cents,
               force_plain=True)
    if m3 and (m3["warble"] is None or m3["warble"] <= first):
        m3["fallback"] = f"warble {first} > {WARBLE_LIMIT} — plain resynthesis"
        return m3
    return m3 or m


manifest = {}
for name, (mode, floor, ceil) in SLICES.items():
    m = press_checked(name, mode, floor, ceil)
    if m:
        manifest[name] = m
        print(f"  {m['mode']:7s} {name:20s} {m['dur']:5.2f}s  warble {m['warble']}"
              f"  grid-dev {m['cents_to_grid_before']} → {m['cents_to_grid_after']}"
              f"{'  FALLBACK' if 'fallback' in m else ''}")

for name, k, off in DRIFTS:
    mode = "owngrid" if name.endswith(("-b", "-c", "-f")) else "stamped"
    _, floor, ceil = SLICES[name]
    out_name = f"{name}.d{k}"
    m = press_checked(name, mode, floor, ceil, out_name=out_name,
                      grid_offset_cents=off)
    if m:
        manifest[out_name] = m
        print(f"  drift   {out_name:20s} {off:+4d}c  warble {m['warble']}"
              f"  grid-dev {m['cents_to_grid_before']} → {m['cents_to_grid_after']}"
              f"{'  FALLBACK' if 'fallback' in m else ''}")

json.dump(manifest, open(os.path.join(VOX, ".manifest.json"), "w"), indent=1)
worst = max((m["warble"] or 0) for m in manifest.values())
falls = [k for k, m in manifest.items() if "fallback" in m]
print(f"WROTE {VOX}/.manifest.json ({len(manifest)} renders, worst warble "
      f"{worst}, fallbacks: {', '.join(falls) if falls else 'none'})")
