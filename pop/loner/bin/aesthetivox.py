# aesthetivox.py — every v2 vocal goes through the WORLD chain.
#
# The rule (established on the cult lane): vocals never skip the
# aesthetivox — an exposed unprocessed take is too raw to ship. But
# loner's charm IS Camille's rubato, so this is not sing.py's re-scoring:
# it is the pop_world_autotune move — decompose with WORLD
# (harvest → stonemask → cheaptrick → d4c), gently pull the f0 contour
# toward the lane's scale, and resynthesize. Her phrasing, timing and
# slides survive; every exposed sample becomes a WORLD render.
#
# TUNING. The band is built on TONIC = 237 Hz (A#3 +30 cents — the Feral
# File take's own centre), so the correction grid is A# natural minor in
# THAT frame, not A440's. Option (b) from the v2 brief: the +30¢ offset
# lives here, in the targets, and the band's TONIC stays untouched.
#
# Two modes:
#
#   sung    voiced frames snap toward the nearest scale tone at
#           strength 0.7, with the per-frame correction smoothed over
#           ~45 ms so vibrato and slides bend rather than staircase;
#           +1.6 dB singer's formant (longdots.sh's ballad-gentle
#           setting, not sing.py's 3.2 default)
#   spoken  no grid — the f0 range is compressed toward its median
#           (^0.55) and dropped a semitone, and the envelope is darkened
#           with a power-domain tilt above ~3.5 kHz. Clearly treated
#           material, no longer raw speech.
#
# Both modes keep pitchsnap_world.py's two survival tricks: f0 is made
# continuous through unvoiced gaps before synthesis (WORLD pops on
# 0→target jumps), and the voiced/unvoiced structure is re-imposed on the
# OUTPUT in the time domain — WORLD audio in voiced regions, the original
# take's consonants composited back in unvoiced ones, 5 ms cosine ramps
# at every seam.
#
# Reads samples/<name>.wav, writes vox/<name>.wav + vox/.manifest.json
# with a pyin before/after receipt (median |cents to grid| per slice).
#
#   pop/.venv/bin/python pop/loner/bin/aesthetivox.py

import json, os
import numpy as np
import soundfile as sf
import pyworld as pw
import librosa

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
VOX = os.path.join(LANE, "vox")
os.makedirs(VOX, exist_ok=True)

TONIC = 237.0                        # A#3 in the take's own tuning
MINOR = np.array([0, 2, 3, 5, 7, 8, 10])   # natural minor, semitones

# name → (mode, f0_floor). Only the slices the v2 score plays.
SLICES = {
    "f-sitting-curled":    ("sung", 140.0),
    "f-think-stone":       ("sung", 140.0),
    "f-i-think":           ("sung", 140.0),
    "f-of-a-stone":        ("sung", 140.0),
    "f-stone":             ("sung", 140.0),
    "f-waiting-patiently": ("sung", 140.0),
    "f-for-time-to-pass":  ("sung", 140.0),
    "f-pass":              ("sung", 140.0),
    "f-whole-line":        ("sung", 140.0),
    "n-getting-curled":    ("sung", 140.0),
    "n-stone-waiting":     ("sung", 140.0),
    "n-of-a-stone":        ("sung", 140.0),
    "n-for-time-to-pass":  ("sung", 140.0),
    "n-emo-again":         ("spoken", 75.0),
    "n-i-knew-it":         ("spoken", 75.0),
    "o-heres-loner":       ("spoken", 75.0),
}

SNAP_STRENGTH = 0.70
SMOOTH_MS = 45.0
FORMANT_DB = 1.6
FRAME_MS = 5.0


def cents_to_grid(hz):
    """Signed cents from the nearest A#-natural-minor tone (237 Hz frame)."""
    cents = 1200.0 * np.log2(hz / TONIC)
    pc = np.mod(cents, 1200.0)
    grid = np.concatenate([MINOR * 100.0, [1200.0]])
    dev = pc[:, None] - grid[None, :]
    return dev[np.arange(len(pc)), np.argmin(np.abs(dev), axis=1)]


def pyin_grid_dev(y, sr):
    """Median |cents to grid| over confidently-voiced pyin frames."""
    f0, vf, vp = librosa.pyin(y.astype(np.float32), fmin=80, fmax=600,
                              sr=sr, frame_length=4096)
    v = f0[(vf if vf is not None else np.zeros(0, bool)) & (vp > 0.5)]
    v = v[np.isfinite(v)]
    if v.size < 5:
        return None
    return float(np.median(np.abs(cents_to_grid(v))))


def smooth(x, frames):
    if frames <= 1:
        return x
    k = np.hanning(frames * 2 + 1)
    k /= k.sum()
    return np.convolve(x, k, mode="same")


manifest = {}
for name, (mode, floor) in SLICES.items():
    src = os.path.join(LANE, "samples", f"{name}.wav")
    if not os.path.exists(src):
        print(f"  ! missing {src} — run bin/slice.mjs first")
        continue
    x, fs = sf.read(src, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)

    f0_raw, t = pw.harvest(x, fs, f0_floor=floor, f0_ceil=600.0, frame_period=FRAME_MS)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft_size = pw.get_cheaptrick_fft_size(fs, f0_floor=floor)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft_size, f0_floor=floor)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft_size)
    voiced = f0 > 0

    if mode == "sung":
        # Gentle pull toward the scale. The correction (not the pitch) is
        # smoothed, so slides between notes stay slides and vibrato keeps
        # its shape — only the centre of each held tone comes home.
        corr = np.zeros_like(f0)
        corr[voiced] = -cents_to_grid(f0[voiced]) * SNAP_STRENGTH
        corr = smooth(corr, int(SMOOTH_MS / FRAME_MS))
        f0_new = np.where(voiced, f0 * 2.0 ** (corr / 1200.0), 0.0)
        # longdots' ballad-gentle presence, not sing.py's 3.2 default
        freqs = np.linspace(0.0, fs / 2.0, sp.shape[1])
        lift_db = FORMANT_DB * np.exp(-((freqs - 2800.0) / 450.0) ** 2)
        sp = sp * (10.0 ** (lift_db / 10.0))[None, :]        # sp is power
    else:
        # Treated speech: range toward the median, down a semitone, lid on.
        med = np.median(f0[voiced]) if voiced.any() else 150.0
        f0_new = np.where(voiced, med * (f0 / med) ** 0.55 * 2.0 ** (-1 / 12), 0.0)
        freqs = np.linspace(0.0, fs / 2.0, sp.shape[1])
        sp = sp * (1.0 / (1.0 + (freqs / 3500.0) ** 2))[None, :]

    # Continuous f0 through unvoiced gaps (WORLD pops on 0→target jumps).
    if voiced.sum() >= 2:
        vi = np.where(voiced)[0]
        f0_synth = np.exp(np.interp(np.arange(len(f0)), vi,
                                    np.log(np.maximum(f0_new[vi], 1e-6))))
    else:
        f0_synth = np.maximum(f0_new, 1e-6)

    y = pw.synthesize(f0_synth, sp, ap, fs, frame_period=FRAME_MS)

    # Re-impose v/uv in the time domain; composite the original take's
    # unvoiced audio back so sibilants stay real.
    spf = int(round(fs * FRAME_MS / 1000.0))
    mask = np.repeat(voiced.astype(np.float64), spf)
    n = min(len(y), len(x))
    mask = np.pad(mask, (0, max(0, n - len(mask))), mode="edge")[:n]
    ramp = int(0.005 * fs)
    edges = np.diff(mask.astype(np.int8))
    for idx in np.where(edges == 1)[0]:
        k = np.arange(min(ramp, n - idx - 1))
        mask[idx + 1 + k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
    for idx in np.where(edges == -1)[0]:
        k = np.arange(min(ramp, idx + 1))
        mask[idx - k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
    out = mask * y[:n] + (1.0 - mask) * x[:n]

    # dress-lite: normalize and top-and-tail so no render can click
    peak = np.max(np.abs(out)) or 1.0
    out *= 0.90 / peak
    tip = int(0.004 * fs)
    w = 0.5 - 0.5 * np.cos(np.pi * np.arange(tip) / tip)
    out[:tip] *= w
    out[-tip:] *= w[::-1]

    dst = os.path.join(VOX, f"{name}.wav")
    sf.write(dst, out.astype(np.float32), fs)

    before = pyin_grid_dev(x, fs)
    after = pyin_grid_dev(out, fs) if mode == "sung" else None
    manifest[name] = dict(
        mode=mode, f0_floor=floor,
        snap_strength=SNAP_STRENGTH if mode == "sung" else None,
        formant_db=FORMANT_DB if mode == "sung" else None,
        grid=f"A# natural minor, tonic {TONIC} Hz (+30c over A440)",
        dur=round(n / fs, 3),
        cents_to_grid_before=round(before, 1) if before is not None else None,
        cents_to_grid_after=round(after, 1) if after is not None else None,
    )
    m = manifest[name]
    print(f"  {mode:6s} {name:22s} {m['dur']:6.2f}s  grid-dev "
          f"{m['cents_to_grid_before']} → {m['cents_to_grid_after']}")

json.dump(manifest, open(os.path.join(VOX, ".manifest.json"), "w"), indent=1)
print(f"WROTE {VOX}/.manifest.json ({len(manifest)} renders)")
