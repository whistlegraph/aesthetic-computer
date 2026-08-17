# halo2.py — the v3.1 additions to the angelic bank.
#
# Three new object families, all WORLD renders (the aesthetivox rule):
#
#   ens-*     THE ENSEMBLE. @jeffrey: "yes lets get the other loner takes
#             in as well maybe as like an ensemble". The corpus found the
#             duet take (du, 7076361738786213166) performs the whole
#             "not again!" arrangement IN Camille's A# frame, and the
#             origin trio (o, 6988619239657622790) sings the full lyric
#             loosely below it. Both become crowd swells: gentle grid
#             pull (strength 0.7 — the pull makes the crowd diatonic
#             without un-crowding it), vowels-only, darker, breathier —
#             group vowels that rise behind the drops, never a lead.
#   *-long    THE SUPER-STONE. "hold her voice super long like offff a
#             stoooooooooooone". sing.py's move, not a granulator: the
#             frame axis is warped so the vowel absorbs the hold — 1:1
#             through "of a st…", the held vowel stretched to seconds
#             with a ±2.2-frame read shimmer at 0.85 Hz (the frozen-
#             envelope trick), 1:1 through the release — with 15¢ / 5.2
#             Hz vibrato fading in over the hold. Variants: the full
#             "of a stooone" at her own D#4 (8 s), stone-only canon
#             layers at A#4 and D#5 (6.5 s), a 4 s echo, and a "paaass"
#             miniature at C#4 / C#5.
#   hk-of-a   A unison double for the climax: the hk take's clean A#4
#             "of a" (7100768279983181099 — the corpus's best non-f
#             pressing, −10¢, fully voiced), lead treatment.
#
# Reads source/ + samples/ + vox3/.onsets.json; writes vox3/*.wav and
# appends provenance to vox3/.manifest2.json.
#
#   pop/.venv/bin/python pop/loner/bin/halo2.py

import json, os
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
VOX3 = os.path.join(LANE, "vox3")

TONIC = 237.0
MINOR = np.array([0, 2, 3, 5, 7, 8, 10])
FRAME_MS = 5.0
SNAP, SMOOTH_MS, FORMANT_DB, AIR_DB = 0.70, 45.0, 1.6, 2.5
BREATH, HALO_DARK = 0.14, 5500.0


def smooth(x, frames):
    if frames <= 1:
        return x
    k = np.hanning(frames * 2 + 1)
    k /= k.sum()
    return np.convolve(x, k, mode="same")


def cents_to_grid(hz):
    cents = 1200.0 * np.log2(hz / TONIC)
    pc = np.mod(cents, 1200.0)
    grid = np.concatenate([MINOR * 100.0, [1200.0]])
    dev = pc[:, None] - grid[None, :]
    return dev[np.arange(len(pc)), np.argmin(np.abs(dev), axis=1)]


def analyze(x, fs, floor=140.0):
    f0_raw, t = pw.harvest(x, fs, f0_floor=floor, f0_ceil=700.0, frame_period=FRAME_MS)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft = pw.get_cheaptrick_fft_size(fs, f0_floor=floor)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft, f0_floor=floor)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft)
    voiced = f0 > 0
    corr = np.zeros_like(f0)
    if voiced.any():
        corr[voiced] = -cents_to_grid(f0[voiced]) * SNAP
    corr = smooth(corr, int(SMOOTH_MS / FRAME_MS))
    f0c = np.where(voiced, f0 * 2.0 ** (corr / 1200.0), 0.0)
    return dict(x=x, fs=fs, f0c=f0c, sp=sp, ap=ap, voiced=voiced)


def shelf(freqs, centre, width):
    return 1.0 / (1.0 + np.exp(-(freqs - centre) / width))


def dress(y, fs, tip_s=0.005):
    peak = np.max(np.abs(y)) or 1.0
    y = y * (0.90 / peak)
    tip = int(tip_s * fs)
    w = 0.5 - 0.5 * np.cos(np.pi * np.arange(tip) / tip)
    y[:tip] *= w
    y[-tip:] *= w[::-1]
    return y.astype(np.float32)


def vuv_mask(voiced, fs, n):
    spf = int(round(fs * FRAME_MS / 1000.0))
    mask = np.repeat(voiced.astype(np.float64), spf)
    mask = np.pad(mask, (0, max(0, n - len(mask))), mode="edge")[:n]
    ramp = int(0.005 * fs)
    edges = np.diff(mask.astype(np.int8))
    for idx in np.where(edges == 1)[0]:
        k = np.arange(min(ramp, n - idx - 1))
        mask[idx + 1 + k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
    for idx in np.where(edges == -1)[0]:
        k = np.arange(min(ramp, idx + 1))
        mask[idx - k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
    return mask


manifest = {}

# ── the ensemble ──────────────────────────────────────────────────────
ENS = {
    "ens-du-1": ("7076361738786213166", 3.50, 8.40, "getting curled up in myself i think (duet)"),
    "ens-du-2": ("7076361738786213166", 8.93, 18.30, "of a stone just waiting very patiently (duet)"),
    "ens-du-3": ("7076361738786213166", 18.66, 22.60, "for time to pass (duet)"),
    "ens-o-1":  ("6988619239657622790", 4.50, 9.60, "sitting curled up in myself (trio)"),
    "ens-o-2":  ("6988619239657622790", 11.00, 13.60, "of a stone (trio)"),
    "ens-o-3":  ("6988619239657622790", 21.45, 25.60, "for time to pass (trio)"),
}
for name, (vid, t0, t1, words) in ENS.items():
    x, fs = sf.read(os.path.join(LANE, "source", f"{vid}-48k.wav"), dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)
    seg = x[int(t0 * fs):int(t1 * fs)].copy()
    a = analyze(seg, fs, floor=110.0)
    freqs = np.linspace(0.0, fs / 2.0, a["sp"].shape[1])
    sp = a["sp"] * (1.0 / (1.0 + (freqs / HALO_DARK) ** 2))[None, :]
    ap = np.minimum(1.0, a["ap"] + BREATH * 1.5 * shelf(freqs, 8000.0, 800.0)[None, :])
    vi = np.where(a["voiced"])[0]
    f0s = (np.exp(np.interp(np.arange(len(a["f0c"])), vi,
                            np.log(np.maximum(a["f0c"][vi], 1e-6))))
           if vi.size >= 2 else np.maximum(a["f0c"], 1e-6))
    y = pw.synthesize(f0s, np.ascontiguousarray(sp), np.ascontiguousarray(ap),
                      fs, frame_period=FRAME_MS)
    n = min(len(y), len(seg))
    out = vuv_mask(a["voiced"], fs, n) * y[:n]        # vowels only — a crowd of vowels
    sf.write(os.path.join(VOX3, f"{name}.wav"), dress(out, fs), fs)
    manifest[name] = dict(post=vid, span=[t0, t1], words=words, mode="crowd",
                          dur=round(n / fs, 3))
    print(f"  crowd  {name:12s} {n / fs:5.2f}s  {words}")

# ── the super-stone ───────────────────────────────────────────────────
# (slice, region seconds: 1:1 until s0, stretch [s0,s1] to hold, 1:1 after;
#  A-start offset, target semitone, hold seconds)
LONGS = {
    "stone-long-5":    ("f-of-a-stone", 0.0, 1.35, 3.10, 5, 6.5),   # of a STOOONE, her D#4
    "stone-long-12":   ("f-of-a-stone", 1.08, 1.35, 3.10, 12, 5.6), # stone only, A#4
    "stone-long-17":   ("f-of-a-stone", 1.08, 1.35, 3.10, 17, 5.6), # stone only, D#5
    "stone-long-echo": ("f-of-a-stone", 1.08, 1.35, 3.10, 5, 2.8),  # the BREATHE ghost
    "pass-long-3":     ("f-for-time-to-pass", 4.26, 4.50, 5.45, 3, 1.9),
    "pass-long-15":    ("f-for-time-to-pass", 4.26, 4.50, 5.45, 15, 1.9),
}
for name, (src, a0, s0, s1, st, hold) in LONGS.items():
    x, fs = sf.read(os.path.join(LANE, "samples", f"{src}.wav"), dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)
    a = analyze(x, fs)
    F = len(a["f0c"])
    fa, f_s0, f_s1 = int(a0 / 0.005), int(s0 / 0.005), int(s1 / 0.005)
    outS = int(hold / 0.005)
    # the warp: A 1:1 · S stretched with read shimmer · R 1:1
    idx = list(range(fa, f_s0))
    for k in range(outS):
        p = f_s0 + (k / max(1, outS - 1)) * (f_s1 - 1 - f_s0)
        p += 2.2 * np.sin(2 * np.pi * 0.85 * k * 0.005)
        idx.append(int(np.clip(round(p), f_s0, f_s1 - 1)))
    idx += list(range(f_s1, F))
    idx = np.array(idx)
    sp_o = np.ascontiguousarray(a["sp"][idx])
    ap_o = np.ascontiguousarray(a["ap"][idx])
    voiced_o = a["voiced"][idx]
    # f0: source contour shifted so the held tone lands on the target,
    # then FLAT through the hold with vibrato fading in over 0.4 s
    med = np.median(a["f0c"][f_s0:f_s1][a["voiced"][f_s0:f_s1]])
    ratio = (TONIC * 2.0 ** (st / 12.0)) / med
    f0_o = a["f0c"][idx] * ratio
    hold_a, hold_b = f_s0 - fa, f_s0 - fa + outS
    tgt = TONIC * 2.0 ** (st / 12.0)
    tsec = np.arange(outS) * 0.005
    vib = 2.0 ** (0.15 * np.clip(tsec / 0.4, 0, 1) * np.sin(2 * np.pi * 5.2 * tsec) / 12.0)
    f0_o[hold_a:hold_b] = tgt * vib
    f0_o = np.where(voiced_o, f0_o, 0.0)
    # lead treatment: formant + air + breath (full depth through the hold)
    freqs = np.linspace(0.0, fs / 2.0, sp_o.shape[1])
    sp_o = sp_o * (10.0 ** ((FORMANT_DB * np.exp(-((freqs - 2800.0) / 450.0) ** 2)
                             + AIR_DB * shelf(freqs, 8000.0, 900.0)) / 10.0))[None, :]
    w = np.zeros(len(idx)); w[hold_a:hold_b] = np.clip(tsec / 0.25, 0, 1)
    ap_o = np.minimum(1.0, ap_o + BREATH * w[:, None] * shelf(freqs, 8000.0, 800.0)[None, :])
    vi = np.where(voiced_o)[0]
    f0s = (np.exp(np.interp(np.arange(len(f0_o)), vi, np.log(np.maximum(f0_o[vi], 1e-6))))
           if vi.size >= 2 else np.maximum(f0_o, 1e-6))
    y = pw.synthesize(f0s, sp_o, ap_o, fs, frame_period=FRAME_MS)
    # composite the original consonants back where the map is 1:1 —
    # rebuild a warped copy of the source frame by frame (5 ms blocks)
    n = len(y)
    mask = vuv_mask(voiced_o, fs, n)
    spf = int(fs * 0.005)
    xw = np.zeros(n)
    for j, srcf in enumerate(idx):
        a0s, b0s = j * spf, min((j + 1) * spf, n)
        if a0s >= n:
            break
        s0s = srcf * spf
        blk = x[s0s:s0s + (b0s - a0s)]
        xw[a0s:a0s + len(blk)] = blk
    out = mask * y + (1 - mask) * xw
    sf.write(os.path.join(VOX3, f"{name}.wav"), dress(out, fs), fs)
    manifest[name] = dict(source=src, st=st, hold_s=hold, dur=round(n / fs, 3),
                          mode="duration-control")
    print(f"  long   {name:16s} {n / fs:5.2f}s  st+{st} hold {hold}s")

# ── the hk unison double ──────────────────────────────────────────────
x, fs = sf.read(os.path.join(LANE, "source", "7100768279983181099-48k.wav"), dtype="float64")
if x.ndim > 1:
    x = x.mean(axis=1)
seg = x[int(7.41 * fs):int(8.66 * fs)].copy()
a = analyze(seg, fs)
freqs = np.linspace(0.0, fs / 2.0, a["sp"].shape[1])
sp = a["sp"] * (10.0 ** ((FORMANT_DB * np.exp(-((freqs - 2800.0) / 450.0) ** 2)
                          + AIR_DB * shelf(freqs, 8000.0, 900.0)) / 10.0))[None, :]
ap = np.minimum(1.0, a["ap"] + BREATH * shelf(freqs, 8000.0, 800.0)[None, :] * 0.7)
vi = np.where(a["voiced"])[0]
f0s = (np.exp(np.interp(np.arange(len(a["f0c"])), vi,
                        np.log(np.maximum(a["f0c"][vi], 1e-6))))
       if vi.size >= 2 else np.maximum(a["f0c"], 1e-6))
y = pw.synthesize(f0s, np.ascontiguousarray(sp), np.ascontiguousarray(ap), fs,
                  frame_period=FRAME_MS)
n = min(len(y), len(seg))
mask = vuv_mask(a["voiced"], fs, n)
out = mask * y[:n] + (1 - mask) * seg[:n]
sf.write(os.path.join(VOX3, "hk-of-a.wav"), dress(out, fs), fs)
manifest["hk-of-a"] = dict(post="7100768279983181099", span=[7.41, 8.66],
                           words="of a (A#4, the corpus's best non-f pressing)",
                           mode="lead")
print(f"  lead   hk-of-a          {n / fs:5.2f}s  of a @ A#4")

json.dump(manifest, open(os.path.join(VOX3, ".manifest2.json"), "w"), indent=1)
print(f"WROTE {VOX3}/.manifest2.json ({len(manifest)} renders)")
