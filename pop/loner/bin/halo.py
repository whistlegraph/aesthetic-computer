# halo.py — the v3 aesthetivox bank: Camille, angelic, and only Camille.
#
# @jeffrey: "for camilles we need to like make her vocals more angelic" —
# then, streaming: arpeggios inside her held vowels, no other performers,
# and the track splitting up toward drum & bass. This script builds every
# vocal object v3 needs, all WORLD renders (the aesthetivox rule holds —
# chops included, because the score chops THESE files, never raw takes):
#
#   lead     v2's gentle grid pull (strength 0.7, 45 ms-smoothed
#            correction, +1.6 dB singer's formant) plus the angelic pair:
#            AIR — a +2.5 dB shelf-equivalent above ~8 kHz in the
#            envelope — and BREATH — aperiodicity lifted up to +0.14 in
#            the same band, ramped in 150 ms into each voiced run, so it
#            lives on held vowels and never on consonants.
#   8ve-a/b  the self-choir: the same slice with the corrected contour
#            doubled (f0 × 2 — cheaptrick envelope untouched, so it is
#            her head-voice, not a chipmunk), detuned +6 / −7 cents,
#            darker (power tilt above ~5.5 kHz), half again breathier,
#            and VOWELS ONLY — unvoiced regions are muted, not
#            composited, so stacked consonants can't smear. The halo is
#            a halo, not a duet.
#   3rd/5th  diatonic self-harmony: each frame's corrected f0 moved up
#            two (or four) A#-natural-minor scale degrees — the delta
#            computed per frame and smoothed 60 ms, so the harmony
#            tracks her residual rubato instead of a fixed interval.
#            Same vowels-only, darker, breathier treatment.
#   arp-*    the harp made of Camille: steady vowel excerpts ("oh" from
#            stone, "ah" from pass) re-sung FLAT at single grid tones
#            across A# minor from st+12 to st+27 (A#4 region), 12 ¢ / 5.2
#            Hz vibrato fading in after 150 ms — dotArp()'s move
#            (cult render10) with WORLD notes instead of dot takes.
#
# One WORLD analysis per source slice, many synths. Tuning is v2's
# option (b), unchanged: the grid is A# natural minor at TONIC = 237 Hz
# (+30 ¢ over A440); the band never moves.
#
# Reads samples/<name>.wav → writes vox3/*.wav + vox3/.manifest.json.
#
#   pop/.venv/bin/python pop/loner/bin/halo.py

import json, os
import numpy as np
import soundfile as sf
import pyworld as pw
import librosa

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
VOX3 = os.path.join(LANE, "vox3")
os.makedirs(VOX3, exist_ok=True)

TONIC = 237.0
MINOR = np.array([0, 2, 3, 5, 7, 8, 10])
FRAME_MS = 5.0
FLOOR = 140.0
SNAP_STRENGTH = 0.70
SMOOTH_MS = 45.0
FORMANT_DB = 1.6
AIR_DB = 2.5           # shelf-equivalent above ~8 kHz, in the envelope
BREATH = 0.14          # max aperiodicity lift on held vowels, same band
HALO_DARK_HZ = 5500.0
HALO_BREATH_X = 1.5

# Camille only — @jeffrey: "no alex voice — this track is all solo
# camille". The o-/s- takes and every spoken aside are out.
LEADS = [
    "f-sitting-curled", "f-think-stone", "f-i-think", "f-of-a-stone",
    "f-stone", "f-waiting-patiently", "f-for-time-to-pass", "f-pass",
    "f-whole-line",
    "n-getting-curled", "n-stone-waiting", "n-of-a-stone", "n-for-time-to-pass",
]
# interval halos beyond the octave pair (name → list of degree shifts)
EXTRA = {
    "f-sitting-curled":    [4],        # the 5th swells under "myself…"
    "f-think-stone":       [2],        # the 3rd under "stone…"
    "f-waiting-patiently": [4],        # "patiently…"
    "f-for-time-to-pass":  [4],        # "pass…"
    "f-of-a-stone":        [2],
    "n-stone-waiting":     [2],
    "f-whole-line":        [4],        # BREATHE's one interval, faint
}
# the harp: vowel source (slice, offset s, length s) and the note set
ARPS = {
    "oh": ("f-stone", 1.55, 0.42),     # the steady middle of "stone"
    "ah": ("f-pass", 0.45, 0.42),      # the open vowel of "pass"
}
ARP_STS = [12, 14, 15, 17, 19, 20, 22, 24, 26, 27]


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


def diatonic_delta(hz, degrees):
    """Cents to add to move each frame UP `degrees` scale degrees."""
    cents = 1200.0 * np.log2(hz / TONIC)
    # nearest grid index across octaves
    steps = np.concatenate([MINOR + 12 * o for o in range(-2, 5)])
    idx = np.argmin(np.abs(cents[:, None] - steps[None, :] * 100.0), axis=1)
    return (steps[np.minimum(idx + degrees, len(steps) - 1)] - steps[idx]) * 100.0


def run_depth(voiced):
    """Seconds into the current voiced run, per frame."""
    d = np.zeros(len(voiced))
    acc = 0.0
    for i, v in enumerate(voiced):
        acc = acc + FRAME_MS / 1000.0 if v else 0.0
        d[i] = acc
    return d


def analyze(path):
    x, fs = sf.read(path, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)
    f0_raw, t = pw.harvest(x, fs, f0_floor=FLOOR, f0_ceil=600.0, frame_period=FRAME_MS)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft_size = pw.get_cheaptrick_fft_size(fs, f0_floor=FLOOR)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft_size, f0_floor=FLOOR)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft_size)
    voiced = f0 > 0
    corr = np.zeros_like(f0)
    if voiced.any():
        corr[voiced] = -cents_to_grid(f0[voiced]) * SNAP_STRENGTH
    corr = smooth(corr, int(SMOOTH_MS / FRAME_MS))
    f0c = np.where(voiced, f0 * 2.0 ** (corr / 1200.0), 0.0)
    return dict(x=x, fs=fs, f0=f0, f0c=f0c, sp=sp, ap=ap, voiced=voiced,
                fft_size=fft_size, depth=run_depth(voiced))


def shelf(freqs, centre, width):
    return 1.0 / (1.0 + np.exp(-(freqs - centre) / width))


def synth(a, f0_new, *, dark=None, breath_x=1.0, vowels_only=False,
          formant=True, air=True):
    fs, x = a["fs"], a["x"]
    voiced = a["voiced"]
    freqs = np.linspace(0.0, fs / 2.0, a["sp"].shape[1])
    sp = a["sp"]
    if formant:
        lift = FORMANT_DB * np.exp(-((freqs - 2800.0) / 450.0) ** 2)
        sp = sp * (10.0 ** (lift / 10.0))[None, :]
    if air:
        sp = sp * (10.0 ** (AIR_DB * shelf(freqs, 8000.0, 900.0) / 10.0))[None, :]
    if dark:
        sp = sp * (1.0 / (1.0 + (freqs / dark) ** 2))[None, :]
    # breath on held vowels only: ramp in 150 ms into each voiced run
    w = np.clip((a["depth"] - 0.15) / 0.25, 0.0, 1.0)
    ap = np.minimum(1.0, a["ap"] + (BREATH * breath_x)
                    * w[:, None] * shelf(freqs, 8000.0, 800.0)[None, :])
    # continuous f0 into the synth
    if voiced.sum() >= 2:
        vi = np.where(voiced)[0]
        f0s = np.exp(np.interp(np.arange(len(f0_new)), vi,
                               np.log(np.maximum(f0_new[vi], 1e-6))))
    else:
        f0s = np.maximum(f0_new, 1e-6)
    y = pw.synthesize(f0s, np.ascontiguousarray(sp), np.ascontiguousarray(ap),
                      fs, frame_period=FRAME_MS)
    # v/uv back in the time domain
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
    if vowels_only:
        out = mask * y[:n]                       # halo: vowels, silence between
    else:
        out = mask * y[:n] + (1.0 - mask) * x[:n]  # lead: her consonants, real
    peak = np.max(np.abs(out)) or 1.0
    out = out * (0.90 / peak)
    tip = int(0.004 * fs)
    wnd = 0.5 - 0.5 * np.cos(np.pi * np.arange(tip) / tip)
    out[:tip] *= wnd
    out[-tip:] *= wnd[::-1]
    return out.astype(np.float32), fs


manifest = {}
for name in LEADS:
    src = os.path.join(LANE, "samples", f"{name}.wav")
    if not os.path.exists(src):
        print(f"  ! missing {src} — run bin/slice.mjs first")
        continue
    a = analyze(src)
    renders = {}

    out, fs = synth(a, a["f0c"])
    sf.write(os.path.join(VOX3, f"{name}.wav"), out, fs)
    renders["lead"] = round(len(out) / fs, 3)

    for tag, cents in (("8ve-a", 1200 + 6), ("8ve-b", 1200 - 7)):
        f0h = np.where(a["voiced"], a["f0c"] * 2.0 ** (cents / 1200.0), 0.0)
        out, fs = synth(a, f0h, dark=HALO_DARK_HZ, breath_x=HALO_BREATH_X,
                        vowels_only=True, air=False)
        sf.write(os.path.join(VOX3, f"{name}-{tag}.wav"), out, fs)
        renders[tag] = round(len(out) / fs, 3)

    for deg in EXTRA.get(name, []):
        tag = "3rd" if deg == 2 else "5th"
        delta = np.zeros_like(a["f0c"])
        if a["voiced"].any():
            delta[a["voiced"]] = diatonic_delta(a["f0c"][a["voiced"]], deg)
        delta = smooth(delta, int(60.0 / FRAME_MS))
        det = 5.0 if deg == 2 else -6.0
        f0h = np.where(a["voiced"], a["f0c"] * 2.0 ** ((delta + det) / 1200.0), 0.0)
        out, fs = synth(a, f0h, dark=HALO_DARK_HZ, breath_x=HALO_BREATH_X,
                        vowels_only=True, air=False)
        sf.write(os.path.join(VOX3, f"{name}-{tag}.wav"), out, fs)
        renders[tag] = round(len(out) / fs, 3)

    manifest[name] = renders
    print(f"  {name:22s} {' · '.join(renders)}")

# ── the harp ──────────────────────────────────────────────────────────
for vowel, (srcname, off, dur) in ARPS.items():
    path = os.path.join(LANE, "samples", f"{srcname}.wav")
    x, fs = sf.read(path, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)
    seg = x[int(off * fs):int((off + dur) * fs)].copy()
    tip = int(0.008 * fs)
    wnd = 0.5 - 0.5 * np.cos(np.pi * np.arange(tip) / tip)
    seg[:tip] *= wnd
    seg[-tip:] *= wnd[::-1]
    tmp = os.path.join(VOX3, ".arpsrc.wav")
    sf.write(tmp, seg, fs)
    a = analyze(tmp)
    tsec = np.arange(len(a["f0c"])) * FRAME_MS / 1000.0
    vib = 2.0 ** (0.12 * np.clip((tsec - 0.15) / 0.2, 0, 1)
                  * np.sin(2 * np.pi * 5.2 * tsec) / 12.0)
    for st in ARP_STS:
        f0f = np.where(a["voiced"], TONIC * 2.0 ** (st / 12.0) * vib, 0.0)
        out, _ = synth(a, f0f, vowels_only=True)
        sf.write(os.path.join(VOX3, f"arp-{vowel}-{st}.wav"), out, fs)
    manifest[f"arp-{vowel}"] = dict(source=srcname, off=off, dur=dur, sts=ARP_STS)
    print(f"  arp-{vowel:20s} {len(ARP_STS)} notes ← {srcname}")
    os.remove(tmp)

json.dump(manifest, open(os.path.join(VOX3, ".manifest.json"), "w"), indent=1)
print(f"WROTE {VOX3}/.manifest.json")
