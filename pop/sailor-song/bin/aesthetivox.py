#!/usr/bin/env python3
"""aesthetivox.py — the niece's Sailor Song vocal through the WORLD chain.

House rule (cult -> loner -> flwe -> xpld -> here): no lead vocal ships raw.
This lane FOLLOWS her — no time warp, no duration control — so this is
loner's regulation (pitch only, timing untouched) pushed sharper, with
cult/sing.py's singer's formant + de-ess and the macneopolitan chorus's
15 ms retune smoothing.

  THE STEM       Demucs vocals (pop/samples/sailor-song-take/stems/...),
                 resampled to 48 k so it stays sample-aligned with the take.
  THE FRAME      the grid is G# natural minor (= B major) in the GUITAR's
                 frame, +13 c over A440 (take.analysis.json tuningCents).
                 Her voice is corrected toward her own guitar, not A440, so
                 voice, guitar and the sine beds all agree.
  THE NOTE       per-frame nearest scale tone with HYSTERESIS: the held note
                 only changes when the voice is clearly nearer the next one
                 (by HYST semitones). That is what keeps a hard snap from
                 trilling between neighbours — the sharpness without the
                 Eiffel-65 artifact.
  THE SNAP       SNAP 1.0, correction smoothed over SMOOTH_MS = 15 ms. A
                 trace of her vibrato (VIB_KEEP) rides on top so held notes
                 don't freeze. Snap fades out where the pitch is genuinely
                 sliding (|slope| > SLIDE_ST_PER_S) — a slide is not out of tune.
  THE COMPOSITE  voiced = WORLD render; unvoiced = the original stem
                 (5 ms cosine seams), so consonants stay hers.

Writes:
  src/vox/vocals-aesthetivox.wav   tuned lead, same length/timing as the take
  src/vox/vocals-dry-48k.wav       the untuned stem at 48 k (for A/B)
  vox-notes.json                   held-note segmentation (for sine harmonies)
                                   + pyin before/after grid receipt

  pop/.venv/bin/python pop/sailor-song/bin/aesthetivox.py [--snap 1.0] [--smooth-ms 15]
"""
import argparse, json, os, subprocess
import numpy as np
import soundfile as sf
import pyworld as pw
import librosa
from scipy.signal import butter, sosfilt

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
POP = os.path.dirname(LANE)
STEM = os.path.join(POP, "samples/sailor-song-take/stems/htdemucs/vocals.wav")
VOX = os.path.join(LANE, "src/vox")
FS = 48000
FRAME_MS = 5.0

ap = argparse.ArgumentParser()
ap.add_argument("--snap", type=float, default=1.0)
ap.add_argument("--smooth-ms", type=float, default=50.0)   # v1 15 ms read glitchy, 35 still skippy
ap.add_argument("--hyst", type=float, default=0.45)       # semitones
ap.add_argument("--vib-keep", type=float, default=0.30)
ap.add_argument("--formant-db", type=float, default=-2.5)   # a DIP: v1's +1.6 lift read screamy
ap.add_argument("--slide", type=float, default=40.0)      # st/s; at 14 the raw contour leaked through as blips
args = ap.parse_args()

A = json.load(open(os.path.join(LANE, "take.analysis.json")))
TUNE = A["tuningCents"] / 100.0                           # +0.13 semitone
MINOR_GS = [8, 10, 11, 1, 3, 4, 6]                        # G# A# B C# D# E F#
NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

os.makedirs(VOX, exist_ok=True)
dry = os.path.join(VOX, "vocals-dry-48k.wav")
if os.path.exists(STEM):   # the Demucs stem; on a machine without pop/samples, reuse the dry stem already made from it
    subprocess.run(["ffmpeg", "-v", "error", "-y", "-i", STEM, "-ac", "1", "-ar", str(FS),
                    "-c:a", "pcm_f32le", dry], check=True)
x, fs = sf.read(dry, dtype="float64")


def hz_to_m(hz):   # midi in the guitar's frame (tuning offset removed)
    return 69 + 12 * np.log2(hz / 440.0) - TUNE


def m_to_hz(m):
    return 440.0 * 2 ** ((m + TUNE - 69) / 12)


def nearest_scale(m):
    base = 12 * np.floor(m / 12.0)
    opts = np.array([p + base + o for p in MINOR_GS for o in (-12, 0, 12)], float)
    return opts[np.argmin(np.abs(opts - m))]


def smooth(v, frames):
    if frames <= 1:
        return v
    k = np.hanning(frames * 2 + 1); k /= k.sum()
    return np.convolve(v, k, mode="same")


print(f"WORLD analysis of {len(x)/fs:.1f}s …")
f0, t = pw.harvest(x, fs, f0_floor=150.0, f0_ceil=900.0, frame_period=FRAME_MS)
f0 = pw.stonemask(x, f0, t, fs)
fft_size = pw.get_cheaptrick_fft_size(fs, f0_floor=150.0)
sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft_size, f0_floor=150.0)
apr = pw.d4c(x, f0, t, fs, fft_size=fft_size)
voiced = f0 > 0
# Demucs bleed gives WORLD short false-voiced blips; require ≥ 40 ms runs.
runs = np.diff(np.concatenate([[0], voiced.astype(int), [0]]))
for a, b in zip(np.where(runs == 1)[0], np.where(runs == -1)[0]):
    if (b - a) * FRAME_MS < 40:
        voiced[a:b] = False

# ── CLEAN THE TRACK ────────────────────────────────────────────────────
# v1 fed the raw harvest contour to a hard snap: 251 frame-to-frame jumps
# > 2 st in 30 s of chorus (guitar bleed, octave slips) each became an
# audible step. Now: median-filter, fold octave errors against a 150 ms
# running median, and drop what still sits a 4th+ away.
def nanmedfilt(v, k):
    out = np.full(len(v), np.nan)
    h = k // 2
    for i in range(len(v)):
        w = v[max(0, i - h):i + h + 1]
        w = w[np.isfinite(w)]
        if len(w):
            out[i] = np.median(w)
    return out


m = np.where(voiced, hz_to_m(np.maximum(f0, 1e-6)), np.nan)
m = nanmedfilt(m, 7)                                        # 35 ms spikes out
ref = nanmedfilt(m, 31)                                     # 155 ms context
off = m - ref
fold = np.isfinite(off) & (np.abs(off) > 7)
m[fold] -= 12 * np.round(off[fold] / 12)
bad = np.isfinite(m) & (np.abs(m - ref) > 4.5)
m[bad] = np.nan
voiced &= np.isfinite(m)

# ── CONSONANTS ─────────────────────────────────────────────────────────
# harvest calls much of a sung line "voiced" straight through s/t/k/h and
# breaths, and WORLD then buzzes them. A frame is a consonant (the
# original audio plays) when it is aperiodic in the 1–5 kHz band OR its
# energy leans above 4 kHz.
fq = np.linspace(0, fs / 2, sp.shape[1])
band = (fq > 1000) & (fq < 5000)
ap_band = apr[:, band].mean(axis=1)
hf = sp[:, fq > 4000].sum(axis=1) / (sp.sum(axis=1) + 1e-12)
sib = hf > max(0.12, np.percentile(hf[voiced], 90))          # s / sh / t bursts
noisy = ap_band > np.percentile(ap_band[voiced], 97)          # only the breathiest
cons = sib | noisy
cons = np.convolve(cons.astype(float), np.ones(3) / 3, mode="same") > 0.5
sung = voiced & ~cons


def runs_of(b):
    d = np.diff(np.concatenate([[0], b.astype(int), [0]]))
    return zip(np.where(d == 1)[0], np.where(d == -1)[0])


# v3 read "skippy": airy vowels flagged as consonants dropped to her
# UNTUNED original for 5–60 ms slivers (~4 switches/s). Close short gaps
# unless they are real sibilants; drop short tuned islands.
for a, b in runs_of(~sung):
    if 0 < a and b < len(sung) and (b - a) * FRAME_MS < 70 and not sib[a:b].any():
        sung[a:b] = voiced[a:b] | True
for a, b in runs_of(sung):
    if (b - a) * FRAME_MS < 60:
        sung[a:b] = False
sung &= voiced | np.roll(voiced, 1) | np.roll(voiced, -1)
sw = np.abs(np.diff(sung.astype(int))).sum() / (len(sung) * FRAME_MS / 1000)
print(f"switches/sec {sw:.2f}")
print(f"voiced {voiced.mean():.0%}  consonant-in-voiced {(voiced & cons).sum() / max(1, voiced.sum()):.0%}")

# ── THE NOTE: hysteresis + minimum note length ─────────────────────────
ms = smooth(np.where(np.isfinite(m), m, 0), 3)
ms = np.where(np.isfinite(m), ms / np.maximum(smooth(np.isfinite(m).astype(float), 3), 1e-6), np.nan)
target = np.full(len(m), np.nan)
cur = None
for i in range(len(m)):
    if not voiced[i]:
        cur = None
        continue
    s_ = nearest_scale(ms[i])
    if cur is None or (s_ != cur and abs(ms[i] - s_) < abs(ms[i] - cur) - args.hyst):
        cur = s_
    target[i] = cur
MIN_NOTE = int(110 / FRAME_MS)                             # a note must hold 110 ms
i = 0
while i < len(target):
    if not np.isfinite(target[i]):
        i += 1; continue
    j = i
    while j < len(target) and target[j] == target[i]:
        j += 1
    if j - i < MIN_NOTE and i > 0 and np.isfinite(target[i - 1]):
        target[i:j] = target[i - 1]
    i = j

# Ornament flatten: a trip to a neighbour and straight back (A→B→A under
# 160 ms) is her decoration, not a new note — a hard snap turns it into a
# stair-step blip (measured: 66 in 30 s of chorus). Hold A through it.
TRIP = int(160 / FRAME_MS)
for _ in range(2):
    i = 0
    while i < len(target):
        if not np.isfinite(target[i]):
            i += 1; continue
        j = i
        while j < len(target) and target[j] == target[i]:
            j += 1
        if (j - i) < TRIP and i > 0 and j < len(target) and np.isfinite(target[i - 1]) \
                and np.isfinite(target[j]) and target[i - 1] == target[j]:
            target[i:j] = target[j]
        i = j

# ── THE SNAP: output = the note, plus a trace of her own movement ──────
vi = np.where(voiced)[0]
tgt_c = np.interp(np.arange(len(m)), vi, target[vi])
msc = np.interp(np.arange(len(m)), vi, ms[vi])
slope = np.abs(np.gradient(smooth(msc, 4))) / (FRAME_MS / 1000.0)
slide_w = np.clip(1.0 - (slope - args.slide) / args.slide, 0.0, 1.0)
vib = msc - smooth(msc, int(120 / FRAME_MS))
tuned = smooth(tgt_c, int(args.smooth_ms / FRAME_MS)) + vib * args.vib_keep
w = args.snap * slide_w
m_new = w * tuned + (1 - w) * msc
m_new = np.where(voiced, m_new, np.nan)

# continuous f0 through gaps (WORLD pops on 0→target jumps)
vi = np.where(voiced)[0]
f0_synth = m_to_hz(np.interp(np.arange(len(m)), vi, m_new[vi]))
f0_synth = np.where(np.isfinite(f0_synth), f0_synth, 200.0)
np.savez(os.environ.get('AVOX_DEBUG', '/dev/null.npz'), f0=f0_synth, sung=sung, voiced=voiced, target=target, ms=ms) if os.environ.get('AVOX_DEBUG') else None

# presence: a broad dip around 3 kHz, not cult/sing.py's lift — her belts are already bright
freqs = np.linspace(0.0, fs / 2.0, sp.shape[1])
sp = sp * (10.0 ** (args.formant_db * np.exp(-((freqs - 3000.0) / 1100.0) ** 2) / 10.0))[None, :]
print("resynthesizing …")
y = pw.synthesize(f0_synth, sp, apr, fs, frame_period=FRAME_MS)

# ── THE COMPOSITE ───────────────────────────────────────────────────────
spf = int(round(fs * FRAME_MS / 1000.0))
n = len(x)
mask = np.repeat(sung.astype(np.float64), spf)
mask = np.pad(mask, (0, max(0, n - len(mask))), mode="edge")[:n]
y = np.pad(y, (0, max(0, n - len(y))))[:n]
ramp = int(0.012 * fs)                   # 12 ms seams (v1: 5 ms clicked)
edges = np.diff(mask.astype(np.int8))
for idx in np.where(edges == 1)[0]:
    k = np.arange(min(ramp, n - idx - 1))
    mask[idx + 1 + k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
for idx in np.where(edges == -1)[0]:
    k = np.arange(min(ramp, idx + 1))
    mask[idx - k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
# WORLD renders slightly hotter than the stem; match voiced RMS first.
vr = np.sqrt(np.mean((y * mask) ** 2) + 1e-12) / np.sqrt(np.mean((x * mask) ** 2) + 1e-12)
out = mask * y / vr + (1.0 - mask) * x


def deess(v, thresh=0.05, ratio=0.45):
    band = sosfilt(butter(2, [5000, 9000], btype="band", fs=fs, output="sos"), v)
    env = np.abs(band)
    a = np.exp(-1.0 / (0.004 * fs))
    for i in range(1, len(env)):
        env[i] = max(env[i], a * env[i - 1])
    g = 1.0 / (1.0 + ratio * np.clip(env / thresh - 1.0, 0.0, None))
    return v - band * (1.0 - g)


out = deess(out)


def tame(v, lo=2000, hi=5000, thresh=0.04, ratio=0.8):
    """The scream tamer: de-ess's shape one band down. Only when she belts
    does 2–5 kHz get pulled back; quiet lines keep their air."""
    band = sosfilt(butter(2, [lo, hi], btype="band", fs=fs, output="sos"), v)
    env = np.abs(band)
    a = np.exp(-1.0 / (0.010 * fs))
    for i in range(1, len(env)):
        env[i] = max(env[i], a * env[i - 1])
    g = 1.0 / (1.0 + ratio * np.clip(env / thresh - 1.0, 0.0, None))
    return v - band * (1.0 - g)


def level(v, win_ms=300, strength=0.55):
    """Slow RMS leveling: loud phrases come down toward the median, soft
    ones stay put. strength 0 = off, 1 = flat."""
    w = int(win_ms / 1000 * fs)
    rms = np.sqrt(np.convolve(v ** 2, np.ones(w) / w, mode="same") + 1e-10)
    ref = np.median(rms[rms > np.percentile(rms, 40)])
    g = np.where(rms > ref, (ref / rms) ** strength, 1.0)
    return v * smooth(g, 20)


out = level(tame(out))
# vowels-only octave halo (xpld's move): the sung frames, an octave up,
# dark and soft — written as its own stem so the mix sets its level.
f0_halo = f0_synth * 2.0
sp_dark = sp * (1.0 / (1.0 + (freqs / 2500.0) ** 4))[None, :]
halo = pw.synthesize(f0_halo, sp_dark, apr, fs, frame_period=FRAME_MS)
halo = np.pad(halo, (0, max(0, n - len(halo))))[:n] * mask
halo *= 0.9 / (np.max(np.abs(halo)) + 1e-9)
sf.write(os.path.join(VOX, "vocals-halo.wav"), halo.astype(np.float32), fs)

# ── HER OWN HARMONIES ──────────────────────────────────────────────────
# Each stem is her tuned line moved k steps along G# minor — the same
# vibrato, slides and envelope, a different scale degree. Formants stay
# put (cheaptrick's envelope untouched) so it is her voice, not a
# chipmunk; vowels only (consonants live in the lead). The arrangement
# (render.mjs) decides which interval sings where.
SC = sorted(MINOR_GS)


def step(midi, k):
    pc, octv = int(round(midi)) % 12, int(round(midi)) // 12
    if pc not in SC:
        return midi + {2: 3, -2: -4, 4: 7, -5: -8, -7: -12}.get(k, 0)
    i = SC.index(pc) + k
    return octv * 12 + SC[i % 7] + 12 * (i // 7) + (midi - round(midi))


dark = sp * (1.0 / (1.0 + (freqs / 5500.0) ** 4))[None, :]
m_syn = hz_to_m(f0_synth)
HARM = {"up3": 2, "down3": -2, "up5": 4, "down6": -5, "down8": -7, "up8": 7}   # v6.2: up8 — her voice an octave up, for the moments it should fly
for name, k in HARM.items():
    shifted = np.array([step(t_, k) for t_ in tgt_c]) - tgt_c    # per-frame interval
    shifted = smooth(shifted, int(40 / FRAME_MS))                 # interval changes glide
    hy = pw.synthesize(m_to_hz(m_syn + shifted), dark, apr, fs, frame_period=FRAME_MS)
    hy = np.pad(hy, (0, max(0, n - len(hy))))[:n] * mask
    hy *= 0.9 / (np.max(np.abs(hy)) + 1e-9)
    sf.write(os.path.join(VOX, f"harm-{name}.wav"), hy.astype(np.float32), fs)
    print(f"  harmony {name:6s} ({k:+d} steps)")
sf.write(os.path.join(VOX, "vocals-aesthetivox.wav"), out.astype(np.float32), fs)

# ── notes for the harmonies ─────────────────────────────────────────────
notes, a = [], None
for i in range(len(target) + 1):
    v = target[i] if i < len(target) else np.nan
    if a is not None and (not np.isfinite(v) or v != target[a]):
        dur = (i - a) * FRAME_MS / 1000
        if dur >= 0.12:
            notes.append({"t": round(a * FRAME_MS / 1000, 3), "dur": round(dur, 3),
                          "midi": int(target[a]), "note": NAMES[int(target[a]) % 12] + str(int(target[a]) // 12 - 1)})
        a = None
    if a is None and np.isfinite(v):
        a = i


# ── receipt: pyin |cents to grid| before/after ─────────────────────────
def grid_dev(sig):
    seg = sig[int(27 * fs):int(87 * fs)]      # a minute of singing
    seg = librosa.resample(seg.astype(np.float32), orig_sr=fs, target_sr=22050)
    p, vf, vp = librosa.pyin(seg, fmin=150, fmax=900, sr=22050, frame_length=2048)
    p = p[vf & (vp > 0.5)]
    p = p[np.isfinite(p)]
    mm = hz_to_m(p)
    return float(np.median(np.abs([mv - nearest_scale(mv) for mv in mm]))) * 100


before, after = grid_dev(x), grid_dev(out)
json.dump({"grid": "G# natural minor, +13c frame", "snap": args.snap,
           "smoothMs": args.smooth_ms, "hyst": args.hyst, "vibKeep": args.vib_keep,
           "centsToGrid": {"before": round(before, 1), "after": round(after, 1)},
           "notes": notes}, open(os.path.join(LANE, "vox-notes.json"), "w"), indent=1)
print(f"grid-dev (27–87s): {before:.1f}¢ → {after:.1f}¢   {len(notes)} held notes")
print(f"✓ {os.path.join(VOX, 'vocals-aesthetivox.wav')}")
