#!/usr/bin/env python3
"""
aesthetivox-scream.py — a scream take, extended and aesthetivoxed.

The aesthetivox shape applied to a scream: WORLD analysis, the take split
into voiced and unvoiced runs, the consonant runs left at their own length
and every voiced run lengthened by one common ratio (frame interpolation,
never tiled) until the line is as long as asked, and the pitch REPLACED
with absolute notes. The notes are an arpeggio over the chord under the
scream, stepped hard with no glide — sixteenths, then thirty-seconds — which
is what makes it warble like a modem handshake while the formants stay his.

A job with "mode": "wild" is a chaotic take (the chaos bank, or a lead in the
chaos delivery). Those have little steady voicing, so a plain pitch
replacement loses its footing; they are made the way wattajetta's
sing_whisper gives breath a pitch — the same mouth shapes synthesized twice,
once as the noise they were and once with the note as f0 and most of the
aperiodicity kept, then mixed — with "nat" of the take's own pitch movement
left on the note and, with "halo", a darker copy an octave up. "dur": null
keeps the take at its own length. Two strengths are made and measured (how
much of the note is heard as pitched, how far off it sits, how rough the
result is beside the raw take): the full one is used if its note holds, the
light one (the raw take with the tuned copy under it) if not, and the raw
take if the treatment took the roughness out. The verdict and the numbers go
beside the output as <out>.json.

  aesthetivox-scream.py <jobs.json>
  {"sr": 48000, "jobs": [{"in": wav, "out": f32, "dur": seconds | null,
     "step": seconds, "transpose": semitones, "mode"?: "wild", "nat"?: 0.4, "halo"?: 0.2,
     "segs": [[start seconds, [midi, ...]], ...]}]}
  aesthetivox-scream.py --reel <pairs.json> <out.mp3>
  [{"raw": wav, "treated": f32}, ...] → raw, then treated, for each in turn
"""
import json, os, sys, warnings
warnings.filterwarnings("ignore")
import numpy as np
import soundfile as sf
import pyworld as pw

FP = 5.0
FF = "/opt/homebrew/bin/ffmpeg"


def flatness(y, fs):
    """Roughness, as the chaos bank measures it: median spectral flatness of the loud part."""
    import librosa
    y = np.asarray(y, dtype=np.float32)
    rms = librosa.feature.rms(y=y, frame_length=4096, hop_length=1024)[0]
    flat = librosa.feature.spectral_flatness(y=y, n_fft=4096, hop_length=1024)[0]
    n = min(len(rms), len(flat))
    return float(np.median(flat[:n][rms[:n] > rms.max() * 0.15]))


def held(y, fs, f02):
    """How well a result carries its notes: of the frames given a note, the
    share harvest hears as pitched, the share within 50 cents of the note (any
    octave), and the median distance in cents."""
    f0, _ = pw.harvest(np.ascontiguousarray(y, dtype=np.float64), fs, f0_floor=60, f0_ceil=1100, frame_period=FP)
    n = min(len(f0), len(f02)); f0, want = f0[:n], f02[:n]
    idx = want > 0
    if not idx.any(): return dict(voiced=0.0, on=0.0, cents=0.0)
    got = f0[idx] > 0
    cents = 1200 * np.log2(f0[idx][got] / want[idx][got]) if got.any() else np.zeros(0)
    cents = (cents + 600) % 1200 - 600
    return dict(voiced=round(float(got.mean()), 2), on=round(float((np.abs(cents) < 50).sum() / idx.sum()), 2), cents=round(float(np.median(np.abs(cents))), 0) if len(cents) else 0.0)


def runs(mask):
    out, a = [], 0
    for i in range(1, len(mask) + 1):
        if i == len(mask) or mask[i] != mask[a]:
            out.append((a, i, bool(mask[a]))); a = i
    return out


def avox(j, sr):
    x, fs = sf.read(j["in"], dtype="float64")
    if x.ndim > 1:
        x = x.mean(1)
    f0, t = pw.harvest(x, fs, f0_floor=110, f0_ceil=800, frame_period=FP)
    f0 = pw.stonemask(x, f0, t, fs)
    sp = pw.cheaptrick(x, f0, t, fs, f0_floor=110)
    ap = pw.d4c(x, f0, t, fs)
    n = len(f0)
    hop = int(fs * FP / 1000)
    rms = np.array([np.sqrt(np.mean(x[i * hop:i * hop + hop * 2] ** 2) + 1e-12) for i in range(n)])
    loud = rms > rms.max() * 0.12
    voiced = f0 > 0
    forced = voiced.mean() < 0.35
    if forced:                       # a pure rasp: the loud frames carry the note instead
        voiced = loud
        ap = ap * 0.72
    else:                            # close holes under 30 ms, and keep loud rasp inside the line pitched
        for a, b, v in runs(voiced):
            if not v and (b - a) <= 6 and a > 0 and b < n:
                voiced[a:b] = True
        voiced |= loud & (np.convolve(voiced.astype(float), np.ones(41), "same") > 0)
    rs = runs(voiced)
    v_len = sum(b - a for a, b, v in rs if v)
    u_len = n - v_len
    wild = j.get("mode") == "wild"
    dur = j["dur"] if j.get("dur") else n * FP / 1000
    want = int(dur * 1000 / FP)
    # The words keep close to their own pace (voiced runs at most 1.3x); the
    # rest of the asked length is one long hold on the last real vowel, so
    # the line is heard before it is held.
    ratio = min(1.3, max(1.0, (want - u_len) / max(1, v_len))) if j.get("dur") else 1.0
    longs = [i for i, (a, b, v) in enumerate(rs) if v and b - a >= 16]
    last = longs[-1] if longs else max(range(len(rs)), key=lambda i: (rs[i][1] - rs[i][0]) * rs[i][2])
    body = sum(int(round((b - a) * ratio)) if v and b - a > 2 else b - a for a, b, v in rs)
    extra = max(0, want - body)
    src = []
    for i, (a, b, v) in enumerate(rs):
        if v and b - a > 2:
            n_out = int(round((b - a) * ratio))
            if i == last and extra:
                keep = min(8, (b - a) // 3)          # the vowel's own onset and release stay as spoken
                src.append(np.concatenate([np.linspace(a, a + keep, int(keep * ratio), endpoint=False),
                                           np.linspace(a + keep, b - 1 - keep, max(2, n_out + extra - 2 * int(keep * ratio))),
                                           np.linspace(b - 1 - keep, b - 1, int(keep * ratio))]))
            else:
                src.append(np.linspace(a, b - 1, n_out))
        else:
            src.append(np.arange(a, b, dtype=float))
    src = np.concatenate(src)
    lo = np.floor(src).astype(int); hi = np.minimum(lo + 1, n - 1); w = (src - lo)[:, None]
    sp2 = np.ascontiguousarray(sp[lo] * (1 - w) + sp[hi] * w)
    ap2 = np.ascontiguousarray(ap[lo] * (1 - w) + ap[hi] * w)
    v2 = voiced[np.round(src).astype(int)]
    k = np.arange(len(src)) * FP / 1000
    # the arpeggio: hard steps over the chord in force, sixteenths first, thirty-seconds after
    starts = np.array([s[0] for s in j["segs"]])
    step = j["step"]
    turn = dur * 0.4
    count = np.where(k < turn, np.floor(k / step), np.floor(turn / step) + np.floor((k - turn) / (step / 2)))
    f02 = np.zeros(len(src))
    for i in range(len(src)):
        notes = j["segs"][max(0, np.searchsorted(starts, k[i], side="right") - 1)][1]
        f02[i] = 440.0 * 2 ** ((notes[int(count[i]) % len(notes)] + j.get("transpose", 0) - 69) / 12)
    if wild:
        # the take's own pitch movement, part of it, rides on the note
        raw = f0[np.round(src).astype(int)]
        has = raw > 0
        if has.sum() > 3:
            st = np.zeros(len(src)); st[has] = 12 * np.log2(raw[has] / np.median(raw[has]))
            f02 = f02 * 2 ** (j.get("nat", 0.4) * np.clip(st, -3, 3) / 12)
    f02 = np.where(v2, f02, 0.0)
    if wild:
        return wild_out(j, x, fs, f02, sp2, ap2, f0[np.round(src).astype(int)] > 0, forced, ratio)
    y = pw.synthesize(f02, sp2, ap2, fs, FP)
    fade = int(0.06 * fs)
    y[-fade:] *= np.linspace(1, 0, fade)
    pk = np.abs(y).max()
    (y / pk if pk > 0 else y).astype(np.float32).tofile(j["out"])
    return dict(ratio=round(float(ratio), 2), seconds=round(len(y) / fs, 2), voiced=round(float(v2.mean()), 2), forced=bool(forced))


def wild_out(j, x, fs, f02, sp2, ap2, was_voiced, forced, ratio):
    pitched = f02 > 0
    # where the take had a pitch its own breathiness is kept; where it was rasp, most of the noise stays in the note
    ap_t = np.ascontiguousarray(np.where(was_voiced[:, None], ap2, np.minimum(ap2, 0.86)))
    tuned = pw.synthesize(f02, sp2, ap_t, fs, FP)
    breath = pw.synthesize(np.zeros(len(f02)), sp2, np.ascontiguousarray(np.clip(ap2 / (0.72 if forced else 1), 0, 1)), fs, FP)
    halo = j.get("halo", 0.0)
    up = pw.synthesize(f02 * 2, np.ascontiguousarray(np.where(pitched[:, None], sp2, sp2 * 1e-4)), np.ascontiguousarray(np.clip(ap2 + 0.1, 0, 0.95)), fs, FP) if halo else np.zeros(len(tuned))
    m = min(len(tuned), len(breath), len(up))
    same_length = not j.get("dur")
    if same_length: m = min(m, len(x))
    nz = lambda a: a[:m] / (np.sqrt(np.mean(a[:m] ** 2)) + 1e-9)
    rasp = float(1 - was_voiced[pitched].mean()) if pitched.any() else 1.0     # how much of the loud part had no pitch
    full = nz(tuned) + nz(breath) * (0.35 + 0.5 * rasp) + nz(up) * halo
    # the light one: the take itself (or, when it has been lengthened, its breath) with the note under it
    light = nz(x if same_length else breath) + nz(tuned) * 0.5 + nz(up) * halo * 0.6
    base = flatness(x, fs)
    M = {}
    for name, y in (("full", full), ("light", light)):
        M[name] = dict(held(y, fs, f02), flat=float(f"{flatness(y, fs):.3g}"))   # 3 figures, not 4 places: these run near 1e-5
    ok = lambda r: r["on"] >= 0.5 and r["cents"] <= 50
    choice = "full" if ok(M["full"]) else "light"
    if M[choice]["flat"] < 0.75 * base:
        choice = "light" if choice == "full" and M["light"]["flat"] >= 0.75 * base else "raw"
    y = dict(full=full, light=light, raw=x[:m] if same_length else light)[choice].copy()
    fade = int(0.06 * fs)
    y[-fade:] *= np.linspace(1, 0, fade)
    pk = np.abs(y).max()
    (y / pk if pk > 0 else y).astype(np.float32).tofile(j["out"])
    info = dict(choice=choice, raw_flat=float(f"{base:.3g}"), rasp=round(rasp, 2), forced=bool(forced), ratio=round(float(ratio), 2), seconds=round(len(y) / fs, 2), **M)
    json.dump(info, open(j["out"] + ".json", "w"))
    return info


if len(sys.argv) > 1 and sys.argv[1] == "--reel":
    import subprocess
    pairs, SR, out = json.load(open(sys.argv[2])), 48000, []
    dec = lambda p: np.fromfile(p, dtype=np.float32) if p.endswith(".f32") else np.frombuffer(subprocess.run([FF, "-v", "error", "-i", p, "-f", "f32le", "-ac", "1", "-ar", str(SR), "-"], capture_output=True, check=True).stdout, dtype=np.float32)
    at = 0.0
    for p in pairs:
        for k in ("raw", "treated"):
            y = dec(p[k]); y = y / (np.abs(y).max() + 1e-9) * 0.8
            print(f"  {int(at // 60)}:{at % 60:04.1f}  {k:7s} {p.get('label', '')}")
            out += [y, np.zeros(int((0.5 if k == "raw" else 1.1) * SR), np.float32)]; at += len(y) / SR + (0.5 if k == "raw" else 1.1)
    subprocess.run([FF, "-v", "error", "-y", "-f", "f32le", "-ar", str(SR), "-ac", "1", "-i", "-", "-b:a", "192k", sys.argv[3]], input=np.concatenate(out).astype(np.float32).tobytes(), check=True)
    print(sys.argv[3]); sys.exit(0)

spec = json.load(open(sys.argv[1]))
for j in spec["jobs"]:
    if os.path.exists(j["out"]):
        continue
    try:
        print(os.path.basename(j["out"]), avox(j, spec["sr"]), flush=True)
    except Exception as e:
        print("FAILED", os.path.basename(j["out"]), e, flush=True)
