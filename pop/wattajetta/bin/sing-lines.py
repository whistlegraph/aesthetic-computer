#!/usr/bin/env python3
"""
sing-lines.py — whole sung lines for wattajetta cutie.

Takes the spoken takes cutie-takes.mjs made (vault voice/jeffrey-phonemes/
phrases/cutie-<line>-<cadence>) and stretches each onto the notes the
renderer asks for, with aesthetina's sing_phrase: consonants stay at spoken
speed, the note lives on the vowel, the pitch is replaced with the tune.

A pinned inflection usually has several takes in the vault (cheeky, cheeky2,
…): the model reads the line differently each time. Every phrase is sung
from each of them and measured against its notes (how late each syllable
reaches its pitch, how far off it sits), and the take that fits that phrase
best is the one kept. A job may carry its own "pin" (the solo's loud lines).

The words have to be heard. For each line every inflection in the vault is
sung on the line's first job and read back by whisper.cpp. The four whisper
reads closest to the lyric are cast (<cache>/line-cast.json, reused), and the
song takes turns through them: a job's "turn" picks its inflection.

  sing-lines.py <jobs.json> [--force]
  jobs.json: {"cache": dir, "sr": 48000, "lines": {line: lyric}, "pin"?: {line: [cadence]},
              "jobs": [{"key", "line", "turn", "pin"?, "mode"?, "starts": [s], "midis": [m], "end_hold": s}]}
Writes <cache>/<key>.f32 (mono float32 at sr) and <cache>/<key>.json
({"pre": seconds of consonant before the first note}) for each job not there.
"""
import difflib, importlib.util, json, os, re, subprocess, sys, tempfile, warnings
warnings.filterwarnings("ignore")
from pathlib import Path
import numpy as np
import pyworld as pw
from scipy.signal import resample_poly

HERE = Path(__file__).resolve().parent
jobs_file = sys.argv[1]
FORCE = "--force" in sys.argv   # sing every job again, over what is cached
J = json.load(open(jobs_file))
CACHE, SR = Path(J["cache"]), J["sr"]

# aesthetina's singer, imported as a library (it reads --arr at import)
sys.argv = ["voice.py", "--arr=radio"]
spec = importlib.util.spec_from_file_location("avoice", HERE.parents[1] / "aesthetina/bin/voice.py")
V = importlib.util.module_from_spec(spec); spec.loader.exec_module(V)

PHRASES = V.LIB.parents[1] / "jeffrey-phonemes" / "phrases"

# cutie wants each syllable to land on its note at once. aesthetina's singer
# slides into every note (85 ms and up) and keeps much of the speaker's own
# pitch movement; here the slide is 40 ms. The function is taken as written
# and that one line changed, so aesthetina's own file stays as it is.
import inspect
_src = inspect.getsource(V.sing_phrase)
_slide = "glide = 0.085 + 0.016 * abs(m - prev_m)"
assert _slide in _src, "aesthetina's sing_phrase changed: the slide line is gone"
exec(_src.replace(_slide, "glide = 0.04").replace("def sing_phrase(", "def sing_phrase_tight("), V.__dict__)
MODEL = os.environ.get("WHISPER_MODEL", str(Path.home() / "Models/ggml-large-v3-turbo-q5_0.bin"))
FFMPEG = "/opt/homebrew/bin/ffmpeg"
norm = lambda s: re.sub(r"[^a-z ]", "", s.lower().replace("-", "")).split()


def vowel_onsets(slug):
    """Move each syllable's start to its vowel. The singer puts the first voiced
    frame of a syllable on the note, but a syllable that opens on a stop or an
    affricate (jet, cu, tax, put) often has a few voiced frames first: the
    voice bar of the closure, or the end of the vowel before. Then the note
    began there and the vowel came 200-390 ms late (measured 2026-11-20).
    The vowel is taken to begin after the syllable's opening consonant."""
    A = V.phrase_analysis("jeffrey", slug)
    if A.get("onsets_fixed"): return
    v, n, fp = A["v"], A["n"], V.FP
    out = []
    for a, b in A["syl"]:
        b = min(b, n - 1); first = None
        # the last unvoiced run that begins in the first 55 % of the syllable and
        # still has 40 ms of voice after it: that is the opening consonant, and
        # the vowel begins where it ends. (A closing consonant comes later in
        # the syllable, or has no voice after it, and is left alone.)
        k = a
        while k <= b:
            if not v[k]:
                e = k
                while e <= b and not v[e]: e += 1
                after = 0
                while e + after <= b and v[e + after]: after += 1
                if (k - a) <= 0.55 * (b - a + 1) and (e - k) * fp >= 15 and after * fp >= 40: first = e
                k = e
            else: k += 1
        if first is None: first = next((f for f in range(a, b + 1) if v[f]), a)
        out.append((min(first, b), b))
    A["syl"] = out; A["onsets_fixed"] = True


def even(y, pre, job):
    """A lullaby is even. Spoken, the small words (a, the) sit 15-25 dB under
    the stressed ones and vanish in a song. Each syllable is brought most of
    the way to the line's median level (never more than +12 dB, and a loud one
    comes down), with the gain moving over 40 ms so nothing steps."""
    ends = job["starts"][1:] + [job["starts"][-1] + job["end_hold"]]
    slots = [(int((s + pre) * SR), min(len(y), int((e + pre) * SR))) for s, e in zip(job["starts"], ends)]
    rms = [np.sqrt(np.mean(y[a:b] ** 2)) + 1e-9 if b > a else 1e-9 for a, b in slots]
    med = np.median(rms)
    g = np.ones(len(y))
    for (a, b), r in zip(slots, rms): g[a:b] = np.clip((med / r) ** 0.75, 0.5, 4.0)
    w = int(0.04 * SR); g = np.convolve(np.pad(g, w, mode="edge"), np.hanning(2 * w + 1) / np.hanning(2 * w + 1).sum(), mode="valid")[:len(y)]
    y = y * g
    return (y / (np.abs(y).max() + 1e-9)).astype(np.float32)


def sing_whisper(job, cadence):
    """A whisper has no pitch for the singer to move, so it gets the tune a
    different way. Each syllable is stretched evenly across its note (there is
    no vowel to hold, the breath itself is lengthened), and the result is made
    twice from the same mouth shapes: once as the breath it was, once with the
    note as its pitch and most of the breath kept in it. The two together are
    a whisper that sings: the words stay air, and the tune is in the air.
    A darker copy an octave up sits over the pitched one, as in the aesthetivox."""
    A = V.phrase_analysis("jeffrey", f"cutie-{job['line']}-{cadence}")
    n, syl, fp = A["n"], A["syl"], V.FP
    k = len(job["midis"])
    on = [min(a, n - 1) for a, _ in syl[:k]] + [min(n - 1, syl[min(k, len(syl)) - 1][1] + 8)]
    slots = [job["starts"][i + 1] - job["starts"][i] for i in range(k - 1)] + [job["end_hold"]]
    pos, f0, prev = [], [], job["midis"][0]
    for i, L in enumerate(slots):
        T = max(2, int(round(L * 1000 / fp)))
        for q, src in enumerate(np.linspace(on[i], max(on[i] + 1, on[i + 1]) - 1, T)):
            x = min(1.0, q * fp / 1000 / 0.06); m = prev + (job["midis"][i] - prev) * (x * x * (3 - 2 * x))
            pos.append(src); f0.append(440 * 2 ** ((m - 69) / 12))
        prev = job["midis"][i]
    pos = np.clip(np.array(pos, float), 0, n - 1.001); f0 = np.array(f0)
    lo = np.floor(pos).astype(int); hi = np.minimum(lo + 1, n - 1); w = (pos - lo)[:, None]
    sp = np.ascontiguousarray(np.exp(np.log(A["sp"][lo] + 1e-16) * (1 - w) + np.log(A["sp"][hi] + 1e-16) * w))
    ap = np.ascontiguousarray(np.clip(A["ap"][lo] * (1 - w) + A["ap"][hi] * w, 0, 1))
    breath = pw.synthesize(np.zeros(len(f0)), sp, ap, V.PFS, fp)
    tuned = pw.synthesize(f0, sp, np.ascontiguousarray(np.minimum(ap, 0.86)), V.PFS, fp)
    halo = pw.synthesize(f0 * 2, sp, np.ascontiguousarray(np.minimum(ap + 0.1, 0.95)), V.PFS, fp)
    m = min(len(breath), len(tuned), len(halo))
    nz = lambda a: a[:m] / (np.sqrt(np.mean(a[:m] ** 2)) + 1e-9)
    y = nz(breath) * 0.8 + nz(tuned) * 0.7 + nz(halo) * 0.22
    tail = int(0.12 * V.PFS); y[-tail:] *= np.linspace(1, 0, tail)
    y = resample_poly(y / (np.abs(y).max() + 1e-9), SR, V.PFS)
    return 0.0, y.astype(np.float32)


def sing(job, cadence):
    slug = f"cutie-{job['line']}-{cadence}"
    vowel_onsets(slug)
    if job.get("mode") == "angel":
        # the first verse: a person, not a machine (@jeffrey 2026-11-20: "a bit whistley angelic … less robot scary").
        # aesthetina's singer as written — it slides into every note (85 ms and up) — with half of his own pitch
        # movement left on the note, more vibrato, and a quiet copy an octave up for the halo
        pre, y = V.sing_phrase("jeffrey", slug, job["starts"], job["midis"], job["end_hold"], halo=0.08, vib=0.08, nat=0.5)
    else:
        pre, y = V.sing_phrase_tight("jeffrey", slug, job["starts"], job["midis"], job["end_hold"], halo=0.0, vib=0.03, nat=0.15)
    return pre, even(resample_poly(y, SR, V.PFS), pre, job)


def angel(y):
    """mode "angel", after the singing: nothing under 120 Hz, a shelf of air from 7 kHz, no saturation — and the
    whistle: his own sung pitch followed two octaves up by a soft sine with a little breath around it (noise in a
    band an octave wide on the same pitch), as loud as the voice is at that moment and 22 dB under it (at 17, with more room, whisper lost the words)."""
    from scipy.signal import butter, sosfilt
    x = sosfilt(butter(2, 120, "highpass", fs=SR, output="sos"), y.astype(np.float64))
    x = x + 0.8 * sosfilt(butter(1, 7000, "highpass", fs=SR, output="sos"), x)
    d = resample_poly(x, 1, 3); fs = SR // 3
    f0, t = pw.harvest(d, fs, f0_floor=80, f0_ceil=700, frame_period=5.0)
    v = f0 > 0
    if v.sum() > 10:
        f0i = np.interp(np.arange(len(f0)), np.where(v)[0], f0[v])                       # carried across the consonants
        f0i = np.convolve(np.pad(f0i, 8, mode="edge"), np.hanning(17) / np.hanning(17).sum(), "valid")
        n = len(x); tt = np.arange(n) / SR
        fx = np.interp(tt, t, f0i) * 4 * (1 + 0.004 * np.sin(2 * np.pi * 5.3 * tt))
        amp = np.interp(tt, t, np.convolve(v.astype(float), np.ones(15) / 15, "same"))
        env = np.sqrt(np.convolve(x ** 2, np.ones(2400) / 2400, "same")); ph = 2 * np.pi * np.cumsum(fx) / SR
        rng = np.random.default_rng(3); noise = rng.standard_normal(n)
        breath = sosfilt(butter(2, [900, 3200], "bandpass", fs=SR, output="sos"), noise) * 0.25
        x = x + 10 ** (-22 / 20) * np.sqrt(2) * env * amp * (np.sin(ph) + breath)
    return (x / (np.abs(x).max() + 1e-9)).astype(np.float32)


def close_mic(y, clear=False):
    """mode "close": the line sung at the microphone (@jeffrey 2026-11-20: "closer to mic", "more
    overtones", "deeper harmonics"). Nothing under 80 Hz (an octave double keeps its harmonics and
    leaves the bass alone); a proximity lift around 200 Hz; the level held tighter (3:1 over the
    line's own middle, 8 ms / 90 ms); an exciter (the band over 1.2 kHz driven into a tanh, and
    what that makes above 2.5 kHz laid back in) over a gentle tanh on the whole voice; and a
    little more of the consonants and air above 3.5 kHz."""
    from scipy.signal import butter, sosfilt, lfilter
    f = lambda kind, hz, order=2: butter(order, hz, kind, fs=SR, output="sos")
    x = sosfilt(f("highpass", 80), y.astype(np.float64))
    # mode "clear" (the lead everywhere; @jeffrey: "a more clear vocal similar to how we have in the aesthetina"):
    # aesthetina's voice EQ instead of the proximity lift — the box around 320 Hz taken down 3 dB, and
    # shelves up from 3 kHz and 8.5 kHz further on
    x = x - 0.3 * sosfilt(f("bandpass", [230, 440], 1), x) if clear else x + 0.5 * sosfilt(f("bandpass", [130, 320], 1), x)
    env = np.sqrt(np.maximum(1e-12, lfilter([1 - np.exp(-1 / (0.008 * SR))], [1, -np.exp(-1 / (0.008 * SR))], x * x)))
    env = np.maximum(env, lfilter([1 - np.exp(-1 / (0.09 * SR))], [1, -np.exp(-1 / (0.09 * SR))], env))
    thr = np.median(env[env > 0.05 * env.max()])
    x = x * np.where(env > thr, (env / thr) ** (1 / 3 - 1), 1.0)
    pk = np.abs(x).max() + 1e-9
    hi = sosfilt(f("highpass", 1200), x) / pk
    x = np.tanh(1.5 * x / pk) + 0.3 * sosfilt(f("highpass", 2500), np.tanh(5 * hi))
    x = x + (0.55 if clear else 0.4) * sosfilt(f("highpass", 3000 if clear else 3500, 1), x)
    if clear: x = x + 0.4 * sosfilt(f("highpass", 8500, 1), x)
    return (x / (np.abs(x).max() + 1e-9)).astype(np.float32)


def hear(y):
    with tempfile.TemporaryDirectory() as d:
        raw, wav = Path(d) / "x.f32", Path(d) / "x.wav"
        y.tofile(raw)
        subprocess.run([FFMPEG, "-loglevel", "error", "-y", "-f", "f32le", "-ar", str(SR), "-ac", "1", "-i", str(raw), "-ar", "16000", str(wav)], check=True)
        return subprocess.run(["whisper-cli", "-m", MODEL, "-l", "en", "-np", "-nt", "-f", str(wav)], capture_output=True, text=True).stdout.strip().replace("\n", " ")


def recall(want, got):
    a, b = norm(want), norm(got)
    return sum(m.size for m in difflib.SequenceMatcher(None, a, b).get_matching_blocks()) / max(1, len(a))


# cast the readable inflections of each line
cast_file = CACHE / "line-cast.json"
cast = json.load(open(cast_file)) if cast_file.exists() else {}
close = lambda want, got: difflib.SequenceMatcher(None, " ".join(norm(want)), " ".join(norm(got))).ratio()
for line, cadences in J.get("pin", {}).items():   # chosen by ear: no casting
    cast[line] = [{"cadence": c} for c in cadences]
for line, lyric in J["lines"].items():
    if line in cast: continue
    first = next((j for j in J["jobs"] if j["line"] == line and len(j["midis"]) == len(re.split(r"[ -]", lyric))), None)
    if not first: continue
    heard = []
    for meta in sorted(PHRASES.glob(f"cutie-{line}-*.json")):
        c = meta.stem.split("-")[-1]
        got = hear(sing(first, c)[1])
        heard.append({"cadence": c, "recall": round(recall(lyric, got), 2), "close": round(close(lyric, got), 2), "heard": got})
        print(f"  {line:5s} {c:9s} recall {heard[-1]['recall']:.2f} close {heard[-1]['close']:.2f}  heard: {got}")
    heard.sort(key=lambda h: (-h["recall"], -h["close"]))
    cast[line] = [h for h in heard if h["recall"] >= 0.8][:4] or heard[:1]
    print(f"  {line}: cast {', '.join(h['cadence'] for h in cast[line])}")
CACHE.mkdir(parents=True, exist_ok=True)
json.dump({k: v for k, v in cast.items() if k not in J.get("pin", {})}, open(cast_file, "w"), indent=1)

def fit(y, pre, job):
    """How badly a sung phrase misses its notes: per syllable, the time past
    60 ms before the voice is within 50 cents of the note, and the cents past
    40 that its middle sits off. Lower is better; 0 is every syllable on."""
    x = resample_poly(y.astype(np.float64), 1, 3); fs = SR // 3
    f0, t = pw.harvest(x, fs, f0_floor=55, f0_ceil=420, frame_period=5.0)
    ends = job["starts"][1:] + [job["starts"][-1] + job["end_hold"]]
    bad = 0.0
    for s0, e0, m in zip(job["starts"], ends, job["midis"]):
        idx = np.where((t >= s0 + pre) & (t < e0 + pre))[0]; vo = idx[f0[idx] > 0]
        if not len(vo): bad += 6; continue
        cents = 1200 * np.log2(f0[vo] / (440 * 2 ** ((m - 69) / 12)))
        ok = vo[np.abs(cents) < 50]
        lag = (t[ok[0]] - s0 - pre) * 1000 if len(ok) else (e0 - s0) * 1000
        bad += max(0, lag - 60) / 100 + max(0, abs(float(np.median(cents))) - 40) / 50 + 2 * (1 - len(vo) / len(idx))
    return bad


def takes(line, cadences):
    out = []
    for c in cadences:
        out += [c] + [f"{c}{k}" for k in range(2, 10) if (PHRASES / f"cutie-{line}-{c}{k}.json").exists()]
    return [c for c in out if (PHRASES / f"cutie-{line}-{c}.json").exists()]


made, picked = 0, {}
for job in J["jobs"]:
    f = CACHE / f"{job['key']}.f32"
    side = CACHE / f"{job['key']}.json"
    if f.exists() and side.exists() and not FORCE:
        c = json.load(open(side)).get("take"); picked[c] = picked.get(c, 0) + 1; continue
    pinned = job.get("pin") or [h["cadence"] for h in cast[job["line"]]]
    family = [pinned[job.get("turn", 0) % len(pinned)]] if not job.get("pin") else pinned
    best = None
    for c in takes(job["line"], family) or family:
        if job.get("mode") == "whisper":   # no pitch of its own to measure: the take whose words are heard best
            pre, y = sing_whisper(job, c); b = 1 - close(J["lines"][job["line"]], hear(y))
        else:
            pre, y = sing(job, c); b = fit(y, pre, job)
        if best is None or b < best[0]: best = (b, c, pre, y)
    b, c, pre, y = best
    if job.get("mode") in ("close", "clear"): y = close_mic(y, job["mode"] == "clear")
    if job.get("mode") == "angel": y = angel(y)
    y.tofile(f); json.dump({"pre": pre, "take": c, "misfit": round(b, 2)}, open(side, "w")); made += 1
    picked[c] = picked.get(c, 0) + 1
print(f"lines: {len(J['jobs'])} phrases, {made} sung now · takes used: " + ", ".join(f"{k} ×{v}" for k, v in sorted(picked.items(), key=lambda kv: -kv[1])))
