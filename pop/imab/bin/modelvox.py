#!/usr/bin/env python3
"""modelvox.py — render the imab vocal from the AVERAGED-HZ MODEL.

The voice stays organic: per word, a real take supplies the spectral
envelope + consonants (WORLD analysis of the original recording), and
the f0 is replaced by the corpus-averaged curve from vocal-model.py —
which is itself an average of jeffrey's real sung trajectories, glides
and scoops intact. Nothing is snapped to equal temperament; the
ACCOMPANIMENT takes the model's nearest notes instead (printed at the
end). Onsets quantize to the 124 grid; durations are his own medians.

  pop/.venv/bin/python pop/imab/bin/modelvox.py [take_id] [tonic_midi]
  → pop/imab/out/imab-modelvox.wav + out/imab-vox-demo5.mp3
"""
import json, os, subprocess, sys, tempfile
import numpy as np
import soundfile as sf
import pyworld as pw
from scipy.ndimage import gaussian_filter1d

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "../../.."))
DL = f"{REPO}/toolchain/whistlegraph/downloads"
OUT = f"{REPO}/pop/imab/out"
TAKE = sys.argv[1] if len(sys.argv) > 1 else "7311159624588070175"
TONIC = float(sys.argv[2]) if len(sys.argv) > 2 else 57.0   # A3
SR = 48000
BPM, FRAME = 124, 5.0
BEAT = 60.0 / BPM
BAR = 4 * BEAT
NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]
def nname(m): return NAMES[int(round(m)) % 12] + str(int(round(m)) // 12 - 1)

model = json.load(open(f"{DL}/imab.vocal-model.json"))
syl = json.load(open(f"{DL}/whistlegraph-{TAKE}.syllnote.json"))
x, fs = sf.read(f"{DL}/whistlegraph-{TAKE}.wav", dtype="float64")
if x.ndim > 1: x = x.mean(axis=1)
if fs != SR:
    import librosa
    x = librosa.resample(x, orig_sr=fs, target_sr=SR); fs = SR

# match take words to the template, same fuzzy walk as the model builder
import re
def norm(w): return re.sub(r"[^a-z']", "", w.lower())
def fuzzy(a, b):
    return a == b or (len(a) > 3 and len(b) > 3 and (a.startswith(b[:4]) or b.startswith(a[:4])))
tmpl = [w["w"] for w in model["words"]]
src = {}
ti = 0
for w in syl["words"]:
    if ti < len(tmpl) and fuzzy(tmpl[ti], norm(w["text"])):
        src[ti] = w; ti += 1
print(f"take {TAKE}: {ti}/{len(tmpl)} source words")

G0 = 0.1
snap = lambda b, q=0.5: round(b / q) * q
total = G0 + 34 * BEAT + 2.0
mix = np.zeros(int(total * SR))
accomp_notes = []
prev_beat = -1.0
for tidx, mw in enumerate(model["words"]):
    if not mw.get("n") or tidx not in src: continue
    w = src[tidx]
    t0 = max(0, w["fromMs"] / 1000 - 0.04); t1 = min(len(x) / SR, w["toMs"] / 1000 + 0.08)
    seg = np.ascontiguousarray(x[int(t0 * SR):int(t1 * SR)])
    if len(seg) < 0.05 * SR: continue
    f0s, ts = pw.harvest(seg, SR, f0_floor=60.0, f0_ceil=500.0, frame_period=FRAME)
    sp = pw.cheaptrick(seg, f0s, ts, SR)
    ap = pw.d4c(seg, f0s, ts, SR)
    dur = mw["durSec"]
    M = max(8, int(round(dur / (FRAME / 1000))))
    imap = np.linspace(0, len(f0s) - 1, M)
    lo = np.floor(imap).astype(int); hi = np.minimum(lo + 1, len(f0s) - 1); fr = (imap - lo)[:, None]
    sp_t = np.ascontiguousarray(sp[lo] * (1 - fr) + sp[hi] * fr)
    ap_t = np.ascontiguousarray(ap[lo] * (1 - fr) + ap[hi] * fr)
    voiced = np.interp(imap, np.arange(len(f0s)), (f0s > 0).astype(float)) > 0.5
    curve = np.interp(np.linspace(0, 1, M), np.linspace(0, 1, model["npts"]),
                      np.array(mw["relCurve"])) + TONIC
    curve += gaussian_filter1d(np.random.default_rng(tidx).normal(0, 0.05, M), 4)  # breath jitter
    f0_t = np.where(voiced, 440.0 * 2 ** ((curve - 69) / 12), 0.0)
    y = pw.synthesize(np.ascontiguousarray(f0_t), sp_t, ap_t, SR, FRAME)
    beat_on = snap(mw["beat"], 0.5)
    if accomp_notes and beat_on <= prev_beat: beat_on = prev_beat + 0.5
    prev_beat = beat_on
    at = int((G0 + beat_on * BEAT) * SR)
    n = min(len(y), len(mix) - at)
    fade = min(int(0.015 * SR), n // 4)
    env = np.ones(n); env[:fade] = np.linspace(0, 1, fade); env[-fade:] = np.linspace(1, 0, fade)
    mix[at:at + n] += y[:n] * env
    accomp_notes.append((mw["w"], [nname(r + TONIC) for r in mw["nearestRel"]]))
    print(f"  {mw['w']:<12} beat {beat_on:>5} dur {dur:.2f}s  curve {mw['relCurve'][0]+TONIC:.1f}→{mw['relCurve'][-1]+TONIC:.1f}")

pk = np.abs(mix).max()
if pk > 0.9: mix *= 0.9 / pk
sf.write(f"{OUT}/imab-modelvox.wav", mix.astype(np.float32), SR)
print(f"✓ {OUT}/imab-modelvox.wav")
print("accompaniment nearest notes (the band tunes to HIM):")
for w, notes in accomp_notes: print(f"  {w:<12} {' '.join(notes)}")

# halo + demo over the bed at cycles 2–4
tmp = tempfile.mkdtemp()
subprocess.run([f"{REPO}/pop/.venv/bin/python", f"{REPO}/spinging/lib/vocal_bus.py",
                "reverb", f"{OUT}/imab-modelvox.wav", f"{tmp}/halo.wav", "-16", "1.1"], check=True)
acc, _ = sf.read(f"{OUT}/imab-accomp-124.wav", dtype="float64")
if acc.ndim > 1: acc = acc.mean(axis=1)
vox, _ = sf.read(f"{tmp}/halo.wav", dtype="float64")
def rms(a):
    a = a[np.abs(a) > 1e-4]
    return np.sqrt((a * a).mean()) if len(a) else 1e-9
vg = min(6.0, rms(acc) * 1.9 / rms(vox))
demo = acc.copy()
for cyc in (1, 2, 3):
    at = int(cyc * 8 * BAR * SR)
    n = min(len(vox), len(demo) - at)
    demo[at:at + n] += vox[:n] * vg
pk = np.abs(demo).max()
if pk > 0.9: demo *= 0.9 / pk
sf.write(f"{tmp}/demo.wav", np.stack([demo, demo], 1).astype(np.float32), SR)
subprocess.run(["ffmpeg", "-hide_banner", "-loglevel", "error", "-y", "-i", f"{tmp}/demo.wav",
                "-metadata", "title=imab-vox-demo5", "-metadata", "artist=Whistlegraph Dot Org",
                "-c:a", "libmp3lame", "-q:a", "2", f"{OUT}/imab-vox-demo5.mp3"], check=True)
print(f"✓ {OUT}/imab-vox-demo5.mp3 (vox gain {vg:.2f})")
