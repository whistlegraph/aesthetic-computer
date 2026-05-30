#!/usr/bin/env python3
"""identify-speaker.py — slide a window across a long recording, compute a
voice-embedding similarity to a reference clip, output per-window scores.

Used to keep only Jeffrey-speaking windows in a two-person podcast, given
a clean Jeffrey reference (the mediation strict cut).

Pipeline:
  1. Load reference + source via librosa @ 16kHz mono
  2. Compute one reference embedding (Resemblyzer encodes ~10s clips into
     a 256-d speaker vector; we average over the whole reference)
  3. Slide WINDOW_SEC on the source, embed each window, cosine-sim to
     reference
  4. Emit JSON: [{start, end, sim, isTarget}, ...]
  5. Optionally write an ffmpeg concat list of matching spans to stdout

Usage:
  identify-speaker.py REFERENCE SOURCE [--window 6] [--hop 3]
                       [--threshold auto|0.78] [--json out.json]
                       [--concat-list spans.txt]

The default 6s window / 3s hop is much finer than the dryness profiler
because speaker identity flips fast in conversation.
"""
import argparse, json, sys, os, subprocess, tempfile
import numpy as np

ap = argparse.ArgumentParser()
ap.add_argument("reference")
ap.add_argument("source")
ap.add_argument("--window", type=float, default=6.0)
ap.add_argument("--hop", type=float, default=3.0)
ap.add_argument("--threshold", default="auto")  # "auto" picks midpoint of bimodal
ap.add_argument("--json")
ap.add_argument("--concat-list")
args = ap.parse_args()

from resemblyzer import VoiceEncoder, preprocess_wav
from pathlib import Path

print("→ loading encoder…", file=sys.stderr)
encoder = VoiceEncoder(verbose=False)

def to_wav_pcm(path):
    """resemblyzer wants a clean wav path; ffmpeg → tempfile."""
    tmp = tempfile.NamedTemporaryFile(suffix=".wav", delete=False).name
    subprocess.run(
        ["ffmpeg", "-y", "-v", "error", "-i", path, "-ac", "1", "-ar", "16000", tmp],
        check=True,
    )
    return tmp

ref_wav = preprocess_wav(Path(to_wav_pcm(args.reference)))
src_wav_path = to_wav_pcm(args.source)
src_wav = preprocess_wav(Path(src_wav_path))
SR = 16000

print(f"→ ref: {len(ref_wav)/SR:.1f}s    src: {len(src_wav)/SR:.1f}s", file=sys.stderr)

ref_emb = encoder.embed_utterance(ref_wav)
print(f"→ reference embedding: {ref_emb.shape}", file=sys.stderr)

# Slide window across source
W = int(args.window * SR)
H = int(args.hop * SR)
n_windows = max(0, (len(src_wav) - W) // H + 1)
print(f"→ {n_windows} windows ({args.window}s @ {args.hop}s hop)", file=sys.stderr)

sims = []
for i in range(n_windows):
    start = i * H
    end = start + W
    chunk = src_wav[start:end]
    if len(chunk) < SR:  # too short
        continue
    try:
        emb = encoder.embed_utterance(chunk)
    except Exception:
        continue
    sim = float(np.dot(emb, ref_emb) / (np.linalg.norm(emb) * np.linalg.norm(ref_emb) + 1e-9))
    sims.append({
        "startSec": round(start / SR, 2),
        "endSec": round(end / SR, 2),
        "sim": round(sim, 4),
    })
    if (i + 1) % 20 == 0:
        print(f"  {i+1}/{n_windows}", file=sys.stderr)

# Auto-threshold: find midpoint of bimodal distribution (assumes 2 speakers)
sim_vals = sorted(s["sim"] for s in sims)
if args.threshold == "auto":
    # Otsu-ish: pick threshold that maximises between-class variance
    vals = np.array(sim_vals)
    best_t, best_var = vals.mean(), 0.0
    for t in np.linspace(vals.min(), vals.max(), 50):
        a = vals[vals < t]; b = vals[vals >= t]
        if len(a) < 3 or len(b) < 3: continue
        w = len(a) * len(b) / len(vals)**2
        var = w * (a.mean() - b.mean())**2
        if var > best_var: best_var, best_t = var, t
    threshold = float(best_t)
else:
    threshold = float(args.threshold)

for s in sims:
    s["isTarget"] = s["sim"] >= threshold

target_count = sum(1 for s in sims if s["isTarget"])
print(f"→ threshold: {threshold:.4f}", file=sys.stderr)
print(f"→ {target_count}/{len(sims)} windows match reference speaker", file=sys.stderr)
print(f"→ sim range: {sim_vals[0]:.3f} → {sim_vals[-1]:.3f}", file=sys.stderr)

result = {
    "reference": args.reference,
    "source": args.source,
    "window": args.window,
    "hop": args.hop,
    "threshold": threshold,
    "totalWindows": len(sims),
    "targetWindows": target_count,
    "segments": sims,
}

if args.json:
    with open(args.json, "w") as f:
        json.dump(result, f, indent=2)
    print(f"✓ {args.json}", file=sys.stderr)

# Emit ffmpeg concat list of contiguous target spans
if args.concat_list:
    spans = []
    cur = None
    for s in sims:
        if s["isTarget"]:
            if cur and s["startSec"] <= cur[1]:
                cur[1] = max(cur[1], s["endSec"])
            else:
                if cur: spans.append(cur)
                cur = [s["startSec"], s["endSec"]]
        else:
            if cur: spans.append(cur); cur = None
    if cur: spans.append(cur)
    print(f"→ {len(spans)} contiguous target spans, {sum(e-s for s,e in spans):.1f}s total", file=sys.stderr)
    with open(args.concat_list, "w") as f:
        json.dump(spans, f, indent=2)
    print(f"✓ {args.concat_list}", file=sys.stderr)

# Clean up tempfiles
os.unlink(src_wav_path)
