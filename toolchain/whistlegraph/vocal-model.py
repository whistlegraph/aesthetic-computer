#!/usr/bin/env python3
"""vocal-model.py — the averaged-Hz vocal model of a song, from many takes.

For every template word (anchored by whisper utterance windows), extract
the CONTINUOUS sung f0 curve from each performance, register-normalize
per take (semitones vs the take's weighted-median sung pitch), and
point-wise median the curves across takes → one organic averaged
trajectory per word, with duration and onset-beat statistics. This is a
MODEL OF THE VOICE, not a quantized melody: glides, scoops and holds
survive the averaging. The accompaniment then takes the NEAREST
equal-tempered notes to the model (reported per word) — the band tunes
to the singer; the singer is never snapped.

  pop/.venv/bin/python vocal-model.py downloads/imab.corpus.json \
      --template "i'm a butterfly, flapping for you guys, just a costume, i put on, in my room"
  → downloads/<slug>.vocal-model.json

Per-take f0 curves are cached as <id>.f0curve.json next to the wavs.
"""
import json, os, re, sys
import numpy as np
import librosa

HERE = os.path.dirname(os.path.abspath(__file__))
DL = os.path.join(HERE, "downloads")
NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]
NPTS = 32

def norm(w): return re.sub(r"[^a-z']", "", w.lower())
def fuzzy(a, b):
    return a == b or (len(a) > 3 and len(b) > 3 and (a.startswith(b[:4]) or b.startswith(a[:4])))

def take_curves(clip_id, tmpl):
    """per template word: f0 curve (midi, NPTS points), window dur, onset."""
    cache = f"{DL}/whistlegraph-{clip_id}.f0curve.json"
    syl = f"{DL}/whistlegraph-{clip_id}.syllnote.json"
    wav = f"{DL}/whistlegraph-{clip_id}.wav"
    if os.path.exists(cache):
        return json.load(open(cache))
    if not (os.path.exists(syl) and os.path.exists(wav)):
        return None
    doc = json.load(open(syl))
    words = doc["words"]
    seq = []
    ti = 0
    for wi, w in enumerate(words):
        if ti < len(tmpl) and fuzzy(tmpl[ti], norm(w["text"])):
            seq.append((ti, w)); ti += 1
    if ti < 0.7 * len(tmpl): return None
    y, sr = librosa.load(wav, sr=22050, mono=True)
    hop = 256
    f0, voiced, vprob = librosa.pyin(y, sr=sr, fmin=80, fmax=600,
                                     frame_length=2048, hop_length=hop)
    times = librosa.times_like(f0, sr=sr, hop_length=hop)
    out = {"id": clip_id, "words": {}}
    for (tidx, w) in seq:
        t0, t1 = w["fromMs"] / 1000 - 0.04, w["toMs"] / 1000 + 0.08
        sel = (times >= t0) & (times <= t1) & voiced & (vprob > 0.3) & np.isfinite(f0)
        if sel.sum() < 5: continue
        tt = times[sel]; ff = 69 + 12 * np.log2(f0[sel] / 440.0)
        # resample the voiced trajectory to NPTS normalized-time points
        u = (tt - tt[0]) / max(1e-6, tt[-1] - tt[0])
        grid = np.linspace(0, 1, NPTS)
        curve = np.interp(grid, u, ff)
        out["words"][str(tidx)] = {
            "curve": [round(float(x), 3) for x in curve],
            "winDur": round((w["toMs"] - w["fromMs"]) / 1000, 3),
            "onset": round(float(tt[0]), 3),
        }
    json.dump(out, open(cache, "w"))
    return out

def main():
    corpus_path = sys.argv[1]
    template_raw = sys.argv[sys.argv.index("--template") + 1]
    corpus = json.load(open(corpus_path))
    slug = corpus["slug"]
    tmpl = [norm(w) for p in template_raw.split(",") for w in p.strip().split()]

    takes = []
    for clip in corpus["clips"]:
        tc = take_curves(clip["id"], tmpl)
        if tc and len(tc["words"]) >= 0.6 * len(tmpl):
            takes.append(tc)
        if tc is not None:
            print(f"  {clip['id']}: {len(tc['words'])}/{len(tmpl)} word curves")
    print(f"{len(takes)} takes carry curves")

    # register: per take, weighted center = median of all its curve points
    for t in takes:
        pts = np.concatenate([np.array(w["curve"]) for w in t["words"].values()])
        t["center"] = float(np.median(pts))
    # rhythm: onset in med-IOI units per take
    for t in takes:
        idxs = sorted(t["words"], key=lambda k: int(k))
        ons = [t["words"][k]["onset"] for k in idxs]
        d = np.diff(ons); d = d[d > 0.08]
        t["ioi"] = float(np.median(d)) if len(d) else 0.5
        t["t0"] = ons[0] if ons else 0.0

    # ── outlier handling ──────────────────────────────────────────────
    # 1 · octave rescue: a word curve sitting ~±12 st off the ensemble is a
    #     different-octave performance — fold it home, don't lose it.
    # 2 · take rejection: takes still far from the ensemble after rescue
    #     (multi-voice / in-the-round / spoken takes give pyin garbage) are
    #     dropped entirely. Two passes so the model settles.
    for _pass in range(2):
        skel = {}
        for tidx in range(len(tmpl)):
            cs = [np.array(t["words"][str(tidx)]["curve"]) - t["center"]
                  for t in takes if str(tidx) in t["words"]]
            if cs: skel[tidx] = np.median(np.stack(cs), axis=0)
        for t in takes:
            for k, w in t["words"].items():
                if int(k) not in skel: continue
                rel = np.array(w["curve"]) - t["center"]
                dev = float(np.median(rel - skel[int(k)]))
                if 9.5 < abs(dev) < 14.5:
                    w["curve"] = [round(float(x - np.sign(dev) * 12), 3) for x in w["curve"]]
        kept = []
        for t in takes:
            devs = [abs(float(np.median((np.array(w["curve"]) - t["center"]) - skel[int(k)])))
                    for k, w in t["words"].items() if int(k) in skel]
            score = float(np.median(devs)) if devs else 99
            if score <= 3.0: kept.append(t)
            elif _pass == 1: print(f"  ✗ outlier take {t['id']} (median dev {score:.1f} st) dropped")
        takes = kept
    print(f"{len(takes)} takes after outlier rejection")

    model = {"slug": slug, "template": template_raw, "takes": len(takes),
             "npts": NPTS, "words": []}
    for tidx, word in enumerate(tmpl):
        curves, durs, beats = [], [], []
        for t in takes:
            w = t["words"].get(str(tidx))
            if not w: continue
            curves.append(np.array(w["curve"]) - t["center"])
            durs.append(w["winDur"])
            beats.append((w["onset"] - t["t0"]) / t["ioi"])
        if not curves:
            model["words"].append({"w": word, "n": 0}); continue
        C = np.stack(curves)
        med = np.median(C, axis=0)
        # nearest accompaniment notes: duration-weighted histogram of the
        # curve's nearest semitones (top plateaus = what the band plays)
        rounded = np.round(C.flatten()).astype(int)
        vals, counts = np.unique(rounded, return_counts=True)
        near = sorted(zip(vals.tolist(), counts.tolist()), key=lambda x: -x[1])[:3]
        model["words"].append({
            "w": word, "n": len(curves),
            "relCurve": [round(float(x), 2) for x in med],
            "relIQR": [round(float(np.percentile(C, 25)), 2), round(float(np.percentile(C, 75)), 2)],
            "durSec": round(float(np.median(durs)), 3),
            "beat": round(float(np.median(beats)), 2),
            "nearestRel": [n for n, _ in near],
        })
    out_path = os.path.join(DL, f"{slug}.vocal-model.json")
    json.dump(model, open(out_path, "w"), indent=1)
    print(f"✓ {out_path}")
    print(f"{'word':<12}{'n':>4}{'beat':>7}{'dur':>7}  curve(start→mid→end, rel st)   nearest")
    for w in model["words"]:
        if not w["n"]: print(f"{w['w']:<12}{0:>4}   —"); continue
        c = w["relCurve"]
        arc = f"{c[0]:+.1f} → {c[NPTS//2]:+.1f} → {c[-1]:+.1f}"
        print(f"{w['w']:<12}{w['n']:>4}{w['beat']:>7}{w['durSec']:>7}  {arc:<28}{w['nearestRel']}")

if __name__ == "__main__":
    main()
