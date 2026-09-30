#!/usr/bin/env python3
# analyze-take.py — read a live voice+guitar take the way we read a
# menuband take: where the pulse is (and how far it drifts), what the
# tuning is, what chord sits under each beat, and what the voice sings.
#
# Unlike toolchain/whistlegraph/analyze.py (whistle range, fmin 200 Hz),
# this tracks a sung voice (80–1000 Hz) and a strummed guitar. Feed it
# the full mix, or pass --vocals / --guitar stems once Demucs has split
# them for cleaner melody and chord reads.
#
# Usage:
#   pop/.venv/bin/python pop/sailor-song/bin/analyze-take.py src/take.wav \
#     [--vocals stems/vocals.wav] [--guitar stems/other.wav] [--json out.json]

import argparse, json
import numpy as np
import librosa

NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]


def name(m):
    m = int(round(m))
    return f"{NAMES[m % 12]}{m // 12 - 1}"


def load(path, sr=22050):
    y, _ = librosa.load(path, sr=sr, mono=True)
    return y, sr


# ── pulse ─────────────────────────────────────────────────────────────
def pulse(y, sr):
    onset = librosa.onset.onset_strength(y=y, sr=sr, hop_length=512)
    tempo, beats = librosa.beat.beat_track(
        onset_envelope=onset, sr=sr, hop_length=512, tightness=60, units="time"
    )
    tempo = float(np.atleast_1d(tempo)[0])
    # Strummed guitar at a slow tempo often reads at half speed — keep
    # both and let the reader pick the felt pulse.
    ibi = np.diff(beats)
    local = 60.0 / ibi if len(ibi) else np.array([])
    # Drift per 8-beat window: how rubato is the take?
    win = []
    for i in range(0, len(beats) - 8, 8):
        seg = beats[i : i + 9]
        win.append({"t": round(float(seg[0]), 2),
                    "bpm": round(float(8 * 60.0 / (seg[-1] - seg[0])), 1)})
    return {
        "tempoBPM": round(tempo, 2),
        "doubleTimeBPM": round(tempo * 2, 2),
        "beats": [round(float(b), 3) for b in beats],
        "ibiMedian": round(float(np.median(ibi)), 3) if len(ibi) else None,
        "ibiCV": round(float(np.std(ibi) / np.mean(ibi)), 3) if len(ibi) else None,
        "localBPM": {"min": round(float(local.min()), 1),
                     "p10": round(float(np.percentile(local, 10)), 1),
                     "median": round(float(np.median(local)), 1),
                     "p90": round(float(np.percentile(local, 90)), 1),
                     "max": round(float(local.max()), 1)} if len(local) else None,
        "drift": win,
    }


# ── chords ────────────────────────────────────────────────────────────
def chord_templates():
    t, labels = [], []
    shapes = {"": [0, 4, 7], "m": [0, 3, 7], "7": [0, 4, 7, 10],
              "maj7": [0, 4, 7, 11], "m7": [0, 3, 7, 10], "sus4": [0, 5, 7],
              "sus2": [0, 2, 7]}
    for root in range(12):
        for q, iv in shapes.items():
            v = np.zeros(12)
            for i in iv:
                v[(root + i) % 12] = 1
            # Bias toward plain triads so 7ths only win when clearly there.
            w = 1.0 if q in ("", "m") else 0.93
            t.append(w * v / np.linalg.norm(v))
            labels.append(NAMES[root] + q)
    return np.array(t), labels


def chords(y, sr, beats, tuning):
    yh = librosa.effects.harmonic(y, margin=4)
    chroma = librosa.feature.chroma_cqt(y=yh, sr=sr, hop_length=512,
                                        tuning=tuning, bins_per_octave=36)
    frames = librosa.time_to_frames(beats, sr=sr, hop_length=512)
    sync = librosa.util.sync(chroma, frames, aggregate=np.median)
    T, labels = chord_templates()
    sync = sync / (np.linalg.norm(sync, axis=0, keepdims=True) + 1e-9)
    score = T @ sync
    idx = score.argmax(axis=0)
    # Median-ish smoothing: a chord must hold 2 beats unless neighbors agree.
    for i in range(1, len(idx) - 1):
        if idx[i - 1] == idx[i + 1] != idx[i]:
            idx[i] = idx[i - 1]
    bounds = np.concatenate([[0.0], beats, [len(y) / sr]])
    per_beat = []
    for i, k in enumerate(idx):
        per_beat.append({"t": round(float(bounds[i]), 2), "chord": labels[k],
                         "conf": round(float(score[k, i]), 2)})
    # Collapse runs.
    runs = []
    for b in per_beat:
        if runs and runs[-1]["chord"] == b["chord"]:
            runs[-1]["beats"] += 1
        else:
            runs.append({"t": b["t"], "chord": b["chord"], "beats": 1})
    # Vocabulary by total beats.
    vocab = {}
    for r in runs:
        vocab[r["chord"]] = vocab.get(r["chord"], 0) + r["beats"]
    vocab = dict(sorted(vocab.items(), key=lambda kv: -kv[1]))
    return {"runs": runs, "vocab": vocab,
            "chromaMean": [round(float(c), 3) for c in chroma.mean(axis=1)]}


def key_of(chroma_mean):
    maj = np.array([6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88])
    mnr = np.array([6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17])
    c = np.array(chroma_mean)
    best = []
    for prof, q in ((maj, "major"), (mnr, "minor")):
        for i in range(12):
            best.append((float(np.corrcoef(np.roll(prof, i), c)[0, 1]), NAMES[i] + " " + q))
    best.sort(reverse=True)
    return [{"key": k, "r": round(r, 3)} for r, k in best[:4]]


# ── voice ─────────────────────────────────────────────────────────────
def voice(y, sr, tuning):
    hop = 256
    f0, voiced, prob = librosa.pyin(y, sr=sr, fmin=80, fmax=1000,
                                    frame_length=2048, hop_length=hop)
    t = librosa.times_like(f0, sr=sr, hop_length=hop)
    midi = librosa.hz_to_midi(f0) - tuning  # tuning in semitones
    ok = voiced & (prob > 0.5) & np.isfinite(midi)
    notes, cur = [], None
    for i in range(len(midi)):
        if not ok[i]:
            if cur:
                notes.append(cur); cur = None
            continue
        q = int(round(midi[i]))
        if cur and abs(q - cur["q"]) == 0:
            cur["end"] = t[i]; cur["cents"].append(100 * (midi[i] - q))
        else:
            if cur:
                notes.append(cur)
            cur = {"q": q, "start": t[i], "end": t[i], "cents": [100 * (midi[i] - q)]}
    if cur:
        notes.append(cur)
    out = []
    for n in notes:
        d = n["end"] - n["start"] + hop / sr
        if d < 0.09:  # drop portamento blips
            continue
        out.append({"t": round(float(n["start"]), 3), "dur": round(float(d), 3),
                    "midi": n["q"], "note": name(n["q"]),
                    "cents": int(round(float(np.median(n["cents"]))))})
    # Merge same-pitch notes split by tiny gaps.
    merged = []
    for n in out:
        if merged and merged[-1]["midi"] == n["midi"] and \
                n["t"] - (merged[-1]["t"] + merged[-1]["dur"]) < 0.08:
            merged[-1]["dur"] = round(n["t"] + n["dur"] - merged[-1]["t"], 3)
        else:
            merged.append(n)
    vm = midi[ok]
    hist = np.bincount((np.round(vm).astype(int) % 12), minlength=12) if len(vm) else np.zeros(12)
    return {
        "range": [name(np.percentile(vm, 2)), name(np.percentile(vm, 98))] if len(vm) else None,
        "median": name(np.median(vm)) if len(vm) else None,
        "voicedFraction": round(float(ok.mean()), 3),
        "pitchClassHist": {NAMES[i]: int(hist[i]) for i in range(12)},
        "notes": merged,
    }


# ── sections (energy + phrase gaps) ───────────────────────────────────
def phrases(vnotes, gap=0.9):
    out, cur = [], None
    for n in vnotes:
        if cur and n["t"] - cur["end"] > gap:
            out.append(cur); cur = None
        if not cur:
            cur = {"start": n["t"], "end": n["t"] + n["dur"], "notes": []}
        cur["end"] = n["t"] + n["dur"]
        cur["notes"].append(n["note"])
    if cur:
        out.append(cur)
    return [{"start": round(p["start"], 2), "end": round(p["end"], 2),
             "line": " ".join(p["notes"])} for p in out]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("mix")
    ap.add_argument("--vocals")
    ap.add_argument("--guitar")
    ap.add_argument("--json")
    a = ap.parse_args()

    y, sr = load(a.mix)
    yv = load(a.vocals)[0] if a.vocals else y
    yg = load(a.guitar)[0] if a.guitar else y

    tuning = float(librosa.estimate_tuning(y=yg, sr=sr))  # fraction of a semitone
    P = pulse(yg, sr)
    C = chords(yg, sr, np.array(P["beats"]), tuning)
    K = key_of(C["chromaMean"])
    V = voice(yv, sr, tuning)
    ph = phrases(V["notes"])

    res = {"source": a.mix, "duration": round(len(y) / sr, 2),
           "tuningCents": round(tuning * 100, 1), "pulse": P, "key": K,
           "chords": C, "voice": V, "phrases": ph,
           "stems": {"vocals": a.vocals, "guitar": a.guitar}}

    print(f"duration  {res['duration']}s   tuning {res['tuningCents']:+}¢ off A440")
    print(f"tempo     {P['tempoBPM']} BPM (x2 = {P['doubleTimeBPM']})   "
          f"beat-interval CV {P['ibiCV']}")
    print(f"local BPM {P['localBPM']}")
    print("drift     " + "  ".join(f"{w['t']:.0f}s:{w['bpm']}" for w in P["drift"]))
    print("key       " + ", ".join(f"{k['key']} ({k['r']})" for k in K))
    print("chords    " + ", ".join(f"{c} {n}" for c, n in list(C["vocab"].items())[:10]))
    print(f"voice     {V['range']} median {V['median']}  voiced {V['voicedFraction']}")
    print("── chord runs ──")
    line = []
    for r in C["runs"]:
        line.append(f"{r['chord']}×{r['beats']}")
    print(" ".join(line))
    print("── phrases ──")
    for p in ph:
        print(f"  {p['start']:7.2f}–{p['end']:7.2f}  {p['line']}")
    if a.json:
        with open(a.json, "w") as f:
            json.dump(res, f, indent=1)


if __name__ == "__main__":
    main()
