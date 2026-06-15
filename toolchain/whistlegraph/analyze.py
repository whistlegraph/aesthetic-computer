#!/usr/bin/env python3
# analyze.py — musicality readout for a whistlegraph clip.
#
# Reads a WAV (mono) and prints tempo, key/scale estimate, the whistled
# melody as a note sequence (pyin pitch track quantized to MIDI), and
# onset times. Designed as the "what is this song" pass before porting a
# whistlegraph into a /pop track.
#
# Usage: pop/.venv/bin/python analyze.py <wav> [--json out.json]

import sys, json, argparse
import numpy as np
import librosa

NOTE_NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

def midi_to_name(m):
    m = int(round(m))
    return f"{NOTE_NAMES[m % 12]}{m // 12 - 1}"

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("wav")
    ap.add_argument("--json")
    args = ap.parse_args()

    y, sr = librosa.load(args.wav, sr=None, mono=True)
    dur = len(y) / sr

    # ── tempo + beats ────────────────────────────────────────────────
    tempo, beats = librosa.beat.beat_track(y=y, sr=sr, units="time")
    tempo = float(np.atleast_1d(tempo)[0])

    # ── key / scale via chroma (Krumhansl-Schmuckler style profile) ──
    chroma = librosa.feature.chroma_cqt(y=y, sr=sr)
    chroma_mean = chroma.mean(axis=1)
    maj = np.array([6.35,2.23,3.48,2.33,4.38,4.09,2.52,5.19,2.39,3.66,2.29,2.88])
    minr = np.array([6.33,2.68,3.52,5.38,2.60,3.53,2.54,4.75,3.98,2.69,3.34,3.17])
    def best(profile):
        scores = [np.corrcoef(np.roll(profile, i), chroma_mean)[0,1] for i in range(12)]
        k = int(np.argmax(scores)); return k, scores[k]
    kmaj, smaj = best(maj); kmin, smin = best(minr)
    if smaj >= smin:
        key, scale, kscore = NOTE_NAMES[kmaj], "major", smaj
    else:
        key, scale, kscore = NOTE_NAMES[kmin], "minor", smin

    # ── whistled melody: pyin pitch track ───────────────────────────
    f0, voiced, vprob = librosa.pyin(
        y, sr=sr, fmin=200, fmax=2400,  # whistle sits high
        frame_length=2048, hop_length=256)
    times = librosa.times_like(f0, sr=sr, hop_length=256)

    # quantize voiced frames to MIDI, then collapse runs into notes
    notes = []
    cur = None
    for t, hz, v in zip(times, f0, voiced):
        if not v or np.isnan(hz):
            if cur: notes.append(cur); cur = None
            continue
        m = librosa.hz_to_midi(hz)
        q = int(round(m))
        if cur and abs(q - cur["midi"]) == 0:
            cur["end"] = float(t); cur["frames"] += 1; cur["cents"].append(float((m - q) * 100))
        else:
            if cur: notes.append(cur)
            cur = {"midi": q, "start": float(t), "end": float(t), "frames": 1, "cents": [float((m - q) * 100)]}
    if cur: notes.append(cur)
    # drop blips shorter than ~50ms
    notes = [n for n in notes if (n["end"] - n["start"]) >= 0.05]
    melody = [{
        "note": midi_to_name(n["midi"]),
        "midi": n["midi"],
        "startSec": round(n["start"], 3),
        "durSec": round(n["end"] - n["start"], 3),
        "centsOff": round(float(np.mean(n["cents"])), 1),
    } for n in notes]

    # ── onsets ───────────────────────────────────────────────────────
    onset_t = librosa.onset.onset_detect(y=y, sr=sr, units="time", backtrack=True)

    pitched = f0[~np.isnan(f0)]
    rng = (librosa.hz_to_midi(np.nanmin(pitched)), librosa.hz_to_midi(np.nanmax(pitched))) if len(pitched) else (None, None)

    out = {
        "durationSec": round(dur, 2),
        "tempoBPM": round(tempo, 1),
        "key": f"{key} {scale}",
        "keyConfidence": round(float(kscore), 3),
        "beatCount": len(beats),
        "onsetCount": len(onset_t),
        "noteCount": len(melody),
        "pitchRange": [midi_to_name(rng[0]), midi_to_name(rng[1])] if rng[0] else None,
        "melody": melody,
        "onsetsSec": [round(float(t), 3) for t in onset_t],
    }

    # ── pretty print ─────────────────────────────────────────────────
    print(f"  duration   : {out['durationSec']}s")
    print(f"  tempo      : {out['tempoBPM']} BPM  ({out['beatCount']} beats)")
    print(f"  key        : {out['key']}  (conf {out['keyConfidence']})")
    print(f"  pitch range: {out['pitchRange'][0]} … {out['pitchRange'][1]}" if out['pitchRange'] else "  pitch range: —")
    print(f"  onsets     : {out['onsetCount']}")
    print(f"  melody     : {out['noteCount']} notes")
    seq = "  ".join(f"{n['note']}" for n in melody)
    print(f"    {seq}")
    print("  ── note timeline ──")
    for n in melody:
        bar = "▮" * max(1, int(n["durSec"] / 0.06))
        off = f" ({n['centsOff']:+.0f}¢)" if abs(n["centsOff"]) >= 15 else ""
        print(f"    {n['startSec']:>5.2f}s  {n['note']:<4} {n['durSec']:.2f}s {bar}{off}")

    if args.json:
        with open(args.json, "w") as f:
            json.dump(out, f, indent=2)
        print(f"  → json {args.json}")

if __name__ == "__main__":
    main()
