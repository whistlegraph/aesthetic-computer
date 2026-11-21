#!/usr/bin/env python3
"""
acapella-test.py — hear the sung lines alone against a click, to check the
lyric-to-melody mapping.

Takes the first sung row of the last render (the four phrases of the hook, in
out/.cutie-line-jobs.json and the clips in out/.cutie-vox-cache) and writes
three passes, each with a one-bar count-in:

  1  the melody alone: a plain tone on each note, with the click
  2  the voice alone, dry, with the click
  3  both: voice in the left ear, the tone in the right, click in the middle

  pop/.venv/bin/python pop/wattajetta/bin/acapella-test.py
Writes out/cutie-acapella-test.mp3 and prints, for every syllable, the beat it
lands on, its note and how long it is held.
"""
import json, subprocess
from pathlib import Path
import numpy as np

HERE = Path(__file__).resolve().parent
OUT = HERE.parent / "out"
J = json.load(open(OUT / ".cutie-line-jobs.json"))
SR, CACHE, LINES = J["sr"], Path(J["cache"]), J["lines"]
BPM = 112; BEAT = 60 / BPM; BAR = 4 * BEAT
EARLY = 0.02   # the renderer places each line this much early
NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
name = lambda m: f"{NAMES[m % 12]}{m // 12 - 1}"

row = [j for j in J["jobs"] if j["line"] != "back"][:4]   # the first sung row: its four two-bar phrases
N = int((9 * BAR + 2.5) * SR)   # count-in, eight bars, room for the last hold


def click():
    y = np.zeros(N)
    for b in range(36):
        i = int(b * BEAT * SR); n = int(0.018 * SR); t = np.arange(n) / SR
        y[i:i + n] += np.sin(2 * np.pi * (1600 if b % 4 == 0 else 1000) * t) * np.exp(-t / 0.004) * (0.5 if b % 4 == 0 else 0.3)
    return y


def voice():
    y = np.zeros(N)
    for k, job in enumerate(row):
        clip = np.fromfile(CACHE / f"{job['key']}.f32", dtype=np.float32)
        pre = json.load(open(CACHE / f"{job['key']}.json"))["pre"]
        i = int((BAR + k * 2 * BAR - pre - EARLY) * SR)
        a, b = max(0, i), min(N, i + len(clip))
        y[a:b] += clip[a - i:b - i] * 0.8
    return y


def tone():
    y = np.zeros(N)
    for k, job in enumerate(row):
        ends = job["starts"][1:] + [job["starts"][-1] + job["end_hold"]]
        for s, e, m in zip(job["starts"], ends, job["midis"]):
            i = int((BAR + k * 2 * BAR + s) * SR); n = min(int((e - s) * SR), N - i); t = np.arange(n) / SR
            env = np.minimum(1, t / 0.005) * np.minimum(1, (n / SR - t) / 0.02)
            y[i:i + n] += np.sin(2 * np.pi * 440 * 2 ** ((m + 12 - 69) / 12) * t) * env * 0.25   # an octave over the voice, so the two are easy to tell apart
    return y


c, v, g = click(), voice(), tone()
passes = [np.stack([g + c, g + c], 1), np.stack([v + c, v + c], 1), np.stack([v + c, g + c], 1)]
audio = np.concatenate(passes).astype(np.float32)
out = OUT / "cutie-acapella-test.mp3"
subprocess.run(["/opt/homebrew/bin/ffmpeg", "-loglevel", "error", "-y", "-f", "f32le", "-ar", str(SR), "-ac", "2", "-i", "-", "-b:a", "192k", str(out)], input=audio.tobytes(), check=True)

for k, job in enumerate(row):
    syl = LINES[job["line"]].replace("-", " ").split()
    ends = job["starts"][1:] + [job["starts"][-1] + job["end_hold"]]
    print(f"\nphrase {k + 1} · {job['line']} · bars {k * 2 + 1}-{k * 2 + 2}")
    for s, e, m, w in zip(job["starts"], ends, job["midis"], syl):
        beat = k * 8 + s / BEAT
        print(f"  {w:7s} bar {int(beat // 4) + 1} beat {beat % 4 + 1:4.2f}  {name(m):4s} held {(e - s) / BEAT:4.2f} beats")
    if len(syl) > len(job["midis"]): print(f"  (not sung: {' '.join(syl[len(job['midis']):])})")
span = 9 * BAR + 2.5
print(f"\n{out}\n0:00 melody alone · {int(span // 60)}:{span % 60:04.1f} voice alone · {int(2 * span // 60)}:{2 * span % 60:04.1f} voice left, melody right")
