"""Revoice the bell-break single. Pass its liner.score.txt, matching line jobs, and a new directory.

Record cutie-weirdest-cheeky and cutie-weird-warm with cutie-takes.mjs first.
Render weirdest.score.txt with c/run-cutie.mjs --accel 124 --out <new.mp3>.
"""
import argparse
import json
import os
from pathlib import Path
import runpy
import shutil
import subprocess

import numpy as np
from scipy.ndimage import gaussian_filter1d

HERE = Path(__file__).resolve().parent
choir = runpy.run_path(str(HERE / "choir-cutie.py"))
modem = runpy.run_path(str(HERE / "modem-choir.py"))
SR = choir["SR"]


def performance(folder, word, cadence):
    slug = folder / f"cutie-{word}-{cadence}"
    meta = json.loads(slug.with_suffix(".json").read_text())
    if meta["lyric"].replace("-", "") != word:
        raise ValueError(f"Wrong lyric in {slug}")
    audio = subprocess.check_output(["ffmpeg", "-v", "error", "-i", str(slug.with_suffix(".mp3")),
                                     "-ac", "1", "-ar", str(SR), "-f", "f32le", "-"])
    syllables = meta["syllables"]
    start = max(0, syllables[0][0] - .025)
    raw = np.frombuffer(audio, "<f4")[round(start * SR):round((syllables[-1][1] + .04) * SR)]
    analysis = choir["analyze"](raw)
    t, f0 = analysis[:2]
    # Hold the voiced middle of "weir-", leaving the w, d and the whole "-est" at spoken speed.
    core = np.flatnonzero((f0 > 0) & (t > .10) & (t < syllables[0][1] - start - .10))
    runs = np.split(core, np.flatnonzero(np.diff(core) > 1) + 1)
    nucleus = max(runs, key=len)
    if len(nucleus) < 5:
        raise ValueError(f"No sustainable vowel in {slug}")
    return analysis, nucleus


def sing(take, frames, starts, midis):
    (t, f0, shape, air), core = take
    length = frames / SR
    # Leave a little room for the clip player's release after the consonants.
    tail = .065
    clock = np.arange(0, length + .01, choir["FRAME_MS"] / 1000)
    a, b = t[core[0]], t[core[-1]]
    source = np.interp(clock, [0, a, length - tail - (t[-1] - b), length - tail],
                       [0, a, b, t[-1]])
    ix = np.searchsorted(t, source).clip(0, len(t) - 1)
    notes = np.asarray(midis)[np.searchsorted(starts, clock, side="right") - 1]
    midi = gaussian_filter1d(notes.astype(float), 3) + .055 * np.sin(2*np.pi*4.8*clock)
    pitch = np.where(f0[ix] > 0, 440 * np.exp2((midi - 69) / 12), 0)
    y = choir["synth"](pitch, shape[ix], air[ix])[:frames]
    seconds = np.arange(frames) / SR
    y *= np.minimum(1, seconds / .008) * np.clip((length - tail - seconds) / .025, 0, 1)
    return y


def main():
    p = argparse.ArgumentParser(description=__doc__)
    for name in ("score", "jobs", "out"):
        p.add_argument(name, type=Path)
    args = p.parse_args()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    vault = Path(os.environ.get("AC_VAULT") or HERE.parents[2] / "aesthetic-computer-vault")
    phrases = vault / "voice/jeffrey-phonemes/phrases"
    takes = {w: performance(phrases, w, c) for w, c in [("weirdest", "cheeky"), ("weird", "warm")]}
    jobs = {j["key"]: j for j in json.loads(args.jobs.read_text())["jobs"]}
    rows = [line.split() for line in args.score.read_text().splitlines()]
    samples = {r[1]: Path(r[2]) for r in rows if r and r[0] == "smp"}
    at = {r[1]: float(r[2]) for r in rows if r and r[0] == "sung"}
    leads = {k for k in at if samples[k].stem.endswith("-liner")}
    first = min(at[k] for k in leads)
    screams = [p for p in samples.values() if p.name.startswith("scream-") and p.stat().st_size > 12000]
    colours = choir["COLOURS"]
    for row in rows:
        if not row or row[0] != "smp" or row[1] not in at:
            continue
        k = row[1]
        path = samples[k]
        colour, _, part = path.stem.rpartition("-")
        if k not in leads and colour not in colours:
            continue
        original = np.fromfile(path, "<f4")
        word = "weirdest" if at[k] == first else "weird"
        take = takes[word]
        if k in leads:
            job = jobs[path.stem.removesuffix("-liner")]
            if job["line"] not in colours:
                raise ValueError(f"Unexpected liner take {job['line']}")
            voice = sing(take, len(original), job["starts"], job["midis"])
        else:
            i = colours.index(colour)
            note = choir["NOTES"][i]
            if part in ("left", "right"):
                side = -1 if part == "left" else 1
                voice = choir["colour_hold"](take[0], len(original)/SR,
                                             note + (12 if side == 1 and i < 2 else 0), side)
            elif part in ("legato", "scream"):
                voice = sing(take, len(original), [0], [note + (12 if i == 0 else 0)])
                if part == "scream":
                    rasp = np.fromfile(screams[i % len(screams)], "<f4")
                    voice = modem["scream_word"](voice, rasp, choir["hz"](note))
                clock = np.arange(len(voice))/SR
                voice *= modem["envelope"](clock, len(voice)/SR, .012, 1.25)
            else:
                raise ValueError(f"Unexpected choir part {path.name}")
        voice = choir["level"](voice, choir["rms"](original))
        if len(voice) != len(original) or not np.isfinite(voice).all() or not np.any(voice):
            raise ValueError(f"Invalid replacement for {path}")
        dest = out / f"{k}-{word}.f32"
        voice.astype("<f4").tofile(dest)
        row[2] = str(dest)
    (out / "weirdest.score.txt").write_text("\n".join(" ".join(row) for row in rows) + "\n")
    shutil.copyfile(args.score.parent / "wattajetta-cutie.width.json", out / "wattajetta-cutie.width.json")


if __name__ == "__main__":
    main()
