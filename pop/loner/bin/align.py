# align.py — word alignment for the loner slices, from OpenAI whisper-1.
#
# @jeffrey, after watching the study video and hearing the wrong syllable
# inside a block: "hmm ur mistaken, the 'led' is actually the 'up'
# utterance in the data · you need to fix that alignment too · did you
# try using whisper to find the original word boundary · if not we
# should pass the audio to openai for that pls".
#
# He was right. The lane's original receipts came from whisper.cpp
# ggml-small run with `-ml 1` (max one word per segment), which returns
# SUB-WORD tokens: it cut "curled" into "cur" + "led" and then every
# label after it slid by a syllable. The span labelled `led` is where
# she actually sings **up**; the spans labelled `up` + `in` are both the
# one word **in**; and `stone` began 1.3 s early, inside the held octave
# of "of a" — which is why the octave kept bleeding into stone's block.
#
# A note segmentation of the take agrees with OpenAI and not with the
# old receipts: the first phrase is seven sung notes — F4 D#4 · C#4 ·
# C4 · A#3 · D#4 · C4 — for seven syllables, sit·ting curled up in
# my·self. There is no separate "led" note; it was the tail of "curled"
# plus the breath before "up".
#
#   OPENAI_API_KEY=... pop/.venv/bin/python pop/loner/bin/align.py
#     → samples/.align.json   (per slice, per word: start, end, f0, note)
#
# The key is read from the environment, or from the vault env file if
# that is present. It is never written to the receipt.

import json, os, subprocess, sys
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
REPO = os.path.dirname(os.path.dirname(LANE))
SAMPLES = os.path.join(LANE, "samples")
TONIC = 237.0
FLOOR = 70.0
VAULT_ENV = os.path.join(REPO, "aesthetic-computer-vault",
                         ".devcontainer", "envs", "devcontainer.env")

# the slices the v4 chart plays — see CHART in halo3.py
SLICES = [
    "f-whole-line", "f-sitting-curled", "f-i-think", "f-of-a-stone",
    "f-just-waiting", "f-very-patiently", "f-for-time-to-pass",
    "n-getting-curled", "n-stone-waiting", "n-for-time-to-pass",
]

CHROM = ["A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A"]


def note_name(hz):
    st = int(round(12.0 * np.log2(hz / TONIC)))
    return f"{CHROM[st % 12]}{3 + (st + 10) // 12}"


def api_key():
    k = os.environ.get("OPENAI_API_KEY")
    if k:
        return k
    if os.path.exists(VAULT_ENV):
        for line in open(VAULT_ENV):
            if line.startswith("OPENAI_API_KEY"):
                return line.split("=", 1)[1].strip().strip("'\"")
    sys.exit("no OPENAI_API_KEY (env or vault)")


# The song is one known sentence; priming whisper with it stops the
# short clips from being misheard (f-sitting-curled came back as "in my
# cell phone", f-for-time-to-pass as "the time to pass").
LYRIC = ("sitting curled up in myself, i think of a stone, "
         "just waiting very patiently for time to pass")


def transcribe(path, key):
    """whisper-1 with word-level timestamps. curl keeps the key off the
    process list of anything but this call."""
    out = subprocess.run(
        ["curl", "-s", "https://api.openai.com/v1/audio/transcriptions",
         "-H", f"Authorization: Bearer {key}",
         "-F", f"file=@{path}",
         "-F", "model=whisper-1",
         "-F", f"prompt={LYRIC}",
         "-F", "response_format=verbose_json",
         "-F", "timestamp_granularities[]=word"],
        capture_output=True, text=True, check=True).stdout
    d = json.loads(out)
    if "error" in d:
        sys.exit(f"{os.path.basename(path)}: {d['error'].get('message')}")
    return d


def repair(words, x, fs):
    """whisper-1 occasionally emits a zero-width word (it gave the whole
    line's `just` as 12.38–12.38). Rather than discard an otherwise
    perfect alignment, find that word's real onset in the audio: the
    first sustained-loud frame between its neighbours."""
    fixed = []
    peak = np.max(np.abs(x)) or 1.0
    n = int(round(fs * 0.005))
    for i, w in enumerate(words):
        w = dict(w)
        if w["end"] - w["start"] > 0.05:
            fixed.append(w)
            continue
        lo = fixed[-1]["end"] if fixed else 0.0
        hi = words[i + 1]["start"] if i + 1 < len(words) else len(x) / fs
        a, b = int(lo * fs / n), int(hi * fs / n)
        seg = x[a * n:b * n]
        m = len(seg) // n
        if m < 4 or hi <= lo:
            fixed.append(w)
            continue
        rms = np.sqrt((seg[:m * n].reshape(m, n) ** 2).mean(axis=1))
        loud = rms > peak * 10.0 ** (-36.0 / 20.0)
        run, onset = int(0.060 / 0.005), None
        for k in range(m - run):
            if loud[k:k + run].all():
                onset = k
                break
        w["start"] = round(lo + (onset or 0) * 0.005, 3)
        w["end"] = round(hi, 3)
        fixed.append(w)
        print(f"    repaired zero-width «{w['word']}» → {w['start']:.2f}–{w['end']:.2f}")
    return fixed


def measure(x, fs, words):
    """Median f0 over each word's voiced frames — the note she sings."""
    f0r, t = pw.harvest(x, fs, f0_floor=FLOOR, f0_ceil=600.0, frame_period=5.0)
    f0 = pw.stonemask(x, f0r, t, fs)
    out = []
    for w in words:
        a, b = int(w["start"] / 0.005), int(w["end"] / 0.005)
        seg = f0[max(0, a):max(1, min(b, len(f0)))]
        seg = seg[seg > 0]
        hz = float(np.median(seg)) if len(seg) else 0.0
        out.append(dict(t=w["word"], start=round(w["start"], 3),
                        end=round(w["end"], 3), f0_hz=round(hz, 1),
                        note=note_name(hz) if hz else ""))
    return out


def main():
    key = api_key()
    align = {}
    for name in SLICES:
        path = os.path.join(SAMPLES, f"{name}.wav")
        if not os.path.exists(path):
            print(f"  {name:22s} — missing, skipped")
            continue
        d = transcribe(path, key)
        words = d.get("words") or []
        x, fs = sf.read(path, dtype="float64")
        if x.ndim > 1:
            x = x.mean(axis=1)
        words = repair(words, x, fs)
        entry = measure(x, fs, words)
        # a usable alignment is strictly ordered with no zero-width word
        # (n-stone-waiting once returned just@3.86 AND waiting@3.86)
        ok = all(b["start"] > a["start"] and b["start"] >= a["end"] - 0.001
                 for a, b in zip(entry[:-1], entry[1:])) \
            and all(w["end"] - w["start"] > 0.05 for w in entry) and len(entry) > 1
        if not ok:
            print(f"  {name:22s} — DEGENERATE alignment, left on the old receipt")
            continue
        align[name] = dict(model="whisper-1 (openai, word timestamps)",
                           text=d.get("text", "").strip(), words=entry)
        print(f"  {name:22s} {len(entry):2d} words  «{d.get('text','').strip()[:52]}»")
        print("    " + " · ".join(f"{w['t']}@{w['start']:.2f}" for w in entry))
    dest = os.path.join(SAMPLES, ".align.json")
    json.dump(align, open(dest, "w"), indent=1)
    print(f"WROTE {dest} ({len(align)} slices)")


if __name__ == "__main__":
    main()
