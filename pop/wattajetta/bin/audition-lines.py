#!/usr/bin/env python3
"""
audition-lines.py — hear every inflection of a cutie line, and see which ones
a machine can read.

For each take in the vault (voice/jeffrey-phonemes/phrases/cutie-<line>-*),
plays the spoken take, then the same take sung on the line's first phrase
from out/.cutie-line-jobs.json. whisper.cpp reads the sung one back.

  pop/.venv/bin/python pop/wattajetta/bin/audition-lines.py [lead|back]
Writes out/cutie-inflections-<line>.mp3 and prints the order and the recall.
"""
import difflib, importlib.util, json, os, re, subprocess, sys, tempfile, warnings
warnings.filterwarnings("ignore")
from pathlib import Path
import numpy as np
from scipy.signal import resample_poly

HERE = Path(__file__).resolve().parent
OUT = HERE.parent / "out"
LINE = next((a for a in sys.argv[1:] if not a.startswith("-")), "lead")
J = json.load(open(OUT / ".cutie-line-jobs.json"))
SR, LYRIC = J["sr"], J["lines"][LINE]
NSYL = len(re.split(r"[ -]", LYRIC))
job = next(j for j in J["jobs"] if j["line"] == LINE and len(j["midis"]) == NSYL)

sys.argv = ["voice.py", "--arr=radio"]
spec = importlib.util.spec_from_file_location("avoice", HERE.parents[1] / "aesthetina/bin/voice.py")
V = importlib.util.module_from_spec(spec); spec.loader.exec_module(V)
PHRASES = V.LIB.parents[1] / "jeffrey-phonemes" / "phrases"
MODEL = os.environ.get("WHISPER_MODEL", str(Path.home() / "Models/ggml-large-v3-turbo-q5_0.bin"))
FFMPEG = "/opt/homebrew/bin/ffmpeg"
norm = lambda s: re.sub(r"[^a-z ]", "", s.lower().replace("-", "")).split()


def spoken(slug):
    raw = subprocess.run([FFMPEG, "-loglevel", "error", "-i", str(PHRASES / f"{slug}.mp3"), "-f", "f32le", "-ac", "1", "-ar", str(SR), "-"], capture_output=True, check=True).stdout
    return np.frombuffer(raw, dtype=np.float32)


def hear(y, d):
    raw, wav = Path(d) / "x.f32", Path(d) / "x.wav"
    y.astype(np.float32).tofile(raw)
    subprocess.run([FFMPEG, "-loglevel", "error", "-y", "-f", "f32le", "-ar", str(SR), "-ac", "1", "-i", str(raw), "-ar", "16000", str(wav)], check=True)
    return subprocess.run(["whisper-cli", "-m", MODEL, "-l", "en", "-np", "-nt", "-f", str(wav)], capture_output=True, text=True).stdout.strip().replace("\n", " ")


def recall(want, got):
    a, b = norm(want), norm(got)
    return sum(m.size for m in difflib.SequenceMatcher(None, a, b).get_matching_blocks()) / max(1, len(a))


gap = lambda s: np.zeros(int(s * SR), np.float32)
reel, at = [], 0.0
with tempfile.TemporaryDirectory() as d:
    for meta in sorted(PHRASES.glob(f"cutie-{LINE}-*.json")):
        slug, cadence = meta.stem, meta.stem.split("-")[-1]
        text = json.load(open(meta))["text"]
        _, y = V.sing_phrase("jeffrey", slug, job["starts"], job["midis"], job["end_hold"], halo=0.0, vib=0.05, nat=0.6)
        y = resample_poly(y, SR, V.PFS).astype(np.float32)
        got = hear(y, d)
        sp = spoken(slug); sp = sp / (np.abs(sp).max() + 1e-9) * 0.9
        print(f"{int(at // 60)}:{at % 60:04.1f}  {cadence:9s} sung recall {recall(LYRIC, got):.2f}  tag+text: {text!r}  heard: {got}")
        part = [sp, gap(0.35), y * 0.9, gap(0.9)]
        reel += part; at += sum(len(p) for p in part) / SR
    raw = Path(d) / "reel.f32"; np.concatenate(reel).tofile(raw)
    out = OUT / f"cutie-inflections-{LINE}.mp3"
    subprocess.run([FFMPEG, "-loglevel", "error", "-y", "-f", "f32le", "-ar", str(SR), "-ac", "1", "-i", str(raw), "-b:a", "192k", str(out)], check=True)
print(out)
