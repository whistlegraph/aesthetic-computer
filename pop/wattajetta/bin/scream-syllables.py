#!/usr/bin/env python3
"""
scream-syllables.py — where each syllable of a scream take begins and ends.

The sung takes get their syllable times from ElevenLabs' alignment; the scream takes
(bin/cutie-screams.mjs) were made without one. Here whisper.cpp gives each word's span, and a word of
two syllables is split at the quietest point of its middle (30–70 % of the word). Written beside the
take as phrases/cutie-scream-<id>.syl.json: {"words": [...], "syllables": [[start s, end s], ...]}.

  pop/.venv/bin/python pop/wattajetta/bin/scream-syllables.py [id ...]
"""
import json, os, subprocess, sys, tempfile
from pathlib import Path
import numpy as np

PHRASES = Path(os.environ.get("AC_VAULT", Path(__file__).resolve().parents[3] / "aesthetic-computer-vault")) / "voice/jeffrey-phonemes/phrases"
MODEL = os.environ.get("WHISPER_MODEL", str(Path.home() / "Models/ggml-large-v3-turbo-q5_0.bin"))
FF = "/opt/homebrew/bin/ffmpeg"
# syllables in each word whisper hears, in order ("attacks" is "a tax")
COUNTS = {
    "title-a": [1, 1, 2, 2, 1, 1], "title-b": [1, 1, 2, 2, 1, 1], "tax-a": [1, 2, 2],
    "nothing-a": [1, 1, 1, 2, 2], "nothing-b": [1, 2, 1, 2], "better-a": [2, 2, 1, 1, 2],
    "cutie-a": [2, 2], "lip-a": [1, 1, 2, 1, 2, 1],
}
for sid in sys.argv[1:] or COUNTS:
    mp3 = PHRASES / f"cutie-scream-{sid}.mp3"
    with tempfile.TemporaryDirectory() as d:
        wav = f"{d}/x.wav"
        subprocess.run([FF, "-loglevel", "error", "-y", "-i", str(mp3), "-ac", "1", "-ar", "16000", wav], check=True)
        subprocess.run(["whisper-cli", "-m", MODEL, "-l", "en", "-np", "-ml", "1", "-sow", "-oj", "-of", f"{d}/w", "-f", wav], capture_output=True, check=True)
        words = [(s["text"].strip(), s["offsets"]["from"] / 1000, s["offsets"]["to"] / 1000) for s in json.load(open(f"{d}/w.json"))["transcription"] if s["text"].strip(" !?.,")]
        x = np.frombuffer(open(wav, "rb").read()[44:], dtype=np.int16).astype(np.float64)
    if len(words) != len(COUNTS[sid]):
        print(f"{sid}: whisper heard {len(words)} words ({' '.join(w for w, _, _ in words)}), expected {len(COUNTS[sid])} — skipped"); continue
    hop = 80; env = np.array([np.sqrt(np.mean(x[i:i + 320] ** 2) + 1) for i in range(0, len(x) - 320, hop)])
    env = np.convolve(env, np.hanning(9) / np.hanning(9).sum(), "same")
    syl = []
    for (w, a, b), n in zip(words, COUNTS[sid]):
        if n == 1: syl.append([a, b]); continue
        i0, i1 = int((a + 0.3 * (b - a)) * 16000 / hop), int((a + 0.7 * (b - a)) * 16000 / hop)
        cut = (i0 + int(np.argmin(env[i0:max(i0 + 1, i1)]))) * hop / 16000
        syl += [[a, round(cut, 3)], [round(cut, 3), b]]
    json.dump({"words": [w for w, _, _ in words], "syllables": syl}, open(PHRASES / f"cutie-scream-{sid}.syl.json", "w"), indent=1)
    print(f"{sid}: {len(syl)} syllables · " + " ".join(f"{a:.2f}-{b:.2f}" for a, b in syl))
