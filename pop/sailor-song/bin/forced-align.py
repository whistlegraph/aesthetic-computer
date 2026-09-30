"""forced-align.py — sung-word boundaries by forced alignment, not recognition.

Whisper GUESSES the words and times them as a side effect: whisper.cpp's -ml 1
splits words into syllable tokens (the labels slide), and whisper-1's word
times drift by half a second on held notes. We already know the lyric, so this
asks a different question: given these exact words, where in her audio does
each one sit? torchaudio's MMS forced aligner (wav2vec2, character CTC) is run
over the whole dry stem with the lyric as the transcript and a <star> token
between lines to soak up guitar bleed and breaths.

Runs in its own venv (torch is not in pop/.venv):
  uv venv -p 3.12 pop/.venv-align && uv pip install -p pop/.venv-align/bin/python torch torchaudio soundfile numpy
  pop/.venv-align/bin/python pop/sailor-song/bin/forced-align.py
    → src/.word-times/forced-align.json   (take seconds; word-times.py --fa reads it)
"""
import json, os, re
import numpy as np, soundfile as sf, torch, torchaudio
from torchaudio.pipelines import MMS_FA as bundle

LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(LANE, "src")
DRY = os.path.join(SRC, "vox", "vocals-dry-48k.wav")
OUT = os.path.join(SRC, ".word-times", "forced-align.json")

lines = [l.strip() for l in open(os.path.join(SRC, "lyrics-sung.txt")) if l.strip() and not l.startswith("[")]
words = [w for l in lines for w in l.split()]
DICT = bundle.get_dict(star="*")
norm = lambda w: "".join(c for c in w.lower().replace("’", "'") if c in DICT and c != "*")

x, sr = sf.read(DRY, dtype="float32", always_2d=True)
wav = torch.from_numpy(x.mean(1))[None]
wav = torchaudio.functional.resample(wav, sr, bundle.sample_rate)
model = bundle.get_model(with_star=True).eval()
with torch.inference_mode():
    # the stem in 30 s windows (overlap 2 s) so memory stays small; emissions are stitched on the frame grid
    S, hop = bundle.sample_rate * 30, bundle.sample_rate * 28
    parts, fps = [], None
    for a in range(0, wav.shape[1], hop):
        e, _ = model(wav[:, a:a + S])
        fps = fps or e.shape[1] / (wav[:, a:a + S].shape[1] / bundle.sample_rate)
        # windows overlap by 2 s: each keeps [a + 1 s, a + 29 s) of its own frames (the first from 0,
        # the last to its end), so the pieces butt together with no frame lost or doubled
        lo = 0 if a == 0 else round(fps)
        hi = e.shape[1] if a + S >= wav.shape[1] else round(29 * fps)
        keep = e[0][lo:hi]
        parts.append(keep)
    emission = torch.cat(parts)[None]
frame_s = wav.shape[1] / bundle.sample_rate / emission.shape[1]

# transcript: every word, and a <star> between lines (guitar-only bars, breaths)
tokens, word_of = [[DICT["*"]]], [None]     # the guitar intro before her first word
for li, l in enumerate(lines):
    if li: tokens.append([DICT["*"]]); word_of.append(None)
    for w in l.split():
        n = norm(w)
        tokens.append([DICT[c] for c in n] if n else [DICT["*"]]); word_of.append(w)
tokens.append([DICT["*"]]); word_of.append(None)   # and the outro after her last
flat = [t for ws in tokens for t in ws]
aligned, scores = torchaudio.functional.forced_align(emission, torch.tensor([flat], dtype=torch.int32), blank=0)
spans = torchaudio.functional.merge_tokens(aligned[0], scores[0].exp())
out, k = [], 0
for ws, w in zip(tokens, word_of):
    sp = spans[k:k + len(ws)]; k += len(ws)
    if w is None: continue
    out.append({"text": w, "from": round(sp[0].start * frame_s, 3), "to": round(sp[-1].end * frame_s, 3),
                "score": round(float(np.mean([s.score for s in sp])), 3),
                "tokens": [{"text": w, "from": round(sp[0].start * frame_s, 3), "to": round(sp[-1].end * frame_s, 3)}]})
json.dump(out, open(OUT, "w"), indent=1)
low = sorted(out, key=lambda w: w["score"])[:8]
print(f"{len(out)} words aligned ({frame_s * 1000:.0f} ms frames) → {OUT}")
print("least sure:", ", ".join(f"{w['text']}@{w['from']:.2f}({w['score']:.2f})" for w in low))
