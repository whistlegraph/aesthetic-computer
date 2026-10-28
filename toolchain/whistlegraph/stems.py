#!/usr/bin/env python3
# stems.py — split a clip into vocals.wav and beat.wav with Demucs.
#
#   ~/.local/pipx/venvs/demucs/bin/python stems.py <clip> <outdir>
#
# Run with the pipx Demucs venv's python. Its torchaudio routes file I/O
# through torchcodec, whose dylib fails to load on this Mac, so the CLI
# separates and then dies saving. Here ffmpeg does both ends and Demucs
# only sees tensors. beat.wav is drums + bass + other — the backing the
# whistlegraph was sung over.

import subprocess
import sys
from pathlib import Path

import numpy as np
import torch
from demucs.apply import apply_model
from demucs.pretrained import get_model

SR = 44100
src, outdir = sys.argv[1], Path(sys.argv[2])
outdir.mkdir(parents=True, exist_ok=True)

raw = subprocess.run(["ffmpeg", "-v", "error", "-i", src, "-f", "f32le", "-ac", "2", "-ar", str(SR), "-"],
                     capture_output=True, check=True).stdout
x = torch.from_numpy(np.frombuffer(raw, np.float32).reshape(-1, 2).T.copy())
model = get_model("htdemucs")
model.eval()
ref = x.mean(0)
with torch.no_grad():
    out = apply_model(model, ((x - ref.mean()) / ref.std())[None], device="cpu", split=True, overlap=0.25)[0]
out = out * ref.std() + ref.mean()
vocals = out[model.sources.index("vocals")]
for name, stem in (("vocals", vocals), ("beat", out.sum(0) - vocals)):
    subprocess.run(["ffmpeg", "-v", "error", "-y", "-f", "f32le", "-ar", str(SR), "-ac", "2", "-i", "-",
                    str(outdir / f"{name}.wav")],
                   input=stem.numpy().T.astype(np.float32).tobytes(), check=True)
print(f"✓ {outdir}/vocals.wav, beat.wav")
