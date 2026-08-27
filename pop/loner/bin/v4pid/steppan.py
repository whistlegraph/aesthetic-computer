#!/usr/bin/env python3
"""Legacy stage entry point; the final lead remains a stable center anchor."""

import os
import subprocess
import sys

import numpy as np

S = os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S, exist_ok=True)
sr = 48000
src, dst = sys.argv[1], sys.argv[2]

raw = subprocess.run(
    ["ffmpeg", "-v", "error", "-i", src, "-ac", "2", "-ar", str(sr),
     "-f", "f32le", "-"],
    capture_output=True,
    check=True,
).stdout
x = np.frombuffer(raw, np.float32).reshape(-1, 2).astype(np.float64)
mono = x.mean(axis=1)
center = np.stack([mono, mono], axis=1) * (2 ** -0.5)
raw_path = f"{S}/vocal-center.raw"
center.astype(np.float32).tofile(raw_path)
subprocess.run(
    ["ffmpeg", "-y", "-v", "error", "-f", "f32le", "-ar", str(sr),
     "-ac", "2", "-i", raw_path, "-c:a", "pcm_s24le", dst],
    check=True,
)
print(dst)
