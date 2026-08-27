import os
import subprocess

import numpy as np

S = os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S, exist_ok=True)
sr = 48000
DURATION = 94.2
NT = int(DURATION * sr)

raw = subprocess.run(
    ["ffmpeg", "-v", "error", "-i", f"{S}/stamp-jsnapped.wav",
     "-ar", str(sr), "-ac", "1", "-f", "f32le", "-"],
    capture_output=True,
    check=True,
).stdout
source = np.frombuffer(raw, np.float32).astype(np.float64)

# A small upward resample makes the tag lighter and short enough to live
# between the final vowel release and the club downbeat.
indices = np.arange(0, len(source) - 1, 1.12)
tag = np.interp(indices, np.arange(len(source)), source) * 0.55
fade = int(0.045 * sr)
tag[:fade] *= np.linspace(0, 1, fade)
tag[-fade:] *= np.linspace(1, 0, fade)

stamp = np.zeros((NT, 2))
start = 61.78
a = int(start * sr)
b = min(NT, a + len(tag))
stamp[a:b, 0] += tag[:b - a]
stamp[a:b, 1] += tag[:b - a]

# A quiet A#/C# floor gathers beneath the tag and carries through the drop.
tone_start = 61.55
ta = int(tone_start * sr)
tt = np.arange(NT - ta) / sr
tenv = (1 - np.exp(-tt / 0.22)) * np.exp(-tt / 1.8)
tone = (
    np.sin(2 * np.pi * 116.54 * tt)
    + 0.55 * np.sin(2 * np.pi * 138.59 * tt)
) * tenv * 0.018
stamp[ta:, 0] += tone
stamp[ta:, 1] += tone

stamp.astype(np.float32).tofile(f"{S}/stem-stamp.raw")
print(f"jeffrey stamp {len(tag) / sr:.2f}s at {start:.2f}, pitched up")
