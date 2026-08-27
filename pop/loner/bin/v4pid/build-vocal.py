import os
import subprocess

import numpy as np

S = os.environ.get("V4PID_WORK") or os.path.expanduser("~/.cache/ac/v4pid")
os.makedirs(S, exist_ok=True)
sr = 48000


def load(path, channels=2):
    raw = subprocess.run(
        [
            "ffmpeg", "-v", "error", "-i", path,
            "-ar", str(sr), "-ac", str(channels), "-f", "f32le", "-",
        ],
        capture_output=True,
        check=True,
    ).stdout
    audio = np.frombuffer(raw, np.float32).astype(np.float64)
    return audio.reshape(-1, channels) if channels == 2 else audio


# Keep one direct lead in a fixed position. The selected whole-line take still
# replaces pass two, but chopped repeats, reverse fragments, phrase-long hums,
# and vocal panning are retired: the arrangement moves around the singer.
vox = load(f"{S}/sep2/htdemucs/v4pid-trim/vocals.wav").copy()
n = len(vox)
mono = vox.mean(axis=1)
take = load("pop/loner/vox4/w-whole-line.wav", channels=1)

lead = mono[int(1.0 * sr):int(30.0 * sr)]
lead_active = lead[np.abs(lead) > 0.01]
take_active = take[np.abs(take) > 0.01]
gain = np.sqrt((lead_active**2).mean()) / max(
    1e-9, np.sqrt((take_active**2).mean())
)
take *= gain

insert = 31.83 - 0.330
a = int(insert * sr)
b = min(n, a + len(take))
crossfade = int(0.12 * sr)
env = np.ones(b - a)
env[:crossfade] = np.linspace(0, 1, crossfade)
env[-crossfade:] = np.linspace(1, 0, crossfade)
for channel in range(2):
    vox[a:b, channel] = vox[a:b, channel] * (1 - env) + take[:b - a] * env

mono = vox.mean(axis=1)
np.stack([mono, mono], axis=1).astype(np.float32).tofile(f"{S}/vox-arped.raw")
print("vocal v8: one direct centered lead")
