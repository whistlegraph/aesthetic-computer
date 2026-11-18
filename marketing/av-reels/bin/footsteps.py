#!/usr/bin/env python3
"""footsteps.py — walk in time with the music: footsteps as the metronome.

    python footsteps.py in.mp4 events.json out.wav steps.json [--period SEC] [--level DB] [--grass]

Lays a footstep on every beat, phased so one lands on the first note in
events.json, and mixes them under the clip's own audio. steps.json records
the first-note time and the period, which the walking scene takes as
--step-phase and --steps so the bob lands with the sound.

Each step is a low knock (heel), a short scuff of grit, and a softer second
scuff as the sole rolls down; left and right alternate, slightly panned.
Needs numpy and ffmpeg.
"""
import sys, json, subprocess, wave, numpy as np

src, events, out_wav, out_json = sys.argv[1:5]
opt = lambda k, d: float(sys.argv[sys.argv.index(k) + 1]) if k in sys.argv else d
period = opt("--period", 0.666); level = opt("--level", -15.0)
ev = json.load(open(events))
t0 = ev["keys"][0]["t"] if ev.get("keys") else (ev["touches"][0]["t"] if ev.get("touches") else 0.0)   # a step lands on the first note, or the first touch
GRASS = "--grass" in sys.argv
sr = 48000
pcm = subprocess.run(["ffmpeg", "-v", "error", "-i", src, "-ac", "2", "-ar", str(sr), "-f", "f32le", "-"], capture_output=True).stdout
music = np.frombuffer(pcm, np.float32).reshape(-1, 2).copy(); dur = len(music) / sr
rng = np.random.default_rng(3)

def lowpass(x, fc):
    a = np.exp(-2 * np.pi * fc / sr); y = np.empty_like(x); z = 0.0
    for i, v in enumerate(x): z = (1 - a) * v + a * z; y[i] = z
    return y

def step(left):
    n = int(0.30 * sr); t = np.arange(n) / sr
    f = 58 + 70 * np.exp(-t / 0.018)
    knock = np.sin(2 * np.pi * np.cumsum(f) / sr) * np.exp(-t / 0.055)
    noise = rng.standard_normal(n); grit = lowpass(noise, 3800) - lowpass(noise, 700)
    heel = grit * np.exp(-t / 0.022) * 0.55
    toe = np.zeros(n); d = int(0.085 * sr); toe[d:] = grit[:n - d] * np.exp(-t[:n - d] / 0.035) * 0.30
    if GRASS:                                   # soft ground: hardly a knock, a longer brush of blades
        swish = (lowpass(noise, 7000) - lowpass(noise, 1800)) * np.exp(-t / 0.075) * 0.7
        s = (knock * 0.28 + swish + toe * 0.8) * rng.uniform(0.8, 1.0)
    else:
        s = (knock * 0.9 + heel + toe) * rng.uniform(0.85, 1.0)
    pan = -0.22 if left else 0.22
    return np.stack([s * (1 - pan), s * (1 + pan)], 1) * 0.5

feet = np.zeros_like(music); times = []; left = True
t = t0 + np.floor(-t0 / period) * period
while t < dur - 0.35:
    if t >= 0:
        i = int(t * sr); s = step(left); feet[i:i + len(s)] += s[:len(feet) - i]; times.append(round(float(t), 3))
    t += period; left = not left
feet *= 10 ** (level / 20) / np.abs(feet).max()
mix = music + feet; mix /= max(1.0, np.abs(mix).max() / 0.97)
w = wave.open(out_wav, "wb"); w.setnchannels(2); w.setsampwidth(2); w.setframerate(sr)
w.writeframes((mix * 32767).astype(np.int16).tobytes()); w.close()
json.dump({"t0": t0, "period": period, "stepsPerSecond": 1 / period, "steps": times}, open(out_json, "w"))
print(f"{len(times)} footsteps every {period:.3f}s ({60 / period:.0f} a minute), one on the first note at {t0:.3f}s -> {out_wav}")
