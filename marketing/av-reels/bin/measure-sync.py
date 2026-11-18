#!/usr/bin/env python3
"""measure-sync.py — how far apart are sound and picture in a capture?

    python measure-sync.py base.mp4 [--from SEC]

For a piece whose keys light when they sound (notepat), this lists when each
key lights in the picture and when each note attacks in the audio, pairs
them in order, and prints the offset. Positive = the picture is late.

Picture: a key light is a burst of new strongly coloured pixels against a
quiet reference frame. Sound: an attack is a jump in high-frequency-weighted
energy. Both are found independently, so a bad mux cannot hide.
Needs numpy and ffmpeg.
"""
import subprocess, sys, numpy as np

path = sys.argv[1]
start = float(sys.argv[sys.argv.index("--from") + 1]) if "--from" in sys.argv else 0.0
W, H = 270, 480
fps = eval(subprocess.run(["ffprobe", "-v", "error", "-select_streams", "v:0", "-show_entries", "stream=r_frame_rate",
                           "-of", "csv=p=0", path], capture_output=True, text=True).stdout.strip())
raw = subprocess.run(["ffmpeg", "-v", "error", "-i", path, "-vf", f"scale={W}:{H}", "-f", "rawvideo", "-pix_fmt", "rgb24", "-"],
                     capture_output=True).stdout
fr = np.frombuffer(raw, np.uint8).reshape(-1, H, W, 3).astype(np.int16)
lit = lambda f: ((f.max(2) - f.min(2)) > 90) & (f.max(2) > 150)
i0 = int(start * fps)
counts = np.array([int(lit(f).sum()) for f in fr])
base = np.percentile(counts[i0:], 10)                       # the keyboard at rest
lights, on = [], False
for i in range(i0, len(fr)):
    hot = counts[i] > base + 600
    if hot and not on:
        ys, xs = np.nonzero(lit(fr[i]))
        lights.append((i / fps, xs.mean() / W))
    elif hot and on and i > 0:
        # a different key lighting straight after: the lit centre jumps
        a = np.nonzero(lit(fr[i])); b = np.nonzero(lit(fr[i - 1]))
        if len(a[1]) and len(b[1]) and abs(np.median(a[1]) - np.median(b[1])) > 14: lights.append((i / fps, a[1].mean() / W))
    on = hot

pcm = subprocess.run(["ffmpeg", "-v", "error", "-i", path, "-ac", "1", "-ar", "48000", "-f", "f32le", "-"], capture_output=True).stdout
x = np.frombuffer(pcm, np.float32); sr = 48000; win, hop = 480, 96
frames = np.lib.stride_tricks.sliding_window_view(x, win)[::hop] * np.hanning(win)
spec = np.abs(np.fft.rfft(frames, axis=1)); wgt = np.arange(spec.shape[1])
flux = np.clip(np.diff((spec * wgt).sum(1), prepend=0), 0, None); t = np.arange(len(flux)) * hop / sr
thr = 0.2 * flux[t > start].max(); attacks = []
for i in range(1, len(flux) - 1):
    if t[i] > start and flux[i] > thr and flux[i] >= flux[i - 1] and flux[i] >= flux[i + 1] and (not attacks or t[i] - attacks[-1] > 0.15):
        attacks.append(t[i])

print(f"{path}: {len(lights)} key lights, {len(attacks)} note attacks (from {start}s)")
print("  lights :", " ".join(f"{a:.2f}" for a, _ in lights[:12]))
print("  attacks:", " ".join(f"{a:.2f}" for a in attacks[:12]))
# Pair each light with the nearest attack. A light is first seen in a frame,
# so its true time lies up to one frame earlier — use the frame midpoint.
pairs, lonely = [], 0
for lt, _ in lights:
    mid = lt - 0.5 / fps
    near = min(attacks, key=lambda a: abs(a - mid)) if attacks else None
    if near is not None and abs(near - mid) <= 0.15: pairs.append((mid - near) * 1000)
    else: lonely += 1
silent = sum(1 for a in attacks if all(abs(a - (lt - 0.5 / fps)) > 0.15 for lt, _ in lights))
if pairs:
    d = np.array(pairs)
    print(f"  picture minus sound, nearest pairs ({len(d)}): median {np.median(d):+.0f} ms, mean {d.mean():+.0f} ms, range {d.min():+.0f}..{d.max():+.0f} ms")
print(f"  lights with no sound nearby: {lonely}; sounds with no light nearby: {silent}")
ok = bool(pairs) and abs(np.median(pairs)) <= 35 and lonely == 0 and silent == 0
print("  SYNC", "OK" if ok else "CHECK")
sys.exit(0 if ok else 1)
