"""Warp the Feral File take onto the lonerclub v4pid timeline.

viz/wordclock.json maps each sung word to the pen stroke that draws it
(t = track time in the released master, v = stroke time in the source
take). Three passes of the sentence; between them the video REWINDS —
the loner un-draws itself through each click-rush door — and the outro
rewinds all the way to blank paper so the reel loops seamlessly.

Emits hardlinked output frames into $WORK/outframes; run.sh encodes.
"""
import json
import os
import subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
LONER = os.path.dirname(os.path.dirname(HERE))
WORK = os.environ.get("FERALREEL_WORK") or os.path.expanduser(
    "~/.cache/ac/feralreel"
)
SRC = f"{LONER}/source/7108062006980201771.mp4"
AUDIO = f"{LONER}/out/lonerclub-v4pid.wav"
FPS = 30
HOLD = 0.35  # beat of rest on the finished figure before each rewind

frames_dir = f"{WORK}/frames"
out_dir = f"{WORK}/outframes"
os.makedirs(frames_dir, exist_ok=True)
os.makedirs(out_dir, exist_ok=True)

if not os.path.exists(f"{frames_dir}/00001.png"):
    subprocess.run(
        [
            "ffmpeg", "-v", "error", "-i", SRC,
            "-vsync", "0", f"{frames_dir}/%05d.png",
        ],
        check=True,
    )
n_src = len([f for f in os.listdir(frames_dir) if f.endswith(".png")])

duration = float(
    subprocess.run(
        [
            "ffprobe", "-v", "error", "-show_entries", "format=duration",
            "-of", "default=nw=1:nk=1", AUDIO,
        ],
        capture_output=True, text=True, check=True,
    ).stdout
)

words = json.load(open(f"{LONER}/viz/wordclock.json"))
passes = [[words[0]]]
for prev, cur in zip(words, words[1:]):
    if cur["t0"] - prev["t1"] > 2.0:
        passes.append([])
    passes[-1].append(cur)

src_end = (n_src - 1) / FPS

# (track time, source time) control points; source time may run backwards
anchors = [(0.0, 0.0)]
for i, p in enumerate(passes):
    for e in p:
        anchors.append((e["t0"], e["v0"]))
    end_t, end_v = p[-1]["t1"], p[-1]["v1"]
    anchors.append((end_t, end_v))
    if i + 1 < len(passes):
        anchors.append((end_t + HOLD, end_v))  # rest, then rewind
    else:
        # outro: her hand finishes in real footage, then the whole
        # drawing rewinds to blank so the loop lands on frame one
        fwd_end = min(end_t + (src_end - end_v) / 0.4, duration - 2.0)
        anchors.append((fwd_end, src_end))
anchors.append((duration, 0.0))

n_out = round(duration * FPS)
for i in range(n_out):
    t = (i + 0.5) / FPS
    v = anchors[-1][1]
    for (t0, v0), (t1, v1) in zip(anchors, anchors[1:]):
        if t <= t1:
            v = v0 + (v1 - v0) * (t - t0) / max(1e-9, t1 - t0)
            break
    idx = min(max(int(round(v * FPS)), 0), n_src - 1)
    dst = f"{out_dir}/{i + 1:05d}.png"
    if os.path.lexists(dst):
        os.remove(dst)
    os.link(f"{frames_dir}/{idx + 1:05d}.png", dst)

print(
    f"feralreel: {n_out} frames over {duration:.2f}s from {n_src} source "
    f"frames, {len(passes)} passes, doors rewind, loop closes at v=0"
)
