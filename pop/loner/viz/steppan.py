#!/usr/bin/env python3
# steppan.py — the lead vocal's panning story: fast 16th-quantized step pans
# (constant-power, grid-locked to 122) with a breathing depth — shallow in the
# open, hard in the big pass — before the phaser pass adds the warble.
import numpy as np, subprocess, sys

sr = 48000
SRC, DST = sys.argv[1], sys.argv[2]
BEAT = 60.0/122; SIX = BEAT/4; BAR = 4*BEAT
SEAM = 31.826

raw = subprocess.run(["ffmpeg","-v","error","-i",SRC,"-ac","2","-ar",str(sr),
                      "-f","f32le","-"],capture_output=True).stdout
x = np.frombuffer(raw,np.float32).reshape(-1,2).astype(np.float64)
n = len(x)
t = np.arange(n)/sr

# 16th index → pan pattern: L R L R L L R R (per half-bar), scaled by depth
PATTERN = np.array([-1,1,-1,1,-1,-1,1,1],dtype=np.float64)
step = (t/SIX).astype(int)
pan = PATTERN[step % 8]
depth = np.where(t < SEAM, 0.35, 0.55)          # the story deepens at the drop
# ease each step edge over 4 ms so the pan steps don't click (no zipper noise)
edge = int(0.004*sr)
kernel = np.ones(edge)/edge
pan = np.convolve(pan*depth, kernel, mode="same")

mono = x.mean(axis=1)
theta = (pan+1)*np.pi/4
out = np.stack([mono*np.cos(theta), mono*np.sin(theta)],1)
# keep a center anchor so she never fully leaves the middle
out = 0.8*out + 0.2*np.stack([mono*0.707, mono*0.707],1)
out.astype(np.float32).tofile("/tmp/steppan.raw")
subprocess.run(["ffmpeg","-y","-v","error","-f","f32le","-ar",str(sr),"-ac","2",
    "-i","/tmp/steppan.raw","-af","aphaser=in_gain=0.9:out_gain=1.0:delay=3.0:decay=0.45:speed=0.55:type=t",
    "-ar",str(sr),"-c:a","pcm_s24le",DST],check=True)
print(DST)
