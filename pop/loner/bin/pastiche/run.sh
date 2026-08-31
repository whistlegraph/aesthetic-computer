#!/usr/bin/env bash
# Pastiche reel: build.py cuts + crossfades the two-pass collage,
# this grades it ethereal (ghost trail, soft bloom, lifted blacks) and
# upscales to 1080x1920, then chrome-reel.mjs lays the lyric words and
# the radial track overlay and muxes the two-pass audio.
set -euo pipefail
cd "$(dirname "$0")"
LONER="$(cd ../.. && pwd)"
WORK="${PASTICHE_WORK:-$HOME/.cache/ac/pastiche}"
END=63.788  # two vocal passes, cut on the second door

python3 build.py

ffmpeg -y -v error -stats \
  -i "$WORK/collage.mp4" \
  -filter_complex "\
[0:v]tmix=frames=3:weights='6 3 2',split[base][glow];\
[glow]gblur=sigma=24[soft];\
[base][soft]blend=all_mode=screen:all_opacity=0.20,\
curves=all='0/0.04 0.5/0.52 1/0.97',\
eq=contrast=1.03:saturation=1.06,\
scale=1080:1920:flags=lanczos,unsharp=5:5:0.3:5:5:0.0,format=yuv420p[v]" \
  -map "[v]" -an -r 30 -c:v libx264 -preset fast -crf 17 \
  "$WORK/graded.mp4"

ffmpeg -y -v error -i "$LONER/out/lonerclub-v4pid.wav" \
  -t $END -af "afade=t=out:st=63.0:d=0.788" "$WORK/audio-reel.wav"

# the performed recording is the score's source of truth when present
if [ -f "$LONER/viz/wg-perform.json" ]; then
  python3 wizard-export.py
else
  python3 trace-strokes.py
fi
node chrome-reel.mjs

echo "reel -> $LONER/out/lonerclub-v4pid-reel.mp4"
