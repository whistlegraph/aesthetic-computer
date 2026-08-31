#!/usr/bin/env bash
# Camille's loner whistlegraph, timestretched onto lonerclub v4pid.
# Retime (retime.py) then remaster: 576x1024 -> 1080x1920 lanczos,
# gentle sharpen + saturation lift, a breath of temporal grain.
set -euo pipefail
cd "$(dirname "$0")"
LONER="$(cd ../.. && pwd)"
WORK="${FERALREEL_WORK:-$HOME/.cache/ac/feralreel}"

python3 retime.py

ffmpeg -y -v error -stats \
  -framerate 30 -i "$WORK/outframes/%05d.png" \
  -i "$LONER/out/lonerclub-v4pid.wav" \
  -vf "scale=1080:1920:flags=lanczos,unsharp=5:5:0.35:5:5:0.0,eq=contrast=1.04:saturation=1.10,format=yuv420p" \
  -r 30 -c:v libx264 -preset medium -crf 18 \
  -c:a aac -b:a 256k -shortest -movflags +faststart \
  "$LONER/out/lonerclub-v4pid-feralreel.mp4"

echo "feralreel -> $LONER/out/lonerclub-v4pid-feralreel.mp4"
