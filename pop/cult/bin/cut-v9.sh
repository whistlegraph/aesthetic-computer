#!/usr/bin/env bash
# cut-v9.sh — master the v9 render once, statically, and cut the mp3.
#
# Same mastering law as v3/v5 and for the same reason: at a loud target
# ffmpeg's loudnorm silently abandons linear=true and starts riding gain,
# which manufactures sample-step artifacts that a click scan then finds.
# So: MEASURE → one static dB → true-peak limiter. Never a second loudnorm.
#
# No embedded cover: art/out was never committed and its generator is gone.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"

FULL="$OUT/cult-remix-v9-full.wav"
SRC="$OUT/cult-remix-v9-full-master.wav"

if [ ! -f "$SRC" ] || [ "$FULL" -nt "$SRC" ]; then
  echo "→ measure full v9"
  STATS=$(ffmpeg -hide_banner -nostats -i "$FULL" \
    -af loudnorm=I=-14:TP=-1.2:LRA=9:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
  MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
  GAIN=$(awk -v i="$MI" 'BEGIN{printf "%.2f", -13.9-i}')
  echo "  measured I=$MI  →  static gain ${GAIN} dB"
  ffmpeg -y -v error -i "$FULL" \
    -af "volume=${GAIN}dB,alimiter=limit=0.78:attack=5:release=90:level=disabled" \
    -ar 48000 -c:a pcm_s24le "$SRC"
fi

ffmpeg -y -v error -i "$SRC" -c:a libmp3lame -b:a 320k \
  -metadata title="whistlegraph cult --- remix (v9, the loop and the signal)" \
  -metadata artist="Whistlegraph" -metadata album="pop / cult" \
  -metadata comment="v3 × v5: the four-line chorus as a steady loop through the ten-act signal narrative." \
  "$OUT/cult-remix-v9.mp3"
echo "✓ $OUT/cult-remix-v9.mp3"
