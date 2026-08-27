#!/usr/bin/env bash
# cut-v1.sh — master the v1 render and cut the mp3.
#
# Same mastering law as pop/cult v3/v5/v9/v10: MEASURE → one static dB →
# true-peak limiter. Never a second loudnorm — at loud targets it silently
# abandons linear=true and starts riding gain, which manufactures
# sample-step artifacts the renderer was careful never to make.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"

FULL="$OUT/season-remix-v1.wav"
SRC="$OUT/season-remix-v1-master.wav"

if [ ! -f "$SRC" ] || [ "$FULL" -nt "$SRC" ]; then
  echo "→ measure v1"
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
  -metadata title="whistlegraph season --- remix (v1)" \
  -metadata artist="Whistlegraph" -metadata album="pop / season" \
  -metadata comment="it's too hot / no it's not — the argument staged across the stereo field, 122 BPM, key of A with the third left open." \
  "$OUT/season-remix-v1.mp3"
echo "✓ $OUT/season-remix-v1.mp3"
