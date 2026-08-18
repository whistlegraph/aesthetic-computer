#!/usr/bin/env bash
# cut-v3.sh — master the factory v3 render and cut the mp3.
#
# The lane's mastering law, unchanged since v1: MEASURE → one static dB →
# true-peak limiter. Never a second loudnorm — at loud targets it silently
# abandons linear=true and rides gain, which manufactures the sample-step
# artifacts the renderer was careful not to make.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"

FULL="$OUT/factory-remix-v3-full.wav"
SRC="$OUT/factory-remix-v3-master.wav"

if [ ! -f "$SRC" ] || [ "$FULL" -nt "$SRC" ]; then
  echo "→ measure v3"
  STATS=$(ffmpeg -hide_banner -nostats -i "$FULL" \
    -af loudnorm=I=-14:TP=-1.5:LRA=11:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
  MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
  GAIN=$(awk -v i="$MI" 'BEGIN{printf "%.2f", -13.9-i}')
  echo "  measured I=$MI  →  static gain ${GAIN} dB"
  ffmpeg -y -v error -i "$FULL" \
    -af "volume=${GAIN}dB,alimiter=limit=0.84:attack=5:release=100:level=disabled" \
    -ar 48000 -c:a pcm_s24le "$SRC"
fi

ffmpeg -y -v error -i "$SRC" -c:a libmp3lame -b:a 320k \
  -metadata title="whistlegraph factory --- remix (v3, regulated)" \
  -metadata artist="Whistlegraph" -metadata album="pop / factory" \
  -metadata comment="The chant regulated onto its own 100 BPM grid: WORLD snap 0.90 in her own 148.73 Hz D, every word boundary pinned to a measured sung event, the press sidechained into the words, and the machine playing only the melody she sang." \
  "$OUT/factory-remix-v3.mp3"
echo "✓ $OUT/factory-remix-v3.mp3"
