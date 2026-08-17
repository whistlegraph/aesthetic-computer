#!/usr/bin/env bash
# cut-v4.sh — master the loner v4 render and cut the mp3.
#
# Same mastering law as cult v3+/loner v1–v3: MEASURE → one static dB →
# true-peak limiter. Never a second loudnorm. The dance cut targets
# −14 LUFS (the ballad's −16 was a dynamics decision; a floor wants
# club level) with the limiter still at −1.5 dBTP.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"

FULL="$OUT/loner-remix-v4-full.wav"
SRC="$OUT/loner-remix-v4-master.wav"

if [ ! -f "$SRC" ] || [ "$FULL" -nt "$SRC" ]; then
  echo "→ measure v4"
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
  -metadata title="whistlegraph loner --- remix (v4, regulated)" \
  -metadata artist="Whistlegraph" -metadata album="pop / loner" \
  -metadata comment="Camille regulated onto a strict 122 floor: WORLD snap 0.92, per-word beat chart, her own backup 3rds and 5ths singing the words, the band's pluck playing only her measured melody, kicks sidechained into the lyrics." \
  "$OUT/loner-remix-v4.mp3"
echo "✓ $OUT/loner-remix-v4.mp3"
