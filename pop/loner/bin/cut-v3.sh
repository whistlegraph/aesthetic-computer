#!/usr/bin/env bash
# cut-v3.sh — master the loner v3 render and cut the mp3.
#
# Same mastering law as cult v3/v5/v9/v10: MEASURE → one static dB →
# true-peak limiter. Never a second loudnorm (it rides gain and
# manufactures clicks). The one deliberate difference: the target is
# −16 LUFS, not −14 — a bedroom ballad keeps its dynamics, and the
# limiter at −1.5 dBTP should have almost nothing to do.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"

FULL="$OUT/loner-remix-v3-full.wav"
SRC="$OUT/loner-remix-v3-master.wav"

if [ ! -f "$SRC" ] || [ "$FULL" -nt "$SRC" ]; then
  echo "→ measure v3"
  STATS=$(ffmpeg -hide_banner -nostats -i "$FULL" \
    -af loudnorm=I=-16:TP=-1.5:LRA=11:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
  MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
  GAIN=$(awk -v i="$MI" 'BEGIN{printf "%.2f", -15.9-i}')
  echo "  measured I=$MI  →  static gain ${GAIN} dB"
  ffmpeg -y -v error -i "$FULL" \
    -af "volume=${GAIN}dB,alimiter=limit=0.84:attack=5:release=100:level=disabled" \
    -ar 48000 -c:a pcm_s24le "$SRC"
fi

ffmpeg -y -v error -i "$SRC" -c:a libmp3lame -b:a 320k \
  -metadata title="whistlegraph loner --- remix (v3, angelic drum & bass)" \
  -metadata artist="Whistlegraph" -metadata album="pop / loner" \
  -metadata comment="Camille, angelic and split up: WORLD leads with air and breath, an octave self-choir, diatonic swells, a harp of her own vowels, kicks between her syllables, and a synthesized two-step at 160 over the 80 BPM ballad floor." \
  "$OUT/loner-remix-v3.mp3"
echo "✓ $OUT/loner-remix-v3.mp3"
