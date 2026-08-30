#!/usr/bin/env bash
# cut-v10.sh — master the v10 render and cut the mp3.
#
# v10 starts when the kicks start: the full render still scores the eight
# carrier bars (their drone and dial tails ring under the entrance), and
# the cut begins 50 ms before bar 8's downbeat with a 20 ms fade, so the
# first thing that lands is the watery-hole metallic ring, then the kick. Trim FIRST, then measure — loudnorm's
# integrated reading must not include sixteen seconds we don't ship.
#
# Same mastering law as v3/v5/v9: MEASURE → one static dB → true-peak
# limiter. Never a second loudnorm (it rides gain and manufactures clicks).
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"

FULL="$OUT/cult-remix-v10-full.wav"
TRIM="$OUT/cult-remix-v10-full-trim.wav"
SRC="$OUT/cult-remix-v10-full-master.wav"

if [ ! -f "$TRIM" ] || [ "$FULL" -nt "$TRIM" ]; then
  echo "→ trim to bar 8 (15.95 s in, 20 ms fade)"
  ffmpeg -y -v error -ss 15.95 -i "$FULL" \
    -af "afade=t=in:st=0:d=0.02" -c:a pcm_s24le "$TRIM"
fi

if [ ! -f "$SRC" ] || [ "$TRIM" -nt "$SRC" ]; then
  echo "→ measure trimmed v10"
  STATS=$(ffmpeg -hide_banner -nostats -i "$TRIM" \
    -af loudnorm=I=-14:TP=-1.2:LRA=9:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
  MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
  GAIN=$(awk -v i="$MI" 'BEGIN{printf "%.2f", -13.9-i}')
  echo "  measured I=$MI  →  static gain ${GAIN} dB"
  ffmpeg -y -v error -i "$TRIM" \
    -af "volume=${GAIN}dB,alimiter=limit=0.78:attack=5:release=90:level=disabled" \
    -ar 48000 -c:a pcm_s24le "$SRC"
fi

ffmpeg -y -v error -i "$SRC" -c:a libmp3lame -b:a 320k \
  -metadata title="whistlegraph cult --- remix (v10.2, sentence first)" \
  -metadata artist="Whistlegraph" -metadata album="pop / cult" \
  -metadata comment="v10.2: the withholding is over — from the first hook the voice says the whole sentence: dash, i wanna, dash, i wanna, run real fast, dot dot dot. Opens on the kick and the dot crowd; real phone: pickup, rotary, hang-up, busy." \
  "$OUT/cult-remix-v10.mp3"
echo "✓ $OUT/cult-remix-v10.mp3"
