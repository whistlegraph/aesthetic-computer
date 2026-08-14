#!/usr/bin/env bash
# render.sh — score the cult remix, then master it.
#
# The renderer deliberately leaves the sum un-clipped and only linearly
# trimmed, so loudness lives here and nowhere else.
#
# Mastering is measure → *static* gain → true-peak limiter, not a second
# loudnorm pass. loudnorm with a target this loud silently abandons
# `linear=true` and starts riding gain, which lifts the quiet intro by
# ~20 dB and manufactures sample-to-sample steps the renderer had already
# been careful not to make (caught by bin/qc.mjs at 8.79 s). One static
# dB figure plus a limiter keeps the render's own dynamics intact.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"
RAW="$OUT/cult-remix.wav"
WAV="$OUT/cult-remix-master.wav"
MP3="$OUT/cult-remix.mp3"

node "$HERE/render.mjs"

TARGET_I=-13.6            # lands ~-13.5 LUFS after the limiter's own lift
CEILING=0.78              # -2.16 dBFS, so 320k lame overshoot still clears -1.2 dBTP

echo "→ measure"
STATS=$(ffmpeg -hide_banner -nostats -i "$RAW" \
  -af loudnorm=I=-14:TP=-1.2:LRA=9:print_format=json -f null - 2>&1 |
  awk '/^\{/,/^\}/')
get() { echo "$STATS" | grep "\"$1\"" | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/'; }
MI=$(get input_i); MTP=$(get input_tp); MLRA=$(get input_lra)
GAIN=$(awk -v i="$MI" -v t="$TARGET_I" 'BEGIN{printf "%.2f", t-i}')
echo "  measured  I=$MI TP=$MTP LRA=$MLRA  →  static gain ${GAIN} dB"

echo "→ master → 24-bit wav"
ffmpeg -y -v error -i "$RAW" \
  -af "volume=${GAIN}dB,alimiter=limit=${CEILING}:attack=4:release=60:level=disabled" \
  -ar 48000 -c:a pcm_s24le "$WAV"

echo "→ master → mp3"
ffmpeg -y -v error -i "$WAV" -c:a libmp3lame -b:a 320k \
  -metadata title="whistlegraph cult --- remix" \
  -metadata artist="Whistlegraph" \
  -metadata album="pop / cult" \
  -metadata comment="club remix of 'The Three of Us Are in a Cult' (2022)" \
  "$MP3"

echo "✓ $WAV"
echo "✓ $MP3"
