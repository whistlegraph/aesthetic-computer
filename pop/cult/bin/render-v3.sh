#!/usr/bin/env bash
# render-v3.sh — score the chorus-first cult remix, then master it.
#
# Mastering law is unchanged from v1/v2 and for the same reason: at a loud
# target ffmpeg's `loudnorm` silently abandons `linear=true` and starts
# riding gain, which lifts quiet passages ~20 dB and manufactures
# sample-to-sample steps the renderer was careful never to make (bin/qc.mjs
# caught one at 8.79 s on v1). So: MEASURE → one *static* dB → true-peak
# limiter. Never a second loudnorm pass.
#
# v1's and v2's outputs are left alone — this writes `-v3` files beside them.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"
RAW="$OUT/cult-remix-v3.wav"
WAV="$OUT/cult-remix-v3-master.wav"
MP3="$OUT/cult-remix-v3.mp3"

node "$HERE/sing.mjs"          # speech-to-singing bank (cached; free on reruns)
node --max-old-space-size=8192 "$HERE/render3.mjs" "$@"

TARGET_I=-13.9
CEILING=0.78              # -2.16 dBFS, so 320k lame overshoot still clears -1 dBTP

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
  -af "volume=${GAIN}dB,alimiter=limit=${CEILING}:attack=5:release=90:level=disabled" \
  -ar 48000 -c:a pcm_s24le "$WAV"

echo "→ master → mp3"
ffmpeg -y -v error -i "$WAV" -c:a libmp3lame -b:a 320k \
  -metadata title="whistlegraph cult --- remix (v3, chorus first)" \
  -metadata artist="Whistlegraph" \
  -metadata album="pop / cult" \
  -metadata comment="chill techno remix of 'The Three of Us Are in a Cult' (2022) — chorus first, words sung by WORLD" \
  "$MP3"

echo "✓ $WAV"
echo "✓ $MP3"
