#!/usr/bin/env bash
# cut-femrag-plusplus.sh — master femrag++ for release and cut the shipping files.
#
# The render's own mp3 pass limits at 0.96 sample-peak, which leaks to
# +0.2 dBTP once the encoder overshoots — fine for auditioning, not legal
# for stores. This cut re-renders the raw float, keeps the lane's tuned
# compressor, then follows the house mastering law (cult v3/v5/v9/v10):
# MEASURE → one static dB → true-peak limiter. Never a second loudnorm.
#
# Ships: out/femrag-plusplus-master.wav (16-bit/44.1 for DistroKid)
#        out/femrag-plusplus-release.mp3 (320k, tagged)
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"

RAW="$OUT/femrag-plusplus.mp3.f32.raw"
PRE="$OUT/femrag-plusplus-pre.wav"
WAV="$OUT/femrag-plusplus-master.wav"
MP3="$OUT/femrag-plusplus-release.mp3"

if [ ! -f "$RAW" ]; then
  echo "→ render (keeping raw float)"
  node "$HERE/render-femrag-plusplus.mjs" --keep-raw
fi

echo "→ compressor pass (the lane's tuned chain, minus its 0.96 limiter)"
ffmpeg -y -v error -f f32le -ar 48000 -ac 2 -i "$RAW" \
  -af "acompressor=threshold=-16dB:ratio=2.4:attack=12:release=90:makeup=2:knee=6" \
  -c:a pcm_s24le "$PRE"

echo "→ measure"
STATS=$(ffmpeg -hide_banner -nostats -i "$PRE" \
  -af loudnorm=I=-14:TP=-1.2:LRA=9:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
GAIN=$(awk -v i="$MI" 'BEGIN{printf "%.2f", -13.5-i}')
echo "  measured I=$MI  →  static gain ${GAIN} dB"

echo "→ static gain + true-peak limiter → 16-bit/44.1 master"
ffmpeg -y -v error -i "$PRE" \
  -af "volume=${GAIN}dB,alimiter=limit=0.78:attack=5:release=90:level=disabled,aresample=44100" \
  -sample_fmt s16 -c:a pcm_s16le "$WAV"

ffmpeg -y -v error -i "$WAV" -c:a libmp3lame -b:a 320k \
  -metadata title="Femrag++" \
  -metadata artist="Aesthetic Dot Computer" -metadata album="pixsies" \
  "$MP3"

echo "→ verify master"
ffmpeg -hide_banner -nostats -i "$WAV" \
  -af "ebur128=peak=true" -f null - 2>&1 | grep -A 12 "Summary:" || true
echo "✓ $WAV"
echo "✓ $MP3"
