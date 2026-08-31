#!/usr/bin/env bash
# cut-v10.sh — master the v10 render and cut the mp3.
#
# v10 starts when the kicks start, keeps two bars of the three-voice intro,
# then cuts render bars 10–28 so the sentence itself arrives at about 0:04.
# The full renderer and receipt remain archival; this is the release edit.
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

if [ ! -f "$TRIM" ] || [ "$FULL" -nt "$TRIM" ] || [ "$0" -nt "$TRIM" ]; then
  echo "→ two-bar intro edit (keep bars 8–9, cut bars 10–28)"
  ffmpeg -y -v error -i "$FULL" -filter_complex \
    "[0:a]atrim=start=15.95:end=20,asetpts=PTS-STARTPTS[a];\
[0:a]atrim=start=58,asetpts=PTS-STARTPTS[b];\
[a][b]concat=n=2:v=0:a=1,afade=t=in:st=0:d=0.02[out]" \
    -map "[out]" -c:a pcm_s24le "$TRIM"
fi

SPACE="$OUT/.v10-space.wav"
if [ ! -f "$SPACE" ] || [ "$TRIM" -nt "$SPACE" ]; then
  echo "→ cathedral + low shelf (Peep pass)"
  ffmpeg -y -v error -i "$TRIM" -i "$LANE/samples/cathedral-ir.wav" -filter_complex \
    "[0:a]bass=g=2.5:f=95:w=0.6,asplit[dry][s];[s][1:a]afir=dry=0:wet=3[wet];\
[dry][wet]amix=inputs=2:weights='1 0.35':normalize=0[out]" \
    -map "[out]" -ar 48000 -c:a pcm_s24le "$SPACE"
fi

if [ ! -f "$SRC" ] || [ "$SPACE" -nt "$SRC" ]; then
  echo "→ measure trimmed v10"
  STATS=$(ffmpeg -hide_banner -nostats -i "$SPACE" \
    -af loudnorm=I=-14:TP=-1.2:LRA=9:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
  MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
  GAIN=$(awk -v i="$MI" 'BEGIN{printf "%.2f", -13.9-i}')
  echo "  measured I=$MI  →  static gain ${GAIN} dB"
  ffmpeg -y -v error -i "$SPACE" \
    -af "volume=${GAIN}dB,alimiter=limit=0.78:attack=5:release=90:level=disabled" \
    -ar 48000 -c:a pcm_s24le "$SRC"
fi

ffmpeg -y -v error -i "$SRC" -c:a libmp3lame -b:a 320k \
  -metadata title="whistlegraph cult --- remix (v10.2, sentence first)" \
  -metadata artist="Whistlegraph" -metadata album="pop / cult" \
  -metadata comment="v10.2: the withholding is over — from the first hook the voice says the whole sentence: dash, i wanna, dash, i wanna, run real fast, dot dot dot. Opens on the kick and the dot crowd; real phone: pickup, rotary, hang-up, busy." \
  "$OUT/cult-remix-v10.mp3"
echo "✓ $OUT/cult-remix-v10.mp3"
