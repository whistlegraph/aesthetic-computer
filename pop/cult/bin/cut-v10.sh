#!/usr/bin/env bash
# cut-v10.sh — master the v10 render and cut the mp3.
#
# v10 starts when the kicks start, keeps two bars of intro, then removes the
# withheld opening and one redundant phrase from each late dense act. Every
# narrative act and impact survives; the release lands near 2:21.
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
  echo "→ 2:21 discovery edit (cut bars 10–28, 60–63, 68–71, 84–91)"
  # Ten-millisecond edge fades keep every edit sample-continuous without
  # shortening the timeline (an acrossfade would move all later receipts).
  ffmpeg -y -v error -i "$FULL" -filter_complex \
    "[0:a]atrim=start=15.95:end=20,asetpts=PTS-STARTPTS,afade=t=out:st=4.04:d=0.01[a];\
[0:a]atrim=start=58:end=120,asetpts=PTS-STARTPTS,afade=t=in:d=0.01,afade=t=out:st=61.99:d=0.01[b];\
[0:a]atrim=start=128:end=136,asetpts=PTS-STARTPTS,afade=t=in:d=0.01,afade=t=out:st=7.99:d=0.01[c];\
[0:a]atrim=start=144:end=168,asetpts=PTS-STARTPTS,afade=t=in:d=0.01,afade=t=out:st=23.99:d=0.01[d];\
[0:a]atrim=start=184,asetpts=PTS-STARTPTS,afade=t=in:d=0.01[e];\
[a][b][c][d][e]concat=n=5:v=0:a=1,afade=t=in:st=0:d=0.02[out]" \
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
  -metadata title="wannadash" \
  -metadata artist="Whistlegraph Dot Org" -metadata album="pixsies" \
  -metadata comment="v10.2: the withholding is over — from the first hook the voice says the whole sentence: dash, i wanna, dash, i wanna, run real fast, dot dot dot. Opens on the kick and the dot crowd; real phone: pickup, rotary, hang-up, busy." \
  "$OUT/cult-remix-v10.mp3"
echo "✓ $OUT/cult-remix-v10.mp3"
