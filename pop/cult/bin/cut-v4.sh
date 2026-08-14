#!/usr/bin/env bash
# cut-v4.sh — v4 is a 50-second edit of v2, not a new score.
#
# @jeffrey: "cult remix v2 is way better ... trim the v2 to 0:50 seconds ...
# right when that part starts". v2's hook (hookA) lands at bar 24 = 0:48, so
# the cut opens exactly on it. Move START to open on a different section:
#   air 0 · pulse 0:16 · morse 0:32 · hookA 0:48 · hollow 1:20 · hookB 1:36
#   drift 2:08 · hookC 2:32 · descent 3:12 · ebb 3:28 · out 3:44
#
# v4 re-scores v2 with two lyric changes ("run REAL fast"; a harmonised
# "cult" where a bass dash was), so this masters that full render first —
# measure -> one static dB -> limiter, never a second loudnorm — then cuts.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT="$(dirname "$HERE")/out"

START=${START:-48.0}     # 0:48 — the downbeat of hookA
DUR=${DUR:-50.0}         # 0:50
FADE=${FADE:-1.2}        # out-fade so the tail never truncates mid-note

SRC="$OUT/cult-remix-v4-full-master.wav"
SUFFIX=${SUFFIX:-}          # e.g. SUFFIX=-extended writes a second cut beside the 50s one
WAV="$OUT/cult-remix-v4${SUFFIX}-master.wav"
MP3="$OUT/cult-remix-v4${SUFFIX}.mp3"

FULL="$OUT/cult-remix-v4-full.wav"
if [ ! -f "$SRC" ] || [ "$FULL" -nt "$SRC" ]; then
  echo "→ measure full v4"
  STATS=$(ffmpeg -hide_banner -nostats -i "$FULL" \
    -af loudnorm=I=-14:TP=-1.2:LRA=9:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
  MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
  GAIN=$(awk -v i="$MI" 'BEGIN{printf "%.2f", -13.9-i}')
  echo "  measured I=$MI  →  static gain ${GAIN} dB"
  ffmpeg -y -v error -i "$FULL" \
    -af "volume=${GAIN}dB,alimiter=limit=0.78:attack=5:release=90:level=disabled" \
    -ar 48000 -c:a pcm_s24le "$SRC"
fi

FADE_AT=$(awk -v d="$DUR" -v f="$FADE" 'BEGIN{printf "%.3f", d-f}')

ffmpeg -y -v error -ss "$START" -t "$DUR" -i "$SRC" \
  -af "afade=t=in:st=0:d=0.02,afade=t=out:st=${FADE_AT}:d=${FADE},alimiter=limit=0.78:attack=5:release=90:level=disabled" \
  -ar 48000 -c:a pcm_s24le "$WAV"

COVER="$(dirname "$HERE")/art/out/cult-remix-v4-cover.png"
ffmpeg -y -v error -i "$WAV" ${COVER:+-i "$COVER"} \
  -map 0:a ${COVER:+-map 1:v -c:v mjpeg -disposition:v attached_pic} -c:a libmp3lame -b:a 320k \
  -metadata title="whistlegraph cult --- remix (v4, 50s cut)" \
  -metadata artist="Whistlegraph" \
  -metadata album="pop / cult" \
  -metadata comment="50-second edit of v2, opening on the hook" \
  "$MP3"

echo "✓ $WAV"
echo "✓ $MP3  (${START}s +${DUR}s)"
