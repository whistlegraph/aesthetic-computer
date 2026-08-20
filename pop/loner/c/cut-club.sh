#!/usr/bin/env bash
# cut-club.sh — Lonerclub, mastered hot.
#
# @jeffrey: "make a nice hot club mix". cut-v4.sh targets −14 LUFS, which
# is a release level; a club plays loud and wants the record to arrive
# already committed. This targets −9.
#
# It keeps the lane's mastering law — MEASURE → one static dB → true-peak
# limiter, never a second loudnorm — and buys the extra 5 dB the honest
# way rather than by slamming the limiter with it:
#
#   1. a gentle bus COMPRESSOR first, so the limiter is catching peaks
#      rather than doing the levelling. 5 dB of limiting with no
#      compression is where a master starts to flap.
#   2. a shelf pair, not a smile: +1.2 dB under 90 Hz for the floor and a
#      NARROW +1 dB at 2.8 kHz for her consonants. No broad treble lift —
#      pop/wattajetta: master treble boosts read as tang on laptop
#      speakers, and this cut has bells in the sixth octave now.
#   3. the limiter sits at 0.82, not at the ceiling. mp3 encoding
#      invents inter-sample peaks: limiting to −1.0 dBFS measured −0.3
#      dBTP after the encode. Leaving 1.7 dB of headroom in the wav is
#      what actually lands the MP3 under −1. Louder than that is not
#      loudness, it is distortion on someone's phone.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"

FULL="${1:-$OUT/loner-remix-v4-full.wav}"
DEST="${2:-$OUT/lonerclub.mp3}"
TARGET="${TARGET:--9.0}"

echo "→ measure $(basename "$FULL")"
STATS=$(ffmpeg -hide_banner -nostats -i "$FULL" \
  -af loudnorm=I="$TARGET":TP=-1.0:LRA=9:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
# leave room for the compressor to find; the static move is the rest.
# 2.2 dB was right when the bus glue was the ONLY levelling in the chain.
# The vox bus now carries its own compressor (lonerremix.c, "THE VOCAL
# CHAIN"), so asking this one for the same work again squashed LRA to 1.9
# — a flat master. It only has to catch peaks now.
GAIN=$(awk -v i="$MI" -v t="$TARGET" 'BEGIN{printf "%.2f", t-i-1.1}')
echo "  measured I=$MI  →  static ${GAIN} dB, then glue, then limit"

ffmpeg -y -v error -i "$FULL" -af "\
volume=${GAIN}dB,\
acompressor=threshold=0.22:ratio=1.5:attack=30:release=240:makeup=1.1:knee=6,\
equalizer=f=90:t=q:w=0.9:g=1.2,\
equalizer=f=2800:t=q:w=1.6:g=1.0,\
alimiter=limit=0.82:attack=4:release=90:level=disabled" \
  -ar 48000 -c:a pcm_s24le "$OUT/lonerclub-master.wav"

ffmpeg -y -v error -i "$OUT/lonerclub-master.wav" -c:a libmp3lame -b:a 320k \
  -metadata title="Lonerclub" \
  -metadata artist="Whistlegraph" -metadata album="pop / loner" \
  -metadata comment="Camille Klein's whistlegraph 'loner', regulated onto a strict 122 floor: one sung sentence looped five times, harmony accumulating. WORLD snap 0.92 with nervox tremor, per-word beat chart, FEM bells, her own backup 3rds and 5ths." \
  "$DEST"

ffmpeg -hide_banner -nostats -i "$DEST" -af ebur128=peak=true -f null - 2>&1 \
  | grep -E "^\s+(I|LRA|Peak):"
echo "✓ $DEST"
