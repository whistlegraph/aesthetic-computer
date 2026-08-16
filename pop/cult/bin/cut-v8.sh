#!/usr/bin/env bash
# cut-v8.sh — master the full v8 render. v8 IS the cut: two minutes, whole.
#
# WHY IT IS BUILT THIS WAY: ffmpeg's loudnorm, asked for a loud target,
# silently abandons linear=true and starts riding gain, which manufactures
# sample-step artifacts that a click scan then finds. So this measures the
# render ONCE, applies ONE STATIC dB of gain, and puts a limiter after it.
# There is never a second loudnorm pass, and there is no master tanh.
#
# v8's ten acts, for choosing START:
#   I   carrier    0:00      VI   spread      1:20
#   II  three      0:08      VII  whole       1:32
#   III message    0:40      VIII recognise   1:44
#   IV  secret     0:56      IX   humans      1:48
#   V   reply      1:04      X    carrieroff  1:56
#
#   ./pop/cult/bin/cut-v8.sh                          # the record
#   START=56 DUR=24 SUFFIX=-secret ./pop/cult/bin/cut-v8.sh
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"
COVER="$LANE/art/out/cult-remix-v4-cover.png"

FULL="$OUT/cult-remix-v8-full.wav"
SRC="$OUT/cult-remix-v8-full-master.wav"

if [ ! -f "$SRC" ] || [ "$FULL" -nt "$SRC" ]; then
  echo "→ measure full v8"
  STATS=$(ffmpeg -hide_banner -nostats -i "$FULL" \
    -af loudnorm=I=-14:TP=-1.2:LRA=9:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
  MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
  GAIN=$(awk -v i="$MI" 'BEGIN{printf "%.2f", -13.9-i}')
  echo "  measured I=$MI  →  static gain ${GAIN} dB"
  ffmpeg -y -v error -i "$FULL" \
    -af "volume=${GAIN}dB,alimiter=limit=0.78:attack=5:release=90:level=disabled" \
    -ar 48000 -c:a pcm_s24le "$SRC"
fi

mp3 () { # <wav> <mp3> <title> <comment>
  ffmpeg -y -v error -i "$1" -i "$COVER" \
    -map 0:a -map 1:v -c:v mjpeg -disposition:v attached_pic -c:a libmp3lame -b:a 320k \
    -metadata title="$3" -metadata artist="Whistlegraph" -metadata album="pop / cult" \
    -metadata comment="$4" "$2"
  echo "✓ $2"
}

mp3 "$SRC" "$OUT/cult-remix-v8.mp3" \
  "whistlegraph cult --- remix (v8)" \
  "Two minutes. Half of the first forty seconds is morse with no words in it; the cults are licked rather than said; the phone plays all twelve keypad tones on a dotted-eighth grid."

# An optional window, for auditioning one act.
if [ -n "${SUFFIX:-}" ]; then
  FADE=1.2
  FADE_AT=$(awk -v d="${DUR:-24}" -v f="$FADE" 'BEGIN{printf "%.3f", d-f}')
  WAV="$OUT/cult-remix-v8${SUFFIX}-master.wav"
  ffmpeg -y -v error -ss "${START:-0}" -t "${DUR:-24}" -i "$SRC" \
    -af "afade=t=in:st=0:d=0.02,afade=t=out:st=${FADE_AT}:d=${FADE},alimiter=limit=0.78:attack=5:release=90:level=disabled" \
    -ar 48000 -c:a pcm_s24le "$WAV"
  mp3 "$WAV" "$OUT/cult-remix-v8${SUFFIX}.mp3" \
    "whistlegraph cult --- remix (v8${SUFFIX})" "window ${START:-0}s +${DUR:-24}s"
fi
