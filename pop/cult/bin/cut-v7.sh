#!/usr/bin/env bash
# cut-v7.sh — master the full v7 render once, then cut windows out of it.
#
# WHY IT IS BUILT THIS WAY: ffmpeg's loudnorm, asked for a loud target,
# silently abandons linear=true and starts riding gain, which manufactures
# sample-step artifacts that a click scan then finds. So this measures the
# render ONCE, applies ONE STATIC dB of gain, and puts a limiter after it.
# There is never a second loudnorm pass, and there is no master tanh.
#
# v7's ten acts, for choosing START:
#   I   carrier    0:00      VI   spread      2:08
#   II  three      0:16      VII  whole       2:32
#   III message    0:48      VIII recognise   3:12
#   IV  secret     1:20      IX   humans      3:28
#   V   reply      1:36      X    carrieroff  3:44
#
#   ./pop/cult/bin/cut-v7.sh                       # full + 0:50 + 2:40 cuts
#   START=80 DUR=40 SUFFIX=-secret ./pop/cult/bin/cut-v7.sh
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"
COVER="$LANE/art/out/cult-remix-v4-cover.png"

FULL="$OUT/cult-remix-v7-full.wav"
SRC="$OUT/cult-remix-v7-full-master.wav"

if [ ! -f "$SRC" ] || [ "$FULL" -nt "$SRC" ]; then
  echo "→ measure full v7"
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

# ── the full record ────────────────────────────────────────────────────
mp3 "$SRC" "$OUT/cult-remix-v7.mp3" \
  "whistlegraph cult --- remix (v7, the descent)" \
  "Ten acts. The polyrhythms rise as the voices sink and stair-step: a signal goes out, and it is answered."

# ── the windows ────────────────────────────────────────────────────────
cut () { # <start> <dur> <suffix> <title> <comment>
  local START=$1 DUR=$2 SUFFIX=$3 FADE=1.2
  local WAV="$OUT/cult-remix-v7${SUFFIX}-master.wav"
  local MP3="$OUT/cult-remix-v7${SUFFIX}.mp3"
  local FADE_AT
  FADE_AT=$(awk -v d="$DUR" -v f="$FADE" 'BEGIN{printf "%.3f", d-f}')
  ffmpeg -y -v error -ss "$START" -t "$DUR" -i "$SRC" \
    -af "afade=t=in:st=0:d=0.02,afade=t=out:st=${FADE_AT}:d=${FADE},alimiter=limit=0.78:attack=5:release=90:level=disabled" \
    -ar 48000 -c:a pcm_s24le "$WAV"
  mp3 "$WAV" "$MP3" "$4" "$5"
}

cut "${START:-48.0}" "${DUR:-50.0}" "${SUFFIX:--50s}" \
  "whistlegraph cult --- remix (v7, 50s cut)" \
  "0:48–1:38 — act III, the message, into the turn"

if [ -z "${SUFFIX:-}" ]; then
  cut 48.0 160.0 "-extended" \
    "whistlegraph cult --- remix (v7, 2:40 cut)" \
    "0:48–3:28 — acts III–VII: the message, the secret, the reply, the spread, the whole message. Hear the dashes descend and the cross-rhythms take the space they leave."
fi
