#!/usr/bin/env bash
# cut-amazing.sh — the house master law for "amazing grace".
#
#   premaster (tone + glue) → MEASURE → one static dB → true-peak limiter
#   → verify → tagged WAV / FLAC / mp3 → pop/bin/master-audit.mjs
#
# Never a second loudnorm (pop/MASTERING.md). Sine-heavy renders ship
# dark, so the tone stage carries the brightening polish learned on
# trancenwaltz; the target sits in the "wider narrative" band the house
# allows a hymn: −11.5 LUFS, ≤ −2 dBTP.
#
#   PRE=pop/big-pictures/out/amazing-grace/pre.wav bash pop/big-pictures/bin/cut-amazing.sh
#   TARGET=-11.5 LIMIT=0.79 bash …   # both measured, not guessed
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
LANE="$(cd -- "$HERE/.." && pwd)"
REPO="$(cd -- "$LANE/../.." && pwd)"
OUT="${OUT:-$LANE/out/amazing-grace}"
PRE="${PRE:-$OUT/pre.wav}"
TARGET="${TARGET:--11.5}"
LIMIT="${LIMIT:-0.74}"       # leaves reconstruction headroom below −2 dBTP at 48 kHz
SLUG="amazing-grace"
TITLE="amazing grace"
ARTIST="Aesthetic Dot Computer"
ALBUM="pixsies"

[ -f "$PRE" ] || { echo "✗ no premix at $PRE"; exit 1; }
mkdir -p "$OUT"

echo "→ tone + glue"
ffmpeg -y -loglevel error -i "$PRE" -af "\
highpass=f=30,\
equalizer=f=190:t=q:w=1.0:g=-1.0,\
equalizer=f=4200:t=q:w=1.2:g=2.2,\
highshelf=f=8500:g=3.5,\
highshelf=f=12500:g=1.8,\
acompressor=threshold=-18dB:ratio=1.6:attack=30:release=200:knee=4:detection=rms:mix=0.5" \
  -c:a pcm_f32le "$OUT/premaster.wav"

echo "→ measure"
I=$(ffmpeg -hide_banner -nostats -i "$OUT/premaster.wav" \
      -af "loudnorm=I=${TARGET}:TP=-2.0:LRA=9:print_format=json" -f null - 2>&1 \
    | grep '"input_i"' | sed -E 's/.*: *"([-0-9.]+)".*/\1/')
GAIN=$(awk -v t="$TARGET" -v i="$I" 'BEGIN { printf "%.2f", t - i }')
echo "   premaster ${I} LUFS → static gain ${GAIN} dB"

echo "→ limit"
ffmpeg -y -loglevel error -i "$OUT/premaster.wav" -af "\
volume=${GAIN}dB,\
aresample=192000,\
alimiter=limit=${LIMIT}:attack=4:release=120:asc=true:asc_level=0.35:level=false,\
aresample=48000" \
  -c:a pcm_s24le "$OUT/${SLUG}-master.wav"

echo "→ deliverables"
ffmpeg -y -loglevel error -i "$OUT/${SLUG}-master.wav" -sample_fmt s32 \
  -bits_per_raw_sample 24 \
  -c:a flac -compression_level 8 \
  -metadata title="$TITLE" -metadata artist="$ARTIST" -metadata album="$ALBUM" \
  "$OUT/${SLUG}-release.flac"
ffmpeg -y -loglevel error -i "$OUT/${SLUG}-master.wav" -ar 44100 \
  -c:a libmp3lame -b:a 320k \
  -metadata title="$TITLE" -metadata artist="$ARTIST" -metadata album="$ALBUM" \
  "$OUT/${SLUG}-release.mp3"

echo "→ verify"
ffmpeg -hide_banner -nostats -i "$OUT/${SLUG}-release.flac" -af "ebur128=peak=true" -f null - 2>&1 \
  | grep -E "^\s+(I|LRA|Peak):" | sed 's/^/   /'
node "$REPO/pop/bin/master-audit.mjs" "$OUT/${SLUG}-release.flac"
ls -la "$OUT/${SLUG}-release.flac" "$OUT/${SLUG}-release.mp3" | awk '{print "   " $5 "  " $9}'
