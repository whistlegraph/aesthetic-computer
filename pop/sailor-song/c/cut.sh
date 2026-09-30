#!/usr/bin/env bash
# cut.sh — master "sailor song (outside remix)". Modeled on wannadash's
# pop/cult/bin/cut-release.sh: a translation premaster (32 Hz high-pass,
# phone-readable mids, a touch of top — no 2–5 kHz push, her belts are bright
# already), then the density stage (1.5:1 RMS glue at half mix, tanh soft-clip
# on the transients, ONE measured static gain, 4× oversampled true-peak
# limiter at 0.760). House target (pop/MASTERING.md): −10 ±1 LUFS, ≤ −2 dBTP.
#
# Outputs: out/sailor-song-v5-master.wav (24/48), .flac, .mp3 (320k, cover
# embedded — the file we send her).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"
V="${VERSION:-v19}"
FULL="${FULL:-$OUT/sailor-song-$V-full.wav}"
PRE="$OUT/.$V-premaster.wav"
MASTER="$OUT/sailor-song-$V-master.wav"
COVER="$LANE/cover/sailor-song-cover.jpg"   # v6.2: clean, no title text
trap 'rm -f "$PRE"' EXIT

echo "→ translation premaster"
ffmpeg -y -v error -i "$FULL" -af \
  "highpass=f=32,bass=g=-2.5:f=90:w=0.7,equalizer=f=220:t=q:w=0.9:g=0.6,\
equalizer=f=900:t=q:w=0.85:g=1.6,treble=g=1.2:f=7500:w=0.6" -ar 48000 -c:a pcm_f32le "$PRE"

density() {   # $1 = static gain dB
  echo "acompressor=threshold=0.20:ratio=1.5:attack=30:release=180:knee=4:link=maximum:detection=rms:mix=0.50,\
volume=4.0dB,asoftclip=type=tanh:threshold=0.62:output=0.92,volume=${1}dB,\
aresample=192000,alimiter=limit=${LIMIT:-0.730}:attack=4:release=120:asc=true:asc_level=0.35:level=false,aresample=48000"
}
measure() { ffmpeg -hide_banner -nostats -i "$1" -af "$2${2:+,}ebur128=peak=true:framelog=quiet" -f null - 2>&1 \
  | awk '/Summary/{s=1} s&&/ I:/{print $2; exit}'; }

echo "→ density + loudness taken at the ceiling (measured, not guessed)"
GAIN2="${GAIN2:-6.0}"
for pass in 1 2 3; do
  I=$(measure "$PRE" "$(density "$GAIN2")")
  echo "  pass $pass: GAIN2=${GAIN2} dB → ${I} LUFS"
  awk -v i="$I" 'BEGIN{exit !(i > -10.3 && i < -9.7)}' && break
  GAIN2=$(awk -v g="$GAIN2" -v i="$I" 'BEGIN{printf "%.2f", g + (-10 - i)}')
done
ffmpeg -y -v error -i "$PRE" -af "$(density "$GAIN2")" -ar 48000 -c:a pcm_s24le "$MASTER"

echo "→ deliverables"
META=(-metadata title="sailor s" -metadata artist="s@ge" -metadata album_artist="s@ge" -metadata comment="cover of Gigi Perez — Sailor Song")
ffmpeg -y -v error -i "$MASTER" -c:a flac -compression_level 8 -sample_fmt s32 "${META[@]}" "$OUT/sailor-song-$V.flac"
if [ -f "$COVER" ]; then
  ffmpeg -y -v error -i "$MASTER" -i "$COVER" -map 0:a -map 1:v -c:v mjpeg -disposition:v attached_pic \
    -c:a libmp3lame -b:a 320k -id3v2_version 3 "${META[@]}" "$OUT/sailor-song-$V.mp3"
else
  ffmpeg -y -v error -i "$MASTER" -c:a libmp3lame -b:a 320k "${META[@]}" "$OUT/sailor-song-$V.mp3"
fi

echo "→ verify"
ffmpeg -hide_banner -nostats -i "$MASTER" -af ebur128=peak=true:framelog=quiet -f null - 2>&1 | grep -E "^\s+(I|LRA|Peak):"
echo "✓ $OUT/sailor-song-$V.mp3"
