#!/usr/bin/env bash
# cut-wax.sh — the wax/FM master. @jeffrey (v4pid): "a better stereo /
# material master … with panning and other cool mastering effects like wax
# … or even like fm radio style".
#
# The material chain, in signal order, before the lane's mastering law:
#
#   1. BASS MONO below 120 Hz — how a lacquer is actually cut; keeps the
#      kick centered and the club sub honest while the top opens up.
#   2. WIDTH + MOTION above 120 Hz — sides lifted (M/S, +~30%), and a very
#      slow L/R drift (apulsator at 0.06 Hz, shallow) so the image breathes
#      like a record on a slightly off-center spindle rather than sitting
#      frozen in the middle.
#   3. WOW — vibrato at 0.4 Hz, ±0.12% — vinyl-real, felt not heard.
#   4. MATERIAL — a tanh soft-clip driven ~2.4 dB (tape/lacquer harmonic
#      density) and an exciter around 7.5 kHz for the FM "air" that the
#      15 kHz ceiling would otherwise dull.
#   5. FM DENSITY — one program compressor pumping harder than cut-club's
#      glue (ratio 2.4, 8 ms attack) — the Optimod always-loud feel.
#   6. FM CEILING — lowpass at 15 kHz (what stereo FM actually transmits),
#      highpass at 28 Hz.
#   7. DROP INHALE — from 61.55–63.30, side level folds to 18% around the
#      centered spoken tag; the full club width returns on the downbeat.
#
# Then the law, unchanged from the lane: MEASURE → one static dB →
# true-peak limiter at 0.82. Never a second loudnorm.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"

FULL="${1:?usage: cut-wax.sh premaster.wav dest.mp3}"
DEST="${2:?usage: cut-wax.sh premaster.wav dest.mp3}"
TARGET="${TARGET:--9.0}"

MATERIAL="\
acrossover=split=120:order=4th[low][high];\
[low]pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1[lowm];\
[high]stereotools=slev=1.3,apulsator=hz=0.06:amount=0.14:mode=sine,\
aexciter=amount=0.8:drive=6:blend=0:freq=7500[hip];\
[lowm][hip]amix=inputs=2:normalize=0,\
vibrato=f=0.4:d=0.0012,\
volume=1.1dB,asoftclip=type=tanh,volume=-0.8dB,\
acompressor=threshold=0.28:ratio=1.8:attack=10:release=200:makeup=1.2:knee=8,\
equalizer=f=90:t=q:w=0.9:g=1.0,\
equalizer=f=2800:t=q:w=1.6:g=0.8,\
highpass=f=28,lowpass=f=15000,\
stereotools=slev=0.18:enable='between(t,61.55,63.30)'"

WAX="$OUT/.wax-pre.wav"
ffmpeg -y -v error -i "$FULL" -filter_complex "[0:a]$MATERIAL[out]" \
  -map "[out]" -ar 48000 -c:a pcm_s24le "$WAX"

echo "→ measure $(basename "$WAX")"
STATS=$(ffmpeg -hide_banner -nostats -i "$WAX" \
  -af loudnorm=I="$TARGET":TP=-1.0:LRA=9:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
GAIN=$(awk -v i="$MI" -v t="$TARGET" 'BEGIN{printf "%.2f", t-i}')
echo "  measured I=$MI  →  static ${GAIN} dB, then limit"

ffmpeg -y -v error -i "$WAX" -af "\
volume=${GAIN}dB,\
alimiter=limit=0.82:attack=5:release=100:level=disabled" \
  -ar 48000 -c:a pcm_s24le "$OUT/.wax-master.wav"

ffmpeg -y -v error -i "$OUT/.wax-master.wav" -c:a libmp3lame -b:a 320k \
  "$DEST"
cp "$OUT/.wax-master.wav" "${DEST%.mp3}.wav"   # the master wav rides along
rm -f "$WAX" "$OUT/.wax-master.wav"

ffmpeg -hide_banner -nostats -i "$DEST" -af ebur128=peak=true -f null - 2>&1 \
  | grep -E "^\s+(I|LRA|Peak):"
echo "✓ $DEST"
