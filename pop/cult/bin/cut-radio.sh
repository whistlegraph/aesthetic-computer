#!/usr/bin/env bash
# cut-radio.sh — the radio mix master. @jeffrey: "'radio mix' where it's all
# compressed, everything feels more homogenous, vocals instruments all
# smoooooth like butter blending into one another, side chaining off one
# another, good panning stereo mix."
#
# The engine side is `cultremix --radio` (vox-keyed bed duck, pans 0.75,
# voice sat back, the healing sub throughline). This script is the glue:
#
#   1. BASS MONO below 110 Hz — the sub throughline stays centered and honest.
#   2. GENTLE WIDTH above 110 — sides +15%, no motion tricks; radio is dense.
#   3. GLUE — a program compressor working harder than the club cut
#      (ratio 2.6, 8 ms attack, 250 ms release) so every element leans on
#      every other one; then a tanh softclip for harmonic density.
#   4. SMOOTH TOP — lowpass 15 kHz (the FM ceiling), highpass 26 Hz,
#      a presence dip at 3.2 kHz so nothing pokes.
#
# Then the lane's law: MEASURE → one static dB → true-peak limiter.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"

FULL="$LANE/c/out/cult-remix-radio.wav"
TRIM="$OUT/.radio-trim.wav"
TARGET="${TARGET:--12.8}"

echo "→ trim to bar 8 (15.95 s in, 20 ms fade)"
ffmpeg -y -v error -ss 15.95 -i "$FULL" \
  -af "afade=t=in:st=0:d=0.02" -c:a pcm_s24le "$TRIM"

SPACE="$OUT/.radio-space.wav"
echo "→ cathedral + low shelf (Peep pass)"
ffmpeg -y -v error -i "$TRIM" -i "$LANE/samples/cathedral-ir.wav" -filter_complex \
  "[0:a]bass=g=2.2:f=95:w=0.6,asplit[dry][s];[s][1:a]afir=dry=0:wet=3[wet];\
[dry][wet]amix=inputs=2:weights='1 0.26':normalize=0[out]" \
  -map "[out]" -ar 48000 -c:a pcm_s24le "$SPACE"
TRIM="$SPACE"

RADIO="\
acrossover=split=110:order=4th[low][high];\
[low]pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1[lowm];\
[high]stereotools=slev=1.15[hip];\
[lowm][hip]amix=inputs=2:normalize=0,\
acompressor=threshold=0.24:ratio=2.6:attack=8:release=250:makeup=2.0:knee=6,\
volume=1.0dB,asoftclip=type=tanh,volume=-0.6dB,\
equalizer=f=3200:t=q:w=1.4:g=-1.2,\
equalizer=f=110:t=q:w=1.0:g=0.8,\
highpass=f=26,lowpass=f=15000"

WAX="$OUT/.radio-pre.wav"
ffmpeg -y -v error -i "$TRIM" -filter_complex "[0:a]$RADIO[out]" \
  -map "[out]" -ar 48000 -c:a pcm_s24le "$WAX"

echo "→ measure radio pre"
STATS=$(ffmpeg -hide_banner -nostats -i "$WAX" \
  -af loudnorm=I="$TARGET":TP=-1.0:LRA=8:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
GAIN=$(awk -v i="$MI" -v t="$TARGET" 'BEGIN{printf "%.2f", t-i}')
echo "  measured I=$MI  →  static ${GAIN} dB, then limit"

ffmpeg -y -v error -i "$WAX" -af "\
volume=${GAIN}dB,\
alimiter=limit=0.85:attack=4:release=120:level=disabled" \
  -ar 48000 -c:a pcm_s24le "$OUT/.radio-master.wav"

ffmpeg -y -v error -i "$OUT/.radio-master.wav" -c:a libmp3lame -b:a 320k \
  -metadata title="whistlegraph cult --- remix (v10.2 radio)" \
  -metadata artist="Whistlegraph" -metadata album="pop / cult" \
  -metadata comment="the radio mix: vox-keyed bed duck, voice sat back, pans at 0.75, a healing sub throughline gliding on the bass roots, then glue compression and the FM ceiling. Butter." \
  "$OUT/cult-remix-radio.mp3"
rm -f "$TRIM" "$WAX" "$OUT/.radio-master.wav"

ffmpeg -hide_banner -nostats -i "$OUT/cult-remix-radio.mp3" -af ebur128=peak=true -f null - 2>&1 \
  | grep -E "^\s+(I|LRA|Peak):"
echo "✓ $OUT/cult-remix-radio.mp3"
