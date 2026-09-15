#!/usr/bin/env bash
# bake-amazing.sh — the whole "amazing grace" record from source, ~1 min.
#
#   build engine → C bed (cells) → sung lead (sing-amazing.mjs) →
#   crunch the ac stamp → mix → cut-amazing.sh (the house master law)
#
#   bash pop/big-pictures/bin/bake-amazing.sh
#   INTRO=6.0 VOX=1.20 BED=0.68 bash pop/big-pictures/bin/bake-amazing.sh
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
LANE="$(cd -- "$HERE/.." && pwd)"
REPO="$(cd -- "$LANE/../.." && pwd)"
OUT="$LANE/out/amazing-grace"
ARCH="$REPO/system/public/assets/pop/big-pictures"
mkdir -p "$OUT"

INTRO="${INTRO:-6.0}"        # where the pickup "a-" lands (engine INTRO_SEC)
VOX="${VOX:-1.20}"           # sung lead gain (both stems peak −3 dBFS; ≈3.6 dB over the bed)
BED="${BED:-0.68}"           # C bed gain
STAMP_AT="${STAMP_AT:-65.8}" # the ac signoff, 1.5 s into the amen ring
IR="$REPO/pop/cult/samples/cathedral-ir.wav"

echo "→ engine"
bash "$LANE/c/build.sh" 2>&1 | grep -v -E "warning|organ_render|\^~|^\s+[0-9]+ \||^$" || true
"$LANE/c/amazinhym" --intro "$INTRO" --out "$OUT/bed.wav" 2>&1 | grep -E "total|wrote"

echo "→ sung lead"
node "$HERE/sing-amazing.mjs" | tail -3

echo "→ stamp"
STAMP_SRC="$ARCH/ac-stamp/vocal/ac-stamp-vocal.mp3"
[ -f "$STAMP_SRC" ] || { echo "✗ missing $STAMP_SRC (aws s3 sync the archive down)"; exit 1; }
ffmpeg -y -loglevel error -i "$STAMP_SRC" \
  -af "asetrate=66150,aresample=44100,acrusher=bits=4:mode=lin:aa=1,aformat=sample_rates=22050,aresample=48000,volume=1.6" \
  -ac 2 "$OUT/stamp.wav"

echo "→ mix  (vox $VOX · bed $BED · intro $INTRO s · stamp @ $STAMP_AT s)"
TOTAL=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$OUT/bed.wav")
DELAY_MS=$(python3 -c "print(int(round($INTRO*1000)))")
STAMP_MS=$(python3 -c "print(int(round($STAMP_AT*1000)))")
FADE_ST=$(python3 -c "print(max(0.0, $TOTAL-2.5))")
if [ -f "$IR" ]; then
  # the sung lead gets the cult lane's cathedral, wet 0.20, so it sits in
  # the same room the bed's own reverb implies
  ffmpeg -y -loglevel error -i "$OUT/bed.wav" -i "$OUT/vox.wav" -i "$OUT/stamp.wav" -i "$IR" \
    -filter_complex "\
[1:a]aformat=channel_layouts=stereo,volume=${VOX},asplit[vd][vw];\
[vw][3:a]afir=dry=0:wet=1[vwet];\
[vd][vwet]amix=inputs=2:weights='1 0.20':normalize=0,adelay=${DELAY_MS}|${DELAY_MS}[vox];\
[0:a]volume=${BED}[bed];\
[2:a]adelay=${STAMP_MS}|${STAMP_MS},volume=0.9[stm];\
[bed][vox][stm]amix=inputs=3:duration=first:dropout_transition=0:normalize=0,\
afade=t=out:st=${FADE_ST}:d=2.5[out]" \
    -map "[out]" -ar 48000 -c:a pcm_f32le "$OUT/pre.wav"
else
  ffmpeg -y -loglevel error -i "$OUT/bed.wav" -i "$OUT/vox.wav" -i "$OUT/stamp.wav" \
    -filter_complex "\
[1:a]aformat=channel_layouts=stereo,volume=${VOX},adelay=${DELAY_MS}|${DELAY_MS}[vox];\
[0:a]volume=${BED}[bed];\
[2:a]adelay=${STAMP_MS}|${STAMP_MS},volume=0.9[stm];\
[bed][vox][stm]amix=inputs=3:duration=first:dropout_transition=0:normalize=0,\
afade=t=out:st=${FADE_ST}:d=2.5[out]" \
    -map "[out]" -ar 48000 -c:a pcm_f32le "$OUT/pre.wav"
fi

echo "→ master"
PRE="$OUT/pre.wav" bash "$HERE/cut-amazing.sh"
