#!/usr/bin/env bash
# cut-competitive.sh — non-destructive wannadash competitive-master candidate.
#
# Rebuilds the approved 2:21 edit and spatial premaster, then adds a gentle
# parallel RMS compression pass and an oversampled true-peak limiter. It does
# not replace the release master or stage a DistroKid upload.
#
# Outputs:
#   out/wannadash-competitive-master.wav  24-bit / 48 kHz review master
#   out/wannadash-competitive.flac        24-bit / 48 kHz candidate delivery
#   out/wannadash-competitive.mp3         320 kbps listening copy
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"
FULL="$OUT/cult-remix-v10-full.wav"
TRIM="$OUT/.competitive-trim.wav"
SPACE="$OUT/.competitive-space.wav"
MASTER="$OUT/wannadash-competitive-master.wav"

cleanup() {
  rm -f "$TRIM" "$SPACE"
}
trap cleanup EXIT

echo "→ rebuild discovery edit with two-bar buildup"
ffmpeg -y -v error -i "$FULL" -filter_complex \
  "[0:a]atrim=start=15.95:end=20,asetpts=PTS-STARTPTS,afade=t=out:st=4.04:d=0.01[a];\
[0:a]atrim=start=58:end=120,asetpts=PTS-STARTPTS,afade=t=in:d=0.01[b];\
[0:a]atrim=start=127.76:end=136,asetpts=PTS-STARTPTS[c];\
[0:a]atrim=start=143.76:end=167.95,asetpts=PTS-STARTPTS[d];\
[0:a]atrim=start=183.71,asetpts=PTS-STARTPTS[e];\
[b][c]acrossfade=d=0.24:c1=tri:c2=tri[bc];\
[bc][d]acrossfade=d=0.24:c1=tri:c2=tri[bcd];\
[bcd][e]acrossfade=d=0.24:c1=tri:c2=tri[bcde];\
[a][bcde]concat=n=2:v=0:a=1,afade=t=in:st=0:d=0.02[out]" \
  -map "[out]" -c:a pcm_s24le "$TRIM"

echo "→ rebuild cathedral + bright translation premaster"
ffmpeg -y -v error -i "$TRIM" -i "$LANE/samples/cathedral-ir.wav" -filter_complex \
  "[0:a]highpass=f=28,\
bass=g=-1.2:f=95:w=0.6,\
equalizer=f=220:t=q:w=0.9:g=0.8,\
equalizer=f=800:t=q:w=0.85:g=2.2,\
equalizer=f=2800:t=q:w=0.9:g=1.2,\
treble=g=1.6:f=6500:w=0.6,asplit[dry][s];[s][1:a]afir=dry=0:wet=3[wet];\
[dry][wet]amix=inputs=2:weights='1 0.35':normalize=0[out]" \
  -map "[out]" -ar 48000 -c:a pcm_s24le "$SPACE"

echo "→ competitive density + oversampled true-peak ceiling"
ffmpeg -y -v error -i "$SPACE" -af \
  "acompressor=threshold=0.25:ratio=1.4:attack=24:release=140:knee=3:link=maximum:detection=rms:mix=0.78,\
volume=8.0dB,\
aresample=192000,\
alimiter=limit=0.700:attack=4:release=100:asc=true:asc_level=0.5:level=false,\
aresample=48000" \
  -ar 48000 -c:a pcm_s24le "$MASTER"

ffmpeg -y -v error -i "$MASTER" -c:a flac -compression_level 8 \
  -metadata title="wannadash" -metadata artist="Whistlegraph Dot Org" \
  -metadata album="pixsies" "$OUT/wannadash-competitive.flac"
ffmpeg -y -v error -i "$MASTER" -c:a libmp3lame -b:a 320k \
  -metadata title="wannadash" -metadata artist="Whistlegraph Dot Org" \
  -metadata album="pixsies" "$OUT/wannadash-competitive.mp3"

echo "→ verify"
ffmpeg -hide_banner -nostats -i "$MASTER" \
  -af ebur128=peak=true:framelog=quiet -f null - 2>&1 | \
  grep -E "^\s+(I|LRA|Peak):"
echo "✓ $MASTER"
