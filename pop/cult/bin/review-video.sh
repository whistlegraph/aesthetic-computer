#!/usr/bin/env bash
# review-video.sh — the whole track as a picture you can scrub: four
# band-split source lanes (sub+kick / bass+body / voice / air) drawn as
# full-track waveforms with a playhead riding the audio, in the spirit of
# the loner lane's review-score mp4s. ffmpeg-only; no demucs, no python.
# (This ffmpeg has no drawtext, so the lanes are keyed by color alone:
#  red sub+kick · green bass+body · yellow voice · blue air, top to bottom.)
#
# Usage: bash pop/cult/bin/review-video.sh [in.mp3] [out.mp4]
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
SRC="${1:-$LANE/out/cult-remix-radio.mp3}"
OUT="${2:-${1:-$LANE/out/cult-remix-radio.mp3}}"
OUT="${OUT%.mp3}-review.mp4"
BG="$LANE/out/.review-bg.png"

DUR=$(ffprobe -v quiet -show_format "$SRC" | grep duration | cut -d= -f2)

W=1920; LH=252  # lane height; 4 lanes + title band = 1080

ffmpeg -y -v error -i "$SRC" -filter_complex "\
[0:a]asplit=4[a][b][c][d];\
[a]lowpass=f=150,showwavespic=s=${W}x${LH}:colors=#e0563a[wa];\
[b]highpass=f=150,lowpass=f=1200,showwavespic=s=${W}x${LH}:colors=#3aa66a[wb];\
[c]highpass=f=1200,lowpass=f=3400,showwavespic=s=${W}x${LH}:colors=#e0b23a[wc];\
[d]highpass=f=3400,showwavespic=s=${W}x${LH}:colors=#4a9ae0[wd];\
[wa][wb][wc][wd]vstack=inputs=4,pad=w=${W}:h=1080:x=0:y=72:color=#141210[bg]" \
  -map "[bg]" -frames:v 1 "$BG"

ffmpeg -y -v error -loop 1 -i "$BG" -i "$SRC" -filter_complex "\
[0:v]drawbox=x='t/${DUR}*${W}':y=72:w=3:h=1008:color=#f2ede4@0.9:t=fill,format=yuv420p[v]" \
  -map "[v]" -map 1:a -c:v libx264 -preset veryfast -crf 20 -r 30 \
  -c:a aac -b:a 256k -shortest "$OUT"

rm -f "$BG"
echo "✓ $OUT"
