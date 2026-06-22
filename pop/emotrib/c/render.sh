#!/usr/bin/env bash
# render.sh — full emotrib render, all native (C engine → ffmpeg trap master),
# no JS. Builds + runs the engine, then a punchy, bass-forward club master with
# loud-but-controlled lows → mp3 + wav in ~/Documents/Shelf/emotrib.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
ROOT="$(cd -- "$HERE/.." && pwd)"          # pop/emotrib
OUT="${1:-$HOME/Documents/Shelf/emotrib}"
mkdir -p "$OUT" "$HERE/out"
BPM="${BPM:-140}"

RAW="$HERE/out/emotrib-raw.wav"
MASTER="$OUT/emotrib-MASTER.wav"
MP3="$OUT/emotrib.mp3"

# 1 · build + render the C engine
bash "$HERE/build.sh"
( cd "$HERE" && ./emotrib --out "$RAW" --bpm "$BPM" )

# 2 · TRAP MASTER — protect the sub, keep it tight + loud. Highpass the rumble,
# firm the 808 fundamental, scoop a little low-mid mud, lift presence on the
# snare/hats + air on the bells, glue compress, soft-clip for density,
# loudnorm + true-peak ceiling.
MASTER_AF="highpass=f=24,\
equalizer=f=50:t=q:w=0.8:g=2.2,\
equalizer=f=120:t=q:w=1.0:g=-1.0,\
equalizer=f=350:t=q:w=1.1:g=-2.0,\
equalizer=f=3500:t=q:w=1.1:g=1.0,\
equalizer=f=8000:t=q:w=1.0:g=1.2,\
highshelf=f=12000:g=2.4,\
lowpass=f=19000,\
acompressor=threshold=-18dB:ratio=2.0:attack=20:release=240:makeup=1.4:knee=8,\
asoftclip=type=tanh:threshold=0.98,\
loudnorm=I=-11:TP=-1.2:LRA=11,\
alimiter=limit=0.96:attack=4:release=80"

echo "# master → $MASTER"
ffmpeg -hide_banner -loglevel error -y -i "$RAW" -af "$MASTER_AF" \
  -ar 44100 -sample_fmt s16 "$MASTER"

ffmpeg -hide_banner -loglevel error -y -i "$MASTER" -c:a libmp3lame -b:a 320k "$MP3"
echo "✓ $MP3"
echo "✓ $MASTER"
