#!/bin/sh
set -eu

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=$(CDPATH= cd -- "$HERE/../../.." && pwd)
OUT="$ROOT/pop/teknull/out"
BUILD="${TMPDIR:-/tmp}/aesthetic-computer-taksmukkeklokken"
BIN="$BUILD/taksmukkeklokken"
HARSH="${TAKSMUKKE_HARSH:-0}"
STEM="taksmukkeklokken-spatial-orchestral-c"
MASTER_FILTER="highpass=f=25,lowpass=f=18500,loudnorm=I=-14.7:TP=-1.7:LRA=11"
if [ "$HARSH" != "0" ]; then
  STEM="taksmukkeklokken-spatial-orchestral-harsh-c"
  MASTER_FILTER="highpass=f=25,equalizer=f=2800:t=q:w=.75:g=1.8,equalizer=f=7600:t=q:w=.9:g=1.1,lowpass=f=19000,loudnorm=I=-14.7:TP=-1.7:LRA=11"
fi
RAW="$OUT/.${STEM}.raw.wav"
WAV="$OUT/${STEM}.24bit-48k.wav"
MP3="$OUT/${STEM}.mp3"
RECEIPT="$OUT/${STEM}.events.json"

mkdir -p "$BUILD" "$OUT"
cc -O3 -std=c11 -Wall -Wextra -pedantic -o "$BIN" "$HERE/taksmukkeklokken.c" -lm

set -- --out "$RAW" --receipt "$RECEIPT" --piano-dir "$ROOT/fedac/native/samples/piano" --harsh "$HARSH"
if [ -n "${TAKSMUKKE_ARCADE_SAMPLES:-}" ]; then
  set -- "$@" --arcade-samples "$TAKSMUKKE_ARCADE_SAMPLES"
fi

cd "$ROOT"
"$BIN" "$@"

ffmpeg -y -hide_banner -loglevel error -i "$RAW" \
  -af "$MASTER_FILTER" \
  -ar 48000 -c:a pcm_s24le "$WAV"
ffmpeg -y -hide_banner -loglevel error -i "$WAV" \
  -c:a libmp3lame -b:a 320k "$MP3"
rm -f "$RAW"

PROBE=$(ffprobe -v error -show_entries format=duration,size \
  -show_entries stream=codec_name,sample_rate,channels,bits_per_raw_sample \
  -of json "$WAV")
LOUDNESS=$(ffmpeg -hide_banner -nostats -i "$WAV" \
  -filter_complex 'ebur128=peak=true' -f null - 2>&1)
I=$(printf '%s\n' "$LOUDNESS" | awk '/Summary:/{s=1;next} s && /I:/{print $2;exit}')
LRA=$(printf '%s\n' "$LOUDNESS" | awk '/Summary:/{s=1;next} s && /LRA:/{print $2;exit}')
TP=$(printf '%s\n' "$LOUDNESS" | awk '/Summary:/{s=1;next} s && /Peak:/{print $2;exit}')
WAV_SHA=$(shasum -a 256 "$WAV" | awk '{print $1}')
MP3_SHA=$(shasum -a 256 "$MP3" | awk '{print $1}')
TMP_RECEIPT="$RECEIPT.tmp"
jq --argjson probe "$PROBE" --argjson i "$I" --argjson lra "$LRA" \
  --argjson tp "$TP" --arg wavSha "$WAV_SHA" --arg mp3Sha "$MP3_SHA" \
  '.qc={wav:$probe,integratedLufs:$i,loudnessRangeLu:$lra,truePeakDbfs:$tp,wavSha256:$wavSha,mp3Sha256:$mp3Sha}' \
  "$RECEIPT" > "$TMP_RECEIPT"
mv "$TMP_RECEIPT" "$RECEIPT"

printf '%s\n' "$PROBE"
printf '%s\n%s\n%s\n' "$WAV" "$MP3" "$RECEIPT"
