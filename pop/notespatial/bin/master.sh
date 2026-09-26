#!/usr/bin/env bash
# pop/notespatial/bin/master.sh — the house law on the binaural print.
#
#   bash pop/notespatial/bin/master.sh <print.wav> <gain_db> <out.flac> [ceiling_dbtp]
#
# float print → 20 Hz subsonic high-pass → bass mono below 120 Hz (mid/side:
# the side channel is high-passed at 120 Hz) → one static dB → 4× oversampled
# true-peak limiter (6 ms attack, 90 ms release, ASC) → back to 44.1 k →
# 24-bit FLAC. Nothing rides the song; the composed level arc stays.
set -euo pipefail
IN="$1"; GAIN="$2"; OUT="$3"; CEIL="${4:--2.2}"
LIM=$(python3 -c "print(round(10**($CEIL/20),4))")
ffmpeg -y -v error -i "$IN" -filter_complex "\
[0:a]highpass=f=20:p=2,asplit[a][b];\
[a]pan=mono|c0=0.5*c0+0.5*c1[m];\
[b]pan=mono|c0=0.5*c0-0.5*c1,highpass=f=120:p=2[s];\
[m][s]join=inputs=2:channel_layout=stereo,pan=stereo|c0=c0+c1|c1=c0-c1,\
volume=${GAIN}dB,aresample=176400,\
alimiter=limit=${LIM}:attack=6:release=90:asc=1:level=0,\
aresample=44100[out]" \
-map "[out]" -c:a flac -sample_fmt s32 -bits_per_raw_sample 24 "$OUT"
echo "$OUT"
