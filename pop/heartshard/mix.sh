#!/usr/bin/env bash
# mix.sh — heartshard full mix: the C bed + the WIYH acapella.
#
# The vocal enters at track beat 32 (12.8 s at 150 BPM). The source grid
# phase, measured from the instrumental's drum onsets, is 0.165 s; stretched
# that's 0.1496 s, so the file is delayed 12.6504 s = 607219 samples at 48 kHz.
set -euo pipefail
cd "$(dirname "$0")"
./c/build.sh >/dev/null
./c/build/heartshard
VOX="../samples/whats-inside-your-heart/acapella-150bpm.wav"
ffmpeg -y -v error -i out/heartshard.wav -i "$VOX" -filter_complex "\
[1:a]aresample=48000,highpass=f=85,adelay=607219S|607219S,volume=1.6[v];\
[0:a][v]amix=inputs=2:duration=first:normalize=0,alimiter=limit=0.98[m]" \
  -map "[m]" out/heartshard-full.wav
echo "out/heartshard-full.wav"
