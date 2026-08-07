#!/bin/sh
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
lane=$(CDPATH= cd -- "$here/.." && pwd)
out="$lane/out"

ffmpeg -y -hide_banner -loglevel error \
  -i "$out/physical-push-coast.mp4" \
  -i "$out/physical-fire-recoil.mp4" \
  -i "$out/physical-impact-bloom.mp4" \
  -filter_complex "\
[0:v]trim=start=0:end=3.1,setpts=PTS-STARTPTS[v0];\
[0:a]atrim=start=0:end=3.1,asetpts=PTS-STARTPTS[a0];\
[1:v]trim=start=0:end=2.3,setpts=PTS-STARTPTS[v1];\
[1:a]atrim=start=0:end=2.3,asetpts=PTS-STARTPTS[a1];\
[2:v]trim=start=0:end=4.0,setpts=PTS-STARTPTS[v2];\
[2:a]atrim=start=0:end=4.0,asetpts=PTS-STARTPTS[a2];\
[v0][a0][v1][a1][v2][a2]concat=n=3:v=1:a=1[v][a]" \
  -map '[v]' -map '[a]' -r 24 \
  -c:v libx264 -preset slow -crf 17 -pix_fmt yuv420p \
  -c:a aac -b:a 192k -movflags +faststart \
  "$out/physical-accuracy-proof.mp4"
