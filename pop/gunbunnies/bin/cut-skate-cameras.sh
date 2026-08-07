#!/bin/sh
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
lane=$(CDPATH= cd -- "$here/.." && pwd)
out="$lane/out"

ffmpeg -y -hide_banner -loglevel error \
  -i "$out/skate-cameras-tracking.mp4" \
  -i "$out/skate-cameras-wheel.mp4" \
  -i "$out/skate-cameras-security-pov.mp4" \
  -i "$out/skate-cameras-impact.mp4" \
  -filter_complex "\
[1:v]trim=start=0:end=1.1,setpts=PTS-STARTPTS[v0];\
[1:a]atrim=start=0:end=1.1,asetpts=PTS-STARTPTS[a0];\
[0:v]trim=start=0:end=1.1,setpts=PTS-STARTPTS[v1];\
[0:a]atrim=start=0:end=1.1,asetpts=PTS-STARTPTS[a1];\
[2:v]trim=start=0:end=1.2,setpts=PTS-STARTPTS[v2];\
[2:a]atrim=start=0:end=1.2,asetpts=PTS-STARTPTS[a2];\
[3:v]trim=start=0.4:end=1.9,setpts=PTS-STARTPTS[v3];\
[3:a]atrim=start=0.4:end=1.9,asetpts=PTS-STARTPTS[a3];\
[0:v]trim=start=1.3:end=2.7,setpts=PTS-STARTPTS[v4];\
[0:a]atrim=start=1.3:end=2.7,asetpts=PTS-STARTPTS[a4];\
[1:v]trim=start=1.5:end=2.4,setpts=PTS-STARTPTS[v5];\
[1:a]atrim=start=1.5:end=2.4,asetpts=PTS-STARTPTS[a5];\
[2:v]trim=start=2:end=3.6,setpts=PTS-STARTPTS[v6];\
[2:a]atrim=start=2:end=3.6,asetpts=PTS-STARTPTS[a6];\
[v0][a0][v1][a1][v2][a2][v3][a3][v4][a4][v5][a5][v6][a6]concat=n=7:v=1:a=1[v][a]" \
  -map '[v]' -map '[a]' -r 24 \
  -c:v libx264 -preset slow -crf 17 -pix_fmt yuv420p \
  -c:a aac -b:a 192k -movflags +faststart \
  "$out/skate-cameras-action.mp4"
