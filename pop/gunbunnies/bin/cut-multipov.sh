#!/bin/sh
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
lane=$(CDPATH= cd -- "$here/.." && pwd)
out="$lane/out"

ffmpeg -y -hide_banner -loglevel error \
  -i "$out/dandelion-rifle.mp4" \
  -i "$out/dandelion-rifle-pov.mp4" \
  -i "$out/dandelion-rifle-impact.mp4" \
  -i "$out/dandelion-rifle-reaction.mp4" \
  -filter_complex "\
[0:v]trim=start=0:end=2,setpts=PTS-STARTPTS[v0];\
[1:v]trim=start=0:end=0.7,setpts=PTS-STARTPTS[v1];\
[2:v]trim=start=0.4:end=2.7,setpts=PTS-STARTPTS[v2];\
[1:v]trim=start=1.8:end=3.2,setpts=PTS-STARTPTS[v3];\
[3:v]trim=start=0:end=1.6,setpts=PTS-STARTPTS[v4];\
[v0][v1][v2][v3][v4]concat=n=5:v=1:a=0[v];\
[0:a]atrim=start=0:end=8,asetpts=PTS-STARTPTS,volume=1.0[bed];\
[2:a]atrim=start=0.4:end=2.7,asetpts=PTS-STARTPTS,volume=0.35,adelay=2700|2700[impact];\
[3:a]atrim=start=0:end=1.6,asetpts=PTS-STARTPTS,volume=0.24,adelay=6400|6400[seeds];\
[bed][impact][seeds]amix=inputs=3:duration=first:normalize=0,alimiter=limit=0.95[a]" \
  -map '[v]' -map '[a]' -r 24 \
  -c:v libx264 -preset slow -crf 17 -pix_fmt yuv420p \
  -c:a aac -b:a 192k -movflags +faststart \
  "$out/dandelion-rifle-multipov.mp4"
