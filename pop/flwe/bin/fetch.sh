#!/bin/sh
# Fetch the Flower Eater source takes from the assets mirror and decode
# 48k mono wavs beside them. Re-runnable; source/ stays out of git.
set -e
cd "$(dirname "$0")/../source"
for id in 6992837952212569350 6975681078543551749 6977277752525344005 \
          6949031877718117638 6949737524520602885 6949568150962703621 \
          6950816151547022598 6948629412728360198; do
  [ -f "flwe-$id.mp4" ] || curl -sfL -o "flwe-$id.mp4" \
    "https://assets.aesthetic.computer/whistlegraph/index/posts/$id.mp4"
  ffmpeg -y -loglevel error -i "flwe-$id.mp4" -ac 1 -ar 48000 "flwe-$id.wav"
done
ls -la
