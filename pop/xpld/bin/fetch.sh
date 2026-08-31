#!/bin/sh
# Fetch the Explody Head source takes from the assets mirror and decode
# 48k mono wavs beside them. Re-runnable; source/ stays out of git.
set -e
cd "$(dirname "$0")/../source"
for id in 7275499036398865706 7258670360357276970 7257695616694881582 \
          7257275807209458986 7278943795482283307; do
  [ -f "xpld-$id.mp4" ] || curl -sfL -o "xpld-$id.mp4" \
    "https://assets.aesthetic.computer/whistlegraph/index/posts/$id.mp4"
  ffmpeg -y -loglevel error -i "xpld-$id.mp4" -ac 1 -ar 48000 "xpld-$id.wav"
done
ls -la
