#!/bin/sh
# Screenshots of fiapup in the booted iOS Simulator, one per staged moment,
# upright and on its side, into xbox/fiapup/shots/ios/. Build and install
# first (see FIAPUP.md); this only launches and shoots.
#
#   sh apple/fiapup/sim-shots.sh [moment:seconds …]

set -e
out="$(cd "$(dirname "$0")/../.." && pwd)/xbox/fiapup/shots/ios"
mkdir -p "$out"
moments="${*:-pet:1.1 rollover:3.2 fetch:1.05 beg:2.6 nap:9 zoomies:2.4 tug:3.2}"
for orientation in ${ORIENTATIONS:-portrait landscape}; do
  for moment in $moments; do
    name="${moment%%:*}" seconds="${moment##*:}"
    stage="$name"; [ "$name" = rollover ] && stage=pet
    xcrun simctl launch --terminate-running-process booted computer.aesthetic.fiapup \
      -stage "$stage" -seconds "$seconds" -pause 1 -orientation "$orientation" > /dev/null
    sleep 7
    xcrun simctl io booted screenshot --type=jpeg "$out/$name-$orientation.jpg" > /dev/null 2>&1
    echo "$name $orientation → $out/$name-$orientation.jpg"
  done
done
