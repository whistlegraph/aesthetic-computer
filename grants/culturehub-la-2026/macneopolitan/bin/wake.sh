#!/bin/bash
# Good morning, Sophia — the wake-up run. Dims blueberry and frisbee, plays
# scores/trio-wake.mbscore on all three, and lifts their screens as the song
# begins (8 → BRIGHT over ~7 s from the downbeat). Volume is not touched here;
# set it first (osascript "set volume output volume N", ssh for the others).
# Usage: bin/wake.sh [bright=80] [lead=4.5]
set -u
cd "$(dirname "$0")/.."
BRIGHT=${1:-80}; LEAD=${2:-4.5}
REMOTE=(blueberry.local frisbee.local)
for h in "${REMOTE[@]}"; do ssh -o ConnectTimeout=5 "$h" '~/.local/bin/acbright 8' >/dev/null & done; wait
# Wake the lid cameras now (the eyes follow the room), so a first-time
# camera permission prompt lands before the song, not on its first line.
MB_NAME=computer.aestheticcomputer.menuband.gaze MB_KV="on=1" /tmp/mbpost 2>/dev/null || true
for h in "${REMOTE[@]}"; do ssh -o ConnectTimeout=5 "$h" 'MB_NAME=computer.aestheticcomputer.menuband.gaze MB_KV="on=1" /tmp/mbpost' 2>/dev/null & done; wait
ramp() {  # host — from the downbeat, six steps up
  sleep "$LEAD"
  for v in 18 30 45 58 70 "$BRIGHT"; do
    ssh -o ConnectTimeout=5 "$1" "~/.local/bin/acbright $v" >/dev/null; sleep 1
  done
}
for h in "${REMOTE[@]}"; do ramp "$h" & done
node bin/trio.mjs scores/trio-wake.mbscore neo blueberry.local frisbee.local --quiet
wait
