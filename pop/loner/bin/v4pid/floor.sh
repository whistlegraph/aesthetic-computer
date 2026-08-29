#!/usr/bin/env bash
# floor.sh — cut the lonerclub extended floor mix (~5:57) from the baked stage.
#
# Requires the v4pid work cache (stems already rendered by run.sh). Sequences
# the record's finalized layers onto the extended timeline (assemble-floor.py)
# and masters through the same wax/FM chain, with the drop inhale remapped to
# the three places the spoken tag now lands.
#
# Usage:  bash pop/loner/bin/v4pid/floor.sh [dest.mp3]
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../../../.." && pwd)"
cd "$REPO"

export V4PID_WORK="${V4PID_WORK:-$HOME/.cache/ac/v4pid}"
S="$V4PID_WORK"
DEST="${1:-pop/loner/out/lonerclub-floor.mp3}"
PY="${PY:-python3}"

echo "→ assemble floor"
$PY "$HERE/assemble-floor.py"
ffmpeg -y -v error -f f32le -ar 48000 -ac 2 -i "$S/premaster-floor.raw" \
  -c:a pcm_s24le "$S/premaster-floor.wav"

echo "→ master (wax, floor inhales)"
# the tag lands at the end of clock (dst bar 72), the break build (110),
# and the hook reprise (148): side image folds around each, as on the record
INHALE="between(t,139.87,141.64)+between(t,214.63,216.39)+between(t,289.38,291.15)" \
TARGET="${TARGET:--13.5}" bash pop/loner/c/cut-wax.sh "$S/premaster-floor.wav" "$DEST" 2>&1 | tail -3

ffmpeg -y -v error -i "$DEST" -c copy \
  -metadata title="lonerclub (floor mix)" \
  -metadata artist="Whistlegraph Dot Org" -metadata album="pixsies" \
  -metadata comment="extended floor mix: 48-bar DJ intro, the record intact, a floorless break and a second drop, the hook reprised, 32-bar strip-down outro. Sequenced from the v4pid baked stage; wax/FM master." \
  "$S/t-floor.mp3"
mv "$S/t-floor.mp3" "$DEST"
echo "✓ $DEST"
