#!/usr/bin/env bash
# bells.sh — bake the FEM bell bank the C engine rings (pop/bell/c/bell:
# a thin-shell finite-element model solved for its modes, rendered by
# modal synthesis). Bronze handbell, one strike per G#-minor pitch from
# E4 to E5 — the loner rule keeps bells at or under E5 — tuned into her
# guitar's +13¢ frame. ~10 s per pitch; cached, so only the first run pays.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
BELL="$LANE/../bell/c/bell"
DIR="$LANE/src/bells"
mkdir -p "$DIR"
[ -x "$BELL" ] || (cd "$LANE/../bell/c" && bash build.sh)
for m in 63 64 66 68 70 71 73 75 76 78 80 82 83 85 87 88; do   # v116: a second octave — "the bells after the last long are too low, they drag it around"
  out="$DIR/bell-$m.wav"
  [ -f "$out" ] && continue
  hz=$(awk -v m="$m" 'BEGIN{printf "%.3f", 440*2^((m+0.13-69)/12)}')
  "$BELL" --note "$hz" --material bronze --geometry handbell --dur 4 --vel 0.5 --out "$out" 2>/dev/null
  echo "  fem bell $m ($hz Hz)"
done
echo "✓ $DIR"
