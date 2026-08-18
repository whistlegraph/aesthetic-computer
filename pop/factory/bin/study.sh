#!/usr/bin/env bash
# study.sh — the tuning loop, end to end, as fast as it goes.
#
# Change a beat in chart.py's CHART, run this, watch it. Everything it can
# skip, it skips: only the lead take is re-rendered (the octave halos and
# the low-3rd/5th backups are not in the press+words study), and the WORLD
# analysis comes off disk. FULL=1 rebuilds the whole bank, every render,
# for a real cut.
#
# The audit runs as part of the build and prints its flags, so a bad word
# boundary announces itself here rather than waiting to be heard.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
REPO="$(dirname "$(dirname "$LANE")")"
PY="$REPO/pop/.venv/bin/python"
cd "$REPO"

if [ "${FULL:-}" = "1" ]; then
  "$PY" "$HERE/chart.py"
else
  LEAD_ONLY=1 "$PY" "$HERE/chart.py"
fi

bash "$LANE/c/build.sh" >/dev/null
MINIMAL=1 "$LANE/c/factoryremix" | tail -1
python3 "$HERE/timeline.py" | tail -1
ffmpeg -y -v error -i "$LANE/out/factory-kickvox-full.wav" \
  -codec:a libmp3lame -q:a 0 "$LANE/out/factory-kickvox.mp3"
cp "$LANE/out/factory-kickvox-timeline.mp4" "$LANE/out/factory-kickvox.mp3" \
   ~/Desktop/ 2>/dev/null || true
echo "✓ study rebuilt → ~/Desktop/factory-kickvox-timeline.mp4"
