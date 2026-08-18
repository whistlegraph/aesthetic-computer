#!/usr/bin/env bash
# study.sh — the tuning loop, end to end, as fast as it goes.
#
# @jeffrey: "please work on the code / make it super performant to render
# these out · optimize the rendering time / edit times of these atomic
# edits". Change a beat in halo3's CHART, run this, watch it. Everything
# it can skip, it skips: only the phrase the study plays is re-rendered,
# only the lead take of it (the octave halos and the low-3rd/5th backups
# are not in the kick+vocals study), and the WORLD analysis comes off
# disk. Pass a phrase name to work on a different one; FULL=1 rebuilds
# the whole bank, every render, for a real cut.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
REPO="$(dirname "$(dirname "$LANE")")"
PY="$REPO/pop/.venv/bin/python"
cd "$REPO"

if [ "${FULL:-}" = "1" ]; then
  "$PY" "$HERE/halo3.py"
else
  PHRASES="${1:-w-whole-line}" LEAD_ONLY=1 "$PY" "$HERE/halo3.py"
fi

bash "$LANE/c/build.sh" >/dev/null
MINIMAL=1 "$LANE/c/lonerremix" | tail -1
python3 "$HERE/timeline.py" | tail -1
ffmpeg -y -v error -i "$LANE/out/loner-kickvox-full.wav" \
  -codec:a libmp3lame -q:a 0 "$LANE/out/loner-kickvox.mp3"
cp "$LANE/out/loner-kickvox-timeline.mp4" "$LANE/out/loner-kickvox.mp3" ~/Desktop/ 2>/dev/null || true
echo "✓ study rebuilt → ~/Desktop/loner-kickvox-timeline.mp4"
