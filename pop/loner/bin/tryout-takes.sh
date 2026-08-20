#!/usr/bin/env bash
# tryout-takes.sh — hear another take of the line, solo, on ITS OWN chart.
#
# @jeffrey: "our envelopes etc are still fitting like the original samples ·
# we need to like restart the whole process for each actual take · or it
# will sound wonky".
#
# It did, and it was. The first version of this script warped a take onto
# the chart halo3 built from f- — f-'s hand-pinned onsets, f-'s syllable
# seams, f-'s consonant runways — so every take arrived wearing one
# singer's phrasing. This restarts the process instead:
#
#   1. takechart.py  assemble the line from that take's per-word corpus
#                    files, pin all 18 onsets exactly, find the syllable
#                    seams in ITS voice, keep only `durs` from the score
#   2. halo3.py      the FULL pipeline on that chart — consonant runway,
#                    boundary snap, energy trim, note re-measurement, the
#                    weighted warp clock, THE HOLD, nervox, the sibilant
#                    restore
#   3. stage         one static dB to the f- spine's loudness, so takes are
#                    compared on performance and not on mic distance
#   4. the study     kick + that vocal, and the scrolling piano roll drawn
#                    from that take's chart rather than from f-'s
#
# Any of the ten takes owning all eighteen lyric words: s pf cp f rq lg sh
# rd hk o — though hk is f- sliced a second time, not a second take.
#
#   bash pop/loner/bin/tryout-takes.sh rq sh lg
#     → out/takes/<take>-timeline.mp4 (+ .mp3), and on the Desktop
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
REPO="$(dirname "$(dirname "$LANE")")"
PY="$REPO/pop/.venv/bin/python"
OUT="$LANE/out/takes"
mkdir -p "$OUT"
cd "$REPO"

[ $# -gt 0 ] || { echo "usage: tryout-takes.sh <take> [take…]" >&2; exit 1; }

bash "$LANE/c/build.sh" >/dev/null
"$PY" "$HERE/takechart.py" "$@" 2>/dev/null | grep -E "✓|composition|could not|does not"

# ONE BAD TAKE MUST NOT END THE BENCH. `set -e` plus a bench loop meant a
# single failure — a take halo3 could not build, a wav timeline.py could
# not open — threw away every take queued behind it.
fail=0
for t in "$@"; do
  echo "── $t ────────────────────────────────────────────"
  ( set -e
  TAKES=1 PHRASES="w-$t" LEAD_ONLY=1 "$PY" "$HERE/halo3.py" 2>/dev/null \
    | grep -E "^  w-$t" || { echo "  ! halo3 built nothing for w-$t"; continue; }
  bash "$HERE/stage-takes.sh" "$LANE/vox4/w-$t.wav" "try-$t"
  TAKE="w-$t" TAKE_WAV="try-$t" MINIMAL=1 "$LANE/c/lonerremix" | tail -1
  TAKE="w-$t" TAKE_WAV="try-$t" python3 "$HERE/timeline.py" | tail -1
  ffmpeg -y -v error -i "$LANE/out/loner-kickvox-full.wav" \
    -codec:a libmp3lame -q:a 0 "$OUT/$t.mp3"
  mv "$LANE/out/loner-kickvox-timeline.mp4" "$OUT/$t-timeline.mp4"
  cp "$OUT/$t-timeline.mp4" ~/Desktop/ 2>/dev/null || true
  echo "  ✓ $OUT/$t-timeline.mp4" ) || { echo "  ! $t failed — moving on"; fail=$((fail+1)); }
done
[ "$fail" -eq 0 ] || echo "→ $fail take(s) failed; the rest are in $OUT"
