#!/usr/bin/env bash
# run.sh — lonerclubber360: the six-minute club cut of lonerclub (v4pid).
#
# HYBRID: her real-speed passes carry the hook at the act doors (opening
# verse bar 16, f- verse bar 40, the finale DROPPING at bar 88 right after
# its own 0.25x smear, reprise at 120/136); the stretched material is the
# connective tissue (half-speed tease/weave/tail, quarter-speed arps as the
# riser + kickless break). Grid math: stretches are exactly 0.5x / 0.25x so
# one v4pid bar = 2 / 4 floor bars on the same 122 grid; 6:00 = 183 bars.
# The floor is a palindrome: rhythm assembles act by act, reduces to a
# ticking shaker in the break, returns whole at the drop, peels away in
# reverse until the kick stands alone again.
#
#   STRETCH  rubberband R3, pitch held: vocalsFX x4 + x2, pads x4
#   FLOOR    gen-floor360 (kick+turns, act-aware perc, click-rush doors)
#   BASS     gen-bass360 (pedal between passes, wub in three passes)
#   STAGE    assemble-hybrid — passes, smears, pump, ITD/ILD placement
#   MASTER   cut-wax.sh with the inhale moved to this cut's drop (bar 88)
#
# Needs the v4pid work dir (~/.cache/ac/v4pid) already built by v4pid/run.sh.
# Usage:  bash pop/loner/bin/clubber360/run.sh [dest.mp3]
#   STRETCH=0  reuse stretched stems      TARGET  master LUFS (-11.5)
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../../../.." && pwd)"
cd "$REPO"

export V4PID_WORK="${V4PID_WORK:-$HOME/.cache/ac/v4pid}"
export CLUB360_WORK="${CLUB360_WORK:-$HOME/.cache/ac/clubber360}"
V="$V4PID_WORK"; S="$CLUB360_WORK"
mkdir -p "$S"
DEST="${1:-pop/loner/out/lonerclubber360.mp3}"
PY="${PY:-$REPO/pop/.venv/bin/python}"

[ -f "$V/vocalsFX.wav" ] || { echo "v4pid work dir is empty — run v4pid/run.sh first"; exit 1; }

stretch() { # stretch <in.wav> <out.wav>
  [ "${STRETCH:-1}" = "0" ] && [ -f "$2" ] && return 0
  [ -f "$2" ] && [ "${STRETCH:-1}" != "force" ] && return 0
  echo "  x4 $(basename "$2")"
  rubberband -3 --time 4.0 "$1" "$2" >/dev/null 2>&1
}

echo "→ stretch (rubberband R3, pitch held)"
[ -f "$S/st-pads.wav" ] || ffmpeg -y -v error -f f32le -ar 48000 -ac 2 \
  -i "$V/st-pads.raw" -c:a pcm_s24le "$S/st-pads.wav"
stretch "$S/st-pads.wav" "$S/str-pads.wav"
stretch "$V/vocalsFX.wav" "$S/str-vocalsFX.wav"
if [ ! -f "$S/str2-vocalsFX.wav" ]; then
  echo "  x2 str2-vocalsFX.wav"
  rubberband -3 --time 2.0 "$V/vocalsFX.wav" "$S/str2-vocalsFX.wav" >/dev/null 2>&1
fi

echo "→ floor"
$PY "$HERE/gen-floor360.py"
echo "→ bass"
$PY "$HERE/gen-bass360.py"
echo "→ assemble"
$PY "$HERE/assemble-hybrid.py"
ffmpeg -y -v error -f f32le -ar 48000 -ac 2 -i "$S/premaster360.raw" -c:a pcm_s24le "$S/premaster360.wav"

echo "→ master"
# the FM inhale folds the sides in through the last breath before bar 88
INHALE='between(t,171.49,173.24)' TARGET="${TARGET:--11.5}" \
  bash pop/loner/c/cut-wax.sh "$S/premaster360.wav" "$DEST" 2>&1 | tail -4
ffmpeg -y -v error -i "$DEST" -c copy \
  -metadata title="lonerclubber360" -metadata artist="Whistlegraph Dot Org" "$S/t.mp3"
mv "$S/t.mp3" "$DEST"
echo "✓ $DEST"
