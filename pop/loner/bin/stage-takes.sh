#!/usr/bin/env bash
# stage-takes.sh — level-match every lead take before it enters the engine.
#
# @jeffrey: "i think we need to master / treat each vocal separate right?"
# Right. The three takes were recorded in three rooms at three distances:
# f- lands at −22.1 LUFS raw, s- at −16.8, o- (the group) at −12.2. Warping
# them onto the chart does not fix that — it carries it. Dropping them into
# the arrangement at hand-picked gains means every gain has to be re-guessed
# whenever a take is re-rendered.
#
# So each take is MEASURED and moved by ONE static dB to a common integrated
# loudness, exactly the way the lane masters a mix. After this, a gain in
# lonerremix.c means the same thing whichever take it is applied to, and the
# swap is a change of ROOM rather than a change of level.
set -euo pipefail
LANE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# THE REFERENCE IS THE SPINE. f- is the take that was charted by hand, word
# by word; the others are warped onto ITS chart. So it sets the level too,
# rather than all three being moved to some outside number that would drift
# the moment halo3.py re-renders.
lufs() { ffmpeg -hide_banner -nostats -i "$1" -af ebur128 -f null - 2>&1 \
         | awk '/I:/{v=$2} END{print v}'; }
peak() { ffmpeg -hide_banner -nostats -i "$1" -af volumedetect -f null - 2>&1 \
         | awk -F': ' '/max_volume/{gsub(/ dB/,"",$2); print $2}'; }

TARGET="${TARGET:-$(lufs "$LANE/vox4/w-whole-line.wav")}"

stage() {                     # stage <src> <dest-name>
  local src="$1" dest="$LANE/vox4/$2.wav"
  local i g
  i=$(lufs "$src")
  # THE LANE'S MASTERING LAW, on one voice: MEASURE → one static dB → true-
  # peak limiter. Clamping the static gain instead — taking the smaller of
  # the loudness move and whatever kept the peak at −1 — sounds safe and is
  # not: a take with sharp word onsets (the corpus lines are cut per word,
  # so every word starts with one) loses the whole match. try-pf wanted
  # +6.0 dB, the clamp allowed +0.4, and it arrived nearly 6 dB under the
  # take it was being compared against. A limiter catching the top few dB
  # of a sparse vocal is far less of a change than that.
  g=$(awk -v i="$i" -v t="$TARGET" 'BEGIN{printf "%.2f", t-i}')
  # PCM_16, matching what halo3 writes into the bank. The C loader reads
  # anything, but bin/timeline.py draws its waveform through the stdlib
  # `wave` module and that refuses float ("unknown format: 3") — which
  # killed the whole audition batch after the first take.
  ffmpeg -y -v error -i "$src" \
    -af "volume=${g}dB,alimiter=limit=0.891:attack=4:release=60:level=disabled" \
    -c:a pcm_s16le "$dest"
  printf "  %-12s %8s LUFS  →  %+6s dB  →  %s LUFS, peak %s dB\n" \
         "$2" "$i" "$g" "$(lufs "$dest")" "$(peak "$dest")"
}

# stage-takes.sh <src> <dest-name>  stages exactly that one and stops —
# bin/tryout-takes.sh uses this to bench a take without touching the record.
if [ $# -eq 2 ]; then
  echo "→ level-matching $2 to the f- spine ($TARGET LUFS)"
  stage "$1" "$2"
  exit 0
fi

echo "→ level-matching leads to the f- spine ($TARGET LUFS)"
stage "$LANE/vox-dub/sung-s-whole-line.wav" alt-soft
# THE GROUP TAKE IS NOT STAGED. o-whole-line is Camille, @jeffrey and Alex
# together, and @jeffrey: "lets not do the group vocals anymore · they are
# weird". The chart is one pitch contour and singdub f0-REPLACES the take
# with it — fine for a solo, where harvest tracks one larynx cleanly, but
# three voices singing slightly apart give an f0 estimate that flickers
# between whoever is loudest, and the warp smears the ensemble into one
# synthetic voice. To use that take it would have to be PLACED as a
# performance rather than resynthesised:
#   stage "$LANE/vox-dub/sung-o-whole-line.wav" alt-group
