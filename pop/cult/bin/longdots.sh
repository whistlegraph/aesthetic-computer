#!/usr/bin/env bash
# longdots.sh — aesthetivox the opening dots.
#
# v10.1: the opening's slow dots were granular-stretched raw takes, and
# @jeffrey heard the grains: "more smoooooth … lessss choppy in their
# slowness". The smooth way to make a spoken syllable long is the way this
# lane already makes everything long — the WORLD chain (bin/sing.py), whose
# duration control sustains the vowel and leaves the consonant alone. So:
# each of the nine posts' chosen dot take, held ~1.9 s at its own measured
# pitch snapped to the nearest B-natural-minor tone (median f0 from
# alt/harvest.json), gentle vibrato, into sung/altdot-<id>-long.wav.
# The render prefers these and only falls back to the granular stretch if
# they have not been built.
#
# Needs: alt/samples (see README's derived-bank recipe) and pop/.venv.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
PY="$LANE/../.venv/bin/python3"
[ -x "$PY" ] || PY=python3

# take → note (measured median f0, snapped into B natural minor)
render() {
  local take="$1" note="$2" id
  id=$(echo "$take" | sed 's/alt-\([0-9]*\)-.*/\1/')
  local src="$LANE/alt/samples/$take.wav" out="$LANE/sung/altdot-$id-long.wav"
  if [ ! -f "$src" ]; then echo "! $take missing (harvest alt/ first)" >&2; return; fi
  if [ -f "$out" ] && [ ! "$src" -nt "$out" ]; then return; fi
  "$PY" "$HERE/sing.py" "$src" "$out" --notes "$note:1.9" \
    --vibrato-cents 22 --vibrato-onset-ms 500 --formant-db 1.6 --release-ms 220
  echo "✓ altdot-$id-long ← $take @ $note"
}

# …and the aesthetivox side of the drift: the three performers' own dot
# takes held the same way, two chord-safe tones each, so dotDriftVox can
# answer in tune without a stretcher (the 0.2 s staccato sung/dot-* bank
# chops when slowed — these don't).
rendervox() {
  local src="$1" name="$2" note="$3"
  local in="$LANE/samples/$src.wav" out="$LANE/sung/voxdot-$name.wav"
  if [ ! -f "$in" ]; then echo "! $src missing (run bin/slice.mjs first)" >&2; return; fi
  if [ -f "$out" ] && [ ! "$in" -nt "$out" ]; then return; fi
  "$PY" "$HERE/sing.py" "$in" "$out" --notes "$note:1.7" \
    --vibrato-cents 20 --vibrato-onset-ms 450 --formant-db 1.6 --release-ms 200
  echo "✓ voxdot-$name ← $src @ $note"
}

rendervox dot-jeffrey j-b2  B2
rendervox dot-jeffrey j-fs3 F#3
rendervox dot-alex    a-a3  A3
rendervox dot-alex    a-d4  D4
rendervox dot-camille c-b3  B3
rendervox dot-camille c-fs4 F#4

# The "i wanna" pickups: @jeffrey — "could be slowed a bit more, like
# faded / a bit slowed, its sorta rushed". Same takes, re-sung ~1.5x
# longer with a softer attack; the render fades them a shade lower too.
renderiwanna() {
  local src="$1" name="$2" notes="$3"
  local in="$LANE/samples/$src.wav" out="$LANE/sung/$name.wav"
  if [ ! -f "$in" ]; then echo "! $src missing (run bin/slice.mjs first)" >&2; return; fi
  if [ -f "$out" ] && [ ! "$in" -nt "$out" ]; then return; fi
  "$PY" "$HERE/sing.py" "$in" "$out" --notes "$notes" \
    --vibrato-cents 24 --attack-ms 40 --release-ms 160
  echo "✓ $name ← $src @ $notes"
}
renderiwanna i-wanna-a iwannaslow-a "D4:0.30,E4:0.48"
renderiwanna i-wanna-b iwannaslow-b "B3:0.30,C#4:0.48"
renderiwanna iwanna-c  iwannaslow-c "B3:0.33,C#4:0.45"

# The harmonic "cult" chords: the 4 s sung/cult-* takes end just as the
# drums come back, and @jeffrey heard the cut-off ("much lpnger decay …
# so they dont cut off when the perc and kicks come back"). So every choir
# pitch also gets a five-second hold with a 900 ms release — the choir
# rings past the act boundary and decays under the returning kick instead
# of vanishing at it.
rendercult() {
  local note="$1" tag="$2"
  local in="$LANE/samples/cult.wav" out="$LANE/sung/cultlong-$tag.wav"
  [ -f "$in" ] || { echo "! samples/cult.wav missing (run bin/slice.mjs)" >&2; return; }
  if [ -f "$out" ] && [ ! "$in" -nt "$out" ]; then return; fi
  "$PY" "$HERE/sing.py" "$in" "$out" --notes "$note:5.0" \
    --vibrato-cents 18 --vibrato-onset-ms 900 --formant-db 2.0 --release-ms 900
  echo "✓ cultlong-$tag @ $note"
}
rendercult B2 b2;  rendercult D3 d3;  rendercult F#3 fs3; rendercult G3 g3
rendercult A3 a3;  rendercult B3 b3;  rendercult C#4 cs4; rendercult D4 d4
rendercult E4 e4;  rendercult F#4 fs4; rendercult G4 g4

render alt-70551-dot2 B3
render alt-70555-dot1 B2
render alt-71018-dot3 G3
render alt-71195-dot1 B2
render alt-71244-dot2 C#4
render alt-71437-dot1 B3
render alt-71441-dot1 D4
render alt-71448-dot  F#4
render alt-71560-dot2 C#4
