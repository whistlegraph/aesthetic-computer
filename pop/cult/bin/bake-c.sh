#!/usr/bin/env bash
# bake-c.sh — the fast lane: render "wannadash" with the C engine and cut the
# DistroKid master from it. ~4 s to render + ~7 s to cut, against ~17 s for
# the Node render. The C engine is sample-identical to render10.mjs
# (pre-master peak and linear trim match to six places; residual 1.8e-7),
# so the FLAC that comes out is the same FLAC — see c/README.md.
#
# The Node renderer is still the receipt generator: the review video reads
# out/cult-remix-v10.events.json, which only `node bin/render10.mjs` writes.
# Bake here to LISTEN; run the Node render when you need the score video.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
echo "→ build"
bash "$LANE/c/build.sh" | tail -1
echo "→ render (C)"
"$LANE/c/cultremix" 2>&1 | grep -E "pre-master|^ok"
echo "→ tempo (120 → 128 with the swing, pitch preserved)"
eval "$(python3 "$HERE/tempo.py" "$LANE/c/out/cult-remix-c.wav" "$LANE/c/out/cult-remix-c-tempo.wav" --print-env)"
echo "→ cut"
FULL="$LANE/c/out/cult-remix-c-tempo.wav" bash "$HERE/cut-release.sh"
