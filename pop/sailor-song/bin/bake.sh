#!/usr/bin/env bash
# bake.sh — the whole record from the baked stems, wannadash's bake-c.sh shape:
# chart → build → render (C) → cut. Run the measuring side first
# (aesthetivox.py, measures.py, lock-vox.mjs, replay-guitar.mjs, bells.sh,
# then regularize.mjs — the engine plays src/vox/reg/ by default).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
ROOT="$(cd "$LANE/../.." && pwd)"
echo "→ bells"; bash "$HERE/bells.sh" | tail -1
echo "→ build"; bash "$LANE/c/build.sh" | tail -1
echo "→ render (C)"; (cd "$ROOT" && time "$LANE/c/sailorremix")
echo "→ cut"; bash "$LANE/c/cut.sh"
