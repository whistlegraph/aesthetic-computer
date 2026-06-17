#!/usr/bin/env bash
# build.sh — compile nullnoise.c. No -ffast-math so the numerics stay
# stable across machines and match the JS reference (render-nuellaby.mjs).
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra \
   -o "$HERE/nullnoise" \
   "$HERE/nullnoise.c" \
   -lm
echo "→ $HERE/nullnoise"
