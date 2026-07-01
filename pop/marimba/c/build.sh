#!/usr/bin/env bash
# build.sh — compile fluttabap360.c. No -ffast-math so the numerics stay
# stable across machines and match the JS reference (render-fluttabap360.mjs),
# exactly like pop/nullabye/c/build.sh + pop/hellsine/c/build.sh.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra \
   -o "$HERE/fluttabap360" \
   "$HERE/fluttabap360.c" \
   -lm
echo "→ $HERE/fluttabap360"
