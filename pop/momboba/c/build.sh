#!/usr/bin/env bash
# build.sh — compile momabobasheep.c (needs score.h — run `node c/bake.mjs`
# first). No -ffast-math so summation stays stable and matches the JS
# reference numerically (the hellsine rule).
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O2 -std=c11 -Wall -Wextra \
   -o "$HERE/momabobasheep" \
   "$HERE/momabobasheep.c" \
   -lm
echo "→ $HERE/momabobasheep"
