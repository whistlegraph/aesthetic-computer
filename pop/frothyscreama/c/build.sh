#!/usr/bin/env bash
# build.sh — compile frothyscreama.c. No -ffast-math and no fused
# multiply-add: the engine is held sample-for-sample to the JS renderer
# (compare.mjs), and either one lets the compiler round differently.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra -ffp-contract=off \
   -o "$HERE/frothyscreama" \
   "$HERE/frothyscreama.c" \
   -lm
echo "→ $HERE/frothyscreama"
