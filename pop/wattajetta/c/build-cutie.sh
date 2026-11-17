#!/usr/bin/env bash
# build-cutie.sh — compile wattajetta-cutie.c. No -ffast-math and no fused
# multiply-add: the engine is held sample-for-sample to the JS renderer
# (compare-cutie.mjs), and either one lets the compiler round differently.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra -ffp-contract=off \
   -o "$HERE/wattajetta-cutie" \
   "$HERE/wattajetta-cutie.c" \
   -lm
echo "→ $HERE/wattajetta-cutie"
