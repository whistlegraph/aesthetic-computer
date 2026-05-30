#!/usr/bin/env bash
# build.sh — compile sleephellsine.c into a fast, deterministic binary.
# We deliberately DO NOT pass -ffast-math: it relaxes rounding and can
# change the bit-exact summation of the long sine bell tails, which we
# want stable across machines.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra \
   -pthread \
   -o "$HERE/sleephellsine" \
   "$HERE/sleephellsine.c" \
   -lm
echo "→ $HERE/sleephellsine"
