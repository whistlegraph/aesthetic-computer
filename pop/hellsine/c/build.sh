#!/usr/bin/env bash
# build.sh — compile hellsine.c. No -ffast-math so summation stays stable
# across machines and matches the JS reference numerically.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra \
   -pthread \
   -o "$HERE/hellsine" \
   "$HERE/hellsine.c" \
   -lm
echo "→ $HERE/hellsine"
