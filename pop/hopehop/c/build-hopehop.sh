#!/usr/bin/env bash
# build-hopehop.sh — compile hopehop.c. No -ffast-math so additive
# summation stays stable across machines (matches amaythingra).
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra \
   -o "$HERE/hopehop" \
   "$HERE/hopehop.c" \
   -lm
echo "→ $HERE/hopehop"
