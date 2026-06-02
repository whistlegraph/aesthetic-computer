#!/usr/bin/env bash
# build-amaythingra.sh — compile amaythingra.c. No -ffast-math so additive
# summation stays stable across machines.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra \
   -o "$HERE/amaythingra" \
   "$HERE/amaythingra.c" \
   -lm
echo "→ $HERE/amaythingra"
