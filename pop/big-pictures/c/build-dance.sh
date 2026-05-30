#!/usr/bin/env bash
# build-dance.sh — compile amazing-dance.c. No -ffast-math so additive
# summation stays stable across machines.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra \
   -o "$HERE/amazing-dance" \
   "$HERE/amazing-dance.c" \
   -lm
echo "→ $HERE/amazing-dance"
