#!/usr/bin/env bash
# build.sh — compile wattajetta.c. No -ffast-math so the numerics stay
# stable across machines.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra \
   -o "$HERE/wattajetta" \
   "$HERE/wattajetta.c" \
   -lm
echo "→ $HERE/wattajetta"
