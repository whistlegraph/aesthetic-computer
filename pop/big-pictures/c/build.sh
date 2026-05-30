#!/usr/bin/env bash
# build.sh — compile amazinhym.c (Amazing Grace hymn variant).
# No -ffast-math so additive summation stays stable across machines.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra \
   -o "$HERE/amazinhym" \
   "$HERE/amazinhym.c" \
   -lm
echo "→ $HERE/amazinhym"
