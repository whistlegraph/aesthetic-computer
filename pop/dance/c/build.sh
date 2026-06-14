#!/usr/bin/env bash
# build.sh — compile wobble.c. No -ffast-math so the numerics stay stable
# across machines and match the JS reference (pop/dance/synths/wobble.mjs).
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra \
   -o "$HERE/wobble" \
   "$HERE/wobble.c" \
   -lm
echo "→ $HERE/wobble"
