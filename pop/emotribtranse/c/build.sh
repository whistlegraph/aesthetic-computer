#!/usr/bin/env bash
# build.sh — compile emotribtranse.c (the emo-trance engine).
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter \
   -o "$HERE/emotribtranse" \
   "$HERE/emotribtranse.c" \
   -lm
echo "→ $HERE/emotribtranse"
