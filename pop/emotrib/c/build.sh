#!/usr/bin/env bash
# build.sh — compile emotrib.c (the emo-trap engine).
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter \
   -o "$HERE/emotrib" \
   "$HERE/emotrib.c" \
   -lm
echo "→ $HERE/emotrib"
