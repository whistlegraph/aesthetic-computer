#!/usr/bin/env bash
# build.sh — compile boombaboom.c (sine bells + sine kick engine).
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter \
   -o "$HERE/boombaboom" \
   "$HERE/boombaboom.c" \
   -lm
echo "→ $HERE/boombaboom"
