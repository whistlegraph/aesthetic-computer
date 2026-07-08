#!/usr/bin/env bash
# build.sh — compile the marimba C engines (fluttabap360 + flatterbop180).
# No -ffast-math so the numerics stay stable across machines and match the JS
# reference composers, exactly like pop/nullabye/c/build.sh + pop/hellsine/c/build.sh.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
for eng in fluttabap360 flatterbop180; do
  cc -O3 -std=c11 -Wall -Wextra \
     -o "$HERE/$eng" \
     "$HERE/$eng.c" \
     -lm
  echo "→ $HERE/$eng"
done
