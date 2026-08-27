#!/usr/bin/env bash
# build.sh — compile the loner review-score C renderer.
# (wg.bin comes from convert-data.py; re-run that only if the npz/json/fonts change.)
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cc -O2 -Wall -Wextra -o "$HERE/scorecast" "$HERE/scorecast.c" -lm
echo "✓ $HERE/scorecast"
