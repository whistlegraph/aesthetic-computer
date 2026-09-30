#!/usr/bin/env bash
# build.sh — compile the sailor-song C engine (regenerates the chart first).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
node "$HERE/../bin/chart.mjs"
cc -O2 -Wall -Wextra -o "$HERE/sailorremix" "$HERE/sailorremix.c" -lm
echo "✓ $HERE/sailorremix"
