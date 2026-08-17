#!/usr/bin/env bash
# build.sh — compile the loner v4 C engine.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cc -O2 -Wall -Wextra -o "$HERE/lonerremix" "$HERE/lonerremix.c" -lm
echo "✓ $HERE/lonerremix"
