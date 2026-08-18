#!/usr/bin/env bash
# build.sh — compile the factory v3 C engine.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cc -O2 -Wall -Wextra -o "$HERE/factoryremix" "$HERE/factoryremix.c" -lm
echo "✓ $HERE/factoryremix"
