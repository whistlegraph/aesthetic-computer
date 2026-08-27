#!/usr/bin/env bash
# build.sh — compile the cult remix C engine (same flags as the sibling lanes)
set -euo pipefail
cd "$(dirname "$0")"
cc -O2 -o cultremix cultremix.c -lm
echo "built ./cultremix"
