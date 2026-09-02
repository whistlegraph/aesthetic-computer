#!/usr/bin/env bash
# Build the strum engine.
set -e
cd "$(dirname "$0")"
cc -O2 -std=c11 -Wall -Wextra -o strum strum.c -lm
echo "built ./strum"
