#!/usr/bin/env bash
# Build the accordion engine.
set -e
cd "$(dirname "$0")"
cc -O2 -std=c11 -Wall -Wextra -o accordion accordion.c -lm
echo "built ./accordion"
