#!/usr/bin/env bash
# Build the bell engine. No -ffast-math (keeps summation stable / JS-parity).
set -e
cd "$(dirname "$0")"
cc -O3 -std=c11 -Wall -Wextra -o bell bell.c -lm
echo "built ./bell"
