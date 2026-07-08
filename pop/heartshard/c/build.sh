#!/usr/bin/env bash
# build.sh — compile the heartshard engine.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p build ../out
cc -O2 -std=c99 -Wall -o build/heartshard heartshard.c -lm
echo "build/heartshard"
