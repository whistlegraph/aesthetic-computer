#!/usr/bin/env bash
# build.sh — compile the goatshard engine.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p build ../out
cc -O2 -std=c99 -Wall -o build/goatshard goatshard.c -lm
echo "build/goatshard"
