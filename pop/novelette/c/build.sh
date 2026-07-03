#!/usr/bin/env bash
# build.sh — compile the novelette engine.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p build ../out
cc -O2 -std=c99 -Wall -o build/novelette novelette.c -lm
echo "build/novelette"
