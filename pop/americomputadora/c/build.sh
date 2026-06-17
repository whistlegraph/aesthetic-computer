#!/bin/sh
# build americomputadora.c (needs score.h — run `node c/bake.mjs` first)
cd "$(dirname "$0")"
exec cc -O2 -Wall -Wextra -Wno-unused-parameter -o americomputadora americomputadora.c -lm
