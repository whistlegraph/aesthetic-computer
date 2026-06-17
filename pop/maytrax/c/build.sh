#!/bin/sh
# build maytrax.c (needs score.h — run `node c/bake.mjs` first)
cd "$(dirname "$0")"
exec cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o maytrax maytrax.c -lm
