#!/bin/sh
set -eu
HERE="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
cc -O3 -std=c11 -Wall -Wextra -o "$HERE/spatial-sineabye" "$HERE/spatial-sineabye.c" -lm
echo "built $HERE/spatial-sineabye"
