#!/bin/sh
set -eu
HERE="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
if [ "${1:-}" = "--hd" ]; then
  OUT="$HERE/spatial-sineabye-hd"
  cc -O3 -std=c11 -Wall -Wextra -DW=1440 -DH=1440 -o "$OUT" "$HERE/spatial-sineabye.c" -lm
else
  OUT="$HERE/spatial-sineabye"
  cc -O3 -std=c11 -Wall -Wextra -o "$OUT" "$HERE/spatial-sineabye.c" -lm
fi
echo "built $OUT"
