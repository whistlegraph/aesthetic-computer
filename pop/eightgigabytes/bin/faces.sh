#!/bin/bash
# faces.sh — compile bin/faces.swift against the MenuBand face sources and
# render out/faces-<member>.mp4 for the three singers.
#   bin/faces.sh [fps] [width] [height]
set -e
HERE="$(cd "$(dirname "$0")" && pwd)"; LANE="$(dirname "$HERE")"; REPO="$(cd "$LANE/../.." && pwd)"
SRC="$REPO/slab/menuband/Sources/MenuBand"
BUILD="$LANE/out/.faces-build"; mkdir -p "$BUILD"
cat "$SRC/SingerArticulation.swift" "$SRC/SingerFaceMetal.swift" "$SRC/SingerGaze.swift" "$SRC/SingerFace.swift" "$HERE/faces.swift" > "$BUILD/main.swift"
if [ ! -x "$BUILD/faces" ] || [ "$BUILD/main.swift" -nt "$BUILD/faces" ]; then
  swiftc -O -suppress-warnings -o "$BUILD/faces" "$BUILD/main.swift"
fi
"$BUILD/faces" "$LANE/out" "${1:-30}" "${2:-480}" "${3:-300}"
