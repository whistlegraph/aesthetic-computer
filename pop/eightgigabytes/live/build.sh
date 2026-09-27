#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")/../../.."
LANE=pop/eightgigabytes
BUILD="$LANE/out/live/build"
mkdir -p "$BUILD"
SRC=slab/menuband/Sources/MenuBand
cat "$SRC/MenuBandSinger.swift" "$SRC/SingerArticulation.swift" "$SRC/SingerFaceMetal.swift" "$SRC/SingerGaze.swift" "$SRC/SingerFace.swift" "$SRC/LyricCaption.swift" "$LANE/live/performer.swift" > "$BUILD/main.swift"
clang -O3 -c "$LANE/live/engine.c" -o "$BUILD/engine.o"
for source in util biquad eq_graph comp_1176 chain; do
 clang -O3 -ffast-math -DNDEBUG -c "pop/dsp/c/src/$source.c" -o "$BUILD/$source.o"
done
# The same already-built WORLD/singer objects used by the reference singrender.
CORE=slab/menuband/.build/out/Intermediates.noindex/MenuBand.build/Release/CSinger-t.build/Objects-normal/arm64
swiftc -O -suppress-warnings -I slab/menuband/Sources/CSinger/include -import-objc-header "$LANE/live/engine.h" "$BUILD/main.swift" "$BUILD/engine.o" "$BUILD/util.o" "$BUILD/biquad.o" "$BUILD/eq_graph.o" "$BUILD/comp_1176.o" "$BUILD/chain.o" "$CORE"/*.o -lc++ -framework AudioToolbox -o "$LANE/out/live/performer"
