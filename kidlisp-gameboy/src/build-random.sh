#!/bin/bash

# Generate a random seed and inject it into the C file
RANDOM_SEED=$RANDOM

# Replace the seed value in the source file
sed -i "s/uint16_t seed = 0x[0-9A-Fa-f]*;/uint16_t seed = 0x$(printf '%04X' $RANDOM_SEED);/" demo-graphics.c

echo "Building with random seed: 0x$(printf '%04X' $RANDOM_SEED)"

# Build
/workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/bin/lcc -Wm-yc -o demo-graphics.gbc demo-graphics.c

# Copy to emulator directory
cp demo-graphics.gbc /workspaces/aesthetic-computer/system/public/aesthetic.computer/gb-emulator/

echo "Build complete!"
