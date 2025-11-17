#!/bin/bash

# Build script for Game Boy ROMs
# Automatically detects if source needs CGB (Color Game Boy) or DMG (original Game Boy)
# Usage: ./build.sh <source.c> [output_name]

if [ $# -lt 1 ]; then
    echo "Usage: build.sh <source.c> [output_name]"
    echo "Example: ./build.sh src/melody.c"
    echo "Example: ./build.sh src/scrub.c scrub"
    exit 1
fi

SOURCE_FILE="$1"
GBDK_PATH="$(dirname "$0")/gbdk"
LCC="$GBDK_PATH/bin/lcc"

# Get base name for output
if [ $# -ge 2 ]; then
    OUTPUT_NAME="$2"
else
    OUTPUT_NAME="$(basename "$SOURCE_FILE" .c)"
fi

# Detect if this needs CGB support
IS_CGB=0

# Check for BUILD_TARGET comment
if grep -q "BUILD_TARGET: CGB" "$SOURCE_FILE"; then
    IS_CGB=1
    echo "📟 Detected BUILD_TARGET: CGB"
elif grep -q "#include <gb/cgb.h>" "$SOURCE_FILE"; then
    IS_CGB=1
    echo "📟 Detected CGB include"
elif grep -qE "BCPS_REG|BCPD_REG|set_sprite_palette|set_bkg_palette" "$SOURCE_FILE"; then
    IS_CGB=1
    echo "📟 Detected CGB API usage"
fi

# Build command
if [ $IS_CGB -eq 1 ]; then
    EXTENSION="gbc"
    FLAGS="-Wm-yc"
    echo "🎨 Building as Game Boy Color ROM..."
else
    EXTENSION="gb"
    FLAGS=""
    echo "🎮 Building as original Game Boy ROM..."
fi

OUTPUT_FILE="$OUTPUT_NAME.$EXTENSION"

# Compile
echo "🔨 Compiling: $SOURCE_FILE -> $OUTPUT_FILE"
if [ -n "$FLAGS" ]; then
    "$LCC" $FLAGS -o "$OUTPUT_FILE" "$SOURCE_FILE"
else
    "$LCC" -o "$OUTPUT_FILE" "$SOURCE_FILE"
fi

if [ $? -eq 0 ]; then
    echo "✅ Build successful: $OUTPUT_FILE"
    
    # Copy to assets directory if it exists
    ASSETS_DIR="$(dirname "$0")/../system/public/assets/gameboy"
    if [ -d "$ASSETS_DIR" ]; then
        cp "$OUTPUT_FILE" "$ASSETS_DIR/"
        echo "📦 Copied to: $ASSETS_DIR/$OUTPUT_FILE"
    fi
    
    # Auto-run in aesthetic.computer if OUTPUT_NAME matches
    if [ "$OUTPUT_NAME" = "melody" ] || [ "$OUTPUT_NAME" = "scrub" ]; then
        echo "🎮 Launching: ac gameboy~$OUTPUT_NAME"
        fish -c "cd $(dirname "$0")/.. && ac gameboy~$OUTPUT_NAME"
    fi
else
    echo "❌ Build failed"
    exit 1
fi
