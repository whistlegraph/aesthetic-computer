#!/bin/bash

# Build script for Game Boy ROMs
# Automatically detects if source needs CGB (Color Game Boy) or DMG (original Game Boy)
# Usage: ./build.sh <source.c> [output_name]
# Usage: ./build.sh <source.lisp> [output_name]  # Compiles KidLisp first

if [ $# -lt 1 ]; then
    echo "Usage: build.sh <source.c|source.lisp> [output_name]"
    echo "Example: ./build.sh src/melody.c"
    echo "Example: ./build.sh examples/lines.lisp"
    exit 1
fi

INPUT_FILE="$1"

# If input is .lisp, compile it first
if [[ "$INPUT_FILE" == *.lisp ]]; then
    echo "📜 Compiling KidLisp source..."
    BASENAME=$(basename "$INPUT_FILE" .lisp)
    ./compiler/kidlisp-to-gb.mjs "$INPUT_FILE" "src/${BASENAME}.c"
    SOURCE_FILE="src/${BASENAME}.c"
else
    SOURCE_FILE="$INPUT_FILE"
fi
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
if grep -q "hUGEDriver.h" "$SOURCE_FILE"; then
    # Include hUGEDriver for music support
    "$LCC" $FLAGS -Ihugedriver/include -Wl-lhugedriver/gbdk/hUGEDriver.lib -o "$OUTPUT_FILE" "$SOURCE_FILE"
elif [ -n "$FLAGS" ]; then
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
