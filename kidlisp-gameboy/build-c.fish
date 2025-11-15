#!/usr/bin/env fish
# build-c.fish - Build GameBoy ROM from C using GBDK
# Usage: ./build-c.fish <rom-name>
# Example: ./build-c.fish hello

if test (count $argv) -lt 1
    echo "Usage: ./build-c.fish <rom-name>"
    echo ""
    echo "Examples:"
    echo "  ./build-c.fish hello"
    echo "  ./build-c.fish circle"
    exit 1
end

set ROM_NAME $argv[1]
set WORKSPACE_ROOT /workspaces/aesthetic-computer
set GBDK_PATH "$WORKSPACE_ROOT/kidlisp-gameboy/gbdk"
set LCC "$GBDK_PATH/bin/lcc"
set SRC_FILE "$WORKSPACE_ROOT/kidlisp-gameboy/src/$ROM_NAME.c"
set BUILD_TARGET "DMG"
set OUTPUT_EXT "gb"
if grep -q "BUILD_TARGET: CGB" "$SRC_FILE"
    set BUILD_TARGET "CGB"
    set OUTPUT_EXT "gbc"
end

set OUTPUT_FILE "$ROM_NAME.$OUTPUT_EXT"
set GB_FILE "$WORKSPACE_ROOT/kidlisp-gameboy/src/$OUTPUT_FILE"
set DEPLOY_PATH "$WORKSPACE_ROOT/system/public/aesthetic.computer/gb-emulator/$OUTPUT_FILE"

# Check if the C file exists
if not test -f "$SRC_FILE"
    echo "❌ Error: src/$ROM_NAME.c not found"
    exit 1
end

echo "🔨 Building $ROM_NAME ($BUILD_TARGET) with GBDK..."

# Compile with lcc
cd $WORKSPACE_ROOT/kidlisp-gameboy/src
if test "$BUILD_TARGET" = "CGB"
    $LCC -Wm-yc -o $OUTPUT_FILE $ROM_NAME.c
else
    $LCC -o $OUTPUT_FILE $ROM_NAME.c
end

if test $status -eq 0
    echo "✅ Compilation successful!"
    
    # Copy to gb-emulator directory
    cp $GB_FILE $DEPLOY_PATH
    
    # Clean up intermediate files
    rm -f $ROM_NAME.o $ROM_NAME.lst $ROM_NAME.map $ROM_NAME.sym
    
    echo "✅ Deployed to gb-emulator/"
    echo ""
    echo "🌐 Load with:"
    echo "   ac gameboy~$ROM_NAME"
    echo ""
else
    echo "❌ Compilation failed!"
    exit 1
end
