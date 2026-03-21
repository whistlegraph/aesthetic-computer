#!/bin/bash

# Build with timestamp
TIMESTAMP=$(date +"%Y%m%d-%H%M%S")
ROM_NAME="demo-graphics-$TIMESTAMP.gbc"

echo "Building timestamped ROM: $ROM_NAME"

# Build
/workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/bin/lcc -Wm-yc -o "$ROM_NAME" demo-graphics.c

if [ $? -eq 0 ]; then
    # Copy timestamped version to builds archive
    mkdir -p /workspaces/aesthetic-computer/kidlisp-gameboy/builds
    cp "$ROM_NAME" /workspaces/aesthetic-computer/kidlisp-gameboy/builds/
    
    # Copy timestamped version to emulator directory
    cp "$ROM_NAME" /workspaces/aesthetic-computer/system/public/aesthetic.computer/gb-emulator/
    
    echo "✓ Build complete!"
    echo "  Archived: /workspaces/aesthetic-computer/kidlisp-gameboy/builds/$ROM_NAME"
    echo "  Emulator: /workspaces/aesthetic-computer/system/public/aesthetic.computer/gb-emulator/$ROM_NAME"
    echo ""
    echo "Running: ac gameboy~demo-graphics-$TIMESTAMP"
else
    echo "✗ Build failed!"
    exit 1
fi
