#!/usr/bin/env fish

# Build with timestamp
set TIMESTAMP (date +"%Y%m%d-%H%M%S")
set ROM_NAME "demo-graphics-$TIMESTAMP.gbc"

echo "Building timestamped ROM: $ROM_NAME"

# Build
/workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/bin/lcc -Wm-yc -o $ROM_NAME demo-graphics.c

if test $status -eq 0
    # Copy timestamped version to builds archive
    mkdir -p /workspaces/aesthetic-computer/kidlisp-gameboy/builds
    cp $ROM_NAME /workspaces/aesthetic-computer/kidlisp-gameboy/builds/
    
    # Also copy as current version to emulator directory
    cp $ROM_NAME /workspaces/aesthetic-computer/system/public/aesthetic.computer/gb-emulator/demo-graphics.gbc
    
    echo "✓ Build complete!"
    echo "  Timestamped: /workspaces/aesthetic-computer/kidlisp-gameboy/builds/$ROM_NAME"
    echo "  Current: /workspaces/aesthetic-computer/system/public/aesthetic.computer/gb-emulator/demo-graphics.gbc"
else
    echo "✗ Build failed!"
end
