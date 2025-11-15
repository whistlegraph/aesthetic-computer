#!/usr/bin/env bash
# GameBoy ROM Terminal Emulator Test Script
# Usage: ./test-gb-rom.sh <rom-file.gb>

if [ -z "$1" ]; then
    echo "Usage: $0 <rom-file.gb>"
    echo ""
    echo "Examples:"
    echo "  $0 hello-world.gb"
    echo "  $0 test-minimal.gb"
    exit 1
fi

ROM="$1"

# If ROM path is relative, make it absolute from kidlisp-gameboy dir
if [[ ! "$ROM" = /* ]]; then
    ROM="/workspaces/aesthetic-computer/kidlisp-gameboy/$ROM"
fi

if [ ! -f "$ROM" ]; then
    echo "Error: ROM file not found: $ROM"
    exit 1
fi

echo "🎮 Testing GameBoy ROM: $ROM"
echo "Press Ctrl+C to quit"
echo ""

cd /workspaces/aesthetic-computer/kidlisp-gameboy/gameboy-emulator/examples/terminal
RUSTUP_TOOLCHAIN=nightly cargo run --release -- "$ROM"
