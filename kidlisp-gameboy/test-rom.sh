#!/usr/bin/env bash
# Test a GameBoy ROM using gnuboy SDL emulator
# Usage: ./test-rom.sh <rom-file>

if [ -z "$1" ]; then
    echo "Usage: $0 <rom-file.gb>"
    exit 1
fi

ROM="$1"

if [ ! -f "$ROM" ]; then
    echo "Error: ROM file not found: $ROM"
    exit 1
fi

echo "Testing ROM: $ROM"
echo "Press ESC to quit the emulator"
echo ""

# Run with SDL
sdlgnuboy "$ROM"
