#!/bin/bash

# Safe cleanup script for the recording directory
# Only removes render artifacts, preserves source files

echo "🧹 Cleaning up recording artifacts..."

# Remove render state
if [ -f "state.json" ]; then
    rm state.json
    echo "✅ Removed state.json"
fi

# Remove frame data files (but not .mjs source files)
if ls frame-*.rgb >/dev/null 2>&1; then
    rm frame-*.rgb
    echo "✅ Removed frame-*.rgb files"
fi

# Remove background buffer
if [ -f "background-buffer.bin" ]; then
    rm background-buffer.bin
    echo "✅ Removed background-buffer.bin"
fi

# Remove concatenated frames file
if [ -f "all-frames.rgb" ]; then
    rm all-frames.rgb
    echo "✅ Removed all-frames.rgb"
fi

echo "🎉 Recording artifacts cleaned (source files preserved)"

# Usage note
echo ""
echo "💡 Use this instead of 'rm -rf frame-*' to avoid deleting frame-renderer.mjs"