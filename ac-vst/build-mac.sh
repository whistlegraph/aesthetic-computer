#!/bin/bash
# AC Notepat VST - macOS Build Script
# Run this on the Mac host to build the plugin

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$SCRIPT_DIR/build"
JUCE_DIR="$SCRIPT_DIR/JUCE"

echo "🎹 AC Notepat VST Build Script"
echo "=============================="

# Check for Xcode
if ! xcode-select -p &>/dev/null; then
    echo "❌ Xcode command line tools not installed!"
    echo "   Run: xcode-select --install"
    exit 1
fi

# Clone JUCE if not present
if [ ! -d "$JUCE_DIR" ]; then
    echo "📦 Cloning JUCE framework..."
    git clone --depth 1 --branch 8.0.0 https://github.com/juce-framework/JUCE.git "$JUCE_DIR"
fi

# Create build directory
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# Configure with CMake
echo "🔧 Configuring with CMake..."
cmake .. -G "Xcode" \
    -DCMAKE_OSX_DEPLOYMENT_TARGET=11.0 \
    -DCMAKE_OSX_ARCHITECTURES="arm64;x86_64"

# Build
echo "🏗️ Building..."
cmake --build . --config Release

# Find the built plugin
echo ""
echo "✅ Build complete!"
echo ""
echo "📍 Built plugins:"
find . -name "*.vst3" -o -name "*.component" 2>/dev/null | head -20

# Install to user plugin directories
echo ""
echo "📦 Installing plugins..."

VST3_SRC=$(find . -name "ACNotepat.vst3" -type d | head -1)
AU_SRC=$(find . -name "ACNotepat.component" -type d | head -1)

if [ -n "$VST3_SRC" ]; then
    VST3_DEST="$HOME/Library/Audio/Plug-Ins/VST3/ACNotepat.vst3"
    rm -rf "$VST3_DEST"
    cp -R "$VST3_SRC" "$VST3_DEST"
    echo "   ✓ VST3 installed to: $VST3_DEST"
fi

if [ -n "$AU_SRC" ]; then
    AU_DEST="$HOME/Library/Audio/Plug-Ins/Components/ACNotepat.component"
    rm -rf "$AU_DEST"
    cp -R "$AU_SRC" "$AU_DEST"
    echo "   ✓ AU installed to: $AU_DEST"
fi

echo ""
echo "🎉 Done! Restart Ableton to load the plugin."
echo "   Look for 'AC Notepat' in the plugin browser."
