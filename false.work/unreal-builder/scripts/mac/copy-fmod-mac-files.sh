#!/bin/bash
# Copy FMOD Mac binaries and libraries to Perforce workspace
# Only copies Mac-specific files, doesn't touch shared directories

set -e

SOURCE_DIR="$HOME/Downloads/fmodstudio20310ue5.6mac"
DEST_DIR="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main/Plugins"

echo "🔧 Copying FMOD Mac binaries to Perforce workspace..."
echo ""

# Copy FMODStudio Mac files
echo "=== FMODStudio ==="

# Copy Mac binaries
if [ -d "$SOURCE_DIR/FMODStudio/Binaries/Mac" ]; then
    echo "✓ Copying Binaries/Mac..."
    mkdir -p "$DEST_DIR/FMODStudio/Binaries"
    rsync -av "$SOURCE_DIR/FMODStudio/Binaries/Mac/" "$DEST_DIR/FMODStudio/Binaries/Mac/"
else
    echo "⚠ No Mac binaries found"
fi

# Copy Mac libraries
if [ -d "$SOURCE_DIR/FMODStudio/Libs/Mac" ]; then
    echo "✓ Copying Libs/Mac..."
    mkdir -p "$DEST_DIR/FMODStudio/Libs"
    rsync -av "$SOURCE_DIR/FMODStudio/Libs/Mac/" "$DEST_DIR/FMODStudio/Libs/Mac/"
else
    echo "⚠ No Mac libraries found"
fi

# Copy IOS binaries (might be needed for Mac builds)
if [ -d "$SOURCE_DIR/FMODStudio/Binaries/IOS" ]; then
    echo "✓ Copying Binaries/IOS..."
    mkdir -p "$DEST_DIR/FMODStudio/Binaries"
    rsync -av "$SOURCE_DIR/FMODStudio/Binaries/IOS/" "$DEST_DIR/FMODStudio/Binaries/IOS/"
fi

# Copy TVOS binaries (might be needed)
if [ -d "$SOURCE_DIR/FMODStudio/Binaries/TVOS" ]; then
    echo "✓ Copying Binaries/TVOS..."
    mkdir -p "$DEST_DIR/FMODStudio/Binaries"
    rsync -av "$SOURCE_DIR/FMODStudio/Binaries/TVOS/" "$DEST_DIR/FMODStudio/Binaries/TVOS/"
fi

echo ""
echo "=== FMODStudioNiagara ==="

# Copy FMODStudioNiagara if it has Mac-specific files
if [ -d "$SOURCE_DIR/FMODStudioNiagara" ]; then
    if [ -d "$SOURCE_DIR/FMODStudioNiagara/Binaries/Mac" ]; then
        echo "✓ Copying FMODStudioNiagara Binaries/Mac..."
        mkdir -p "$DEST_DIR/FMODStudioNiagara/Binaries"
        rsync -av "$SOURCE_DIR/FMODStudioNiagara/Binaries/Mac/" "$DEST_DIR/FMODStudioNiagara/Binaries/Mac/"
    fi
    
    if [ -d "$SOURCE_DIR/FMODStudioNiagara/Libs/Mac" ]; then
        echo "✓ Copying FMODStudioNiagara Libs/Mac..."
        mkdir -p "$DEST_DIR/FMODStudioNiagara/Libs"
        rsync -av "$SOURCE_DIR/FMODStudioNiagara/Libs/Mac/" "$DEST_DIR/FMODStudioNiagara/Libs/Mac/"
    fi
fi

echo ""
echo "✅ FMOD Mac files copied successfully!"
echo ""
echo "📁 Files copied to:"
echo "   $DEST_DIR/FMODStudio/Binaries/Mac/"
echo "   $DEST_DIR/FMODStudio/Libs/Mac/"
echo ""
echo "🔍 Verify with:"
echo "   ls -la $DEST_DIR/FMODStudio/Binaries/"
echo "   ls -la $DEST_DIR/FMODStudio/Libs/"
