#!/bin/bash

# SpiderLily Mac Build Script
# Builds SpiderLily using BuildCookRun for packaging
# Run directly on Mac (not in dev container)

set -e

# Parse arguments
SKIP_SYNC=false
BUILD_VERSION=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --no-sync|--skip-sync)
            SKIP_SYNC=true
            shift
            ;;
        *)
            BUILD_VERSION="$1"
            shift
            ;;
    esac
done

# Default version if not provided
BUILD_VERSION="${BUILD_VERSION:-$(date +%Y.%m.%d-%H%M)}"

# Mac paths
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
PROJECT_FILE="$PROJECT_ROOT/SpiderLily.uproject"
UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
OUTPUT_DIR="$HOME/Builds/$BUILD_VERSION"

echo "========================================="
echo "SpiderLily - Mac Build"
echo "========================================="
echo "Version: $BUILD_VERSION"
echo "Skip Sync: $SKIP_SYNC"
echo ""

# Verify paths
if [ ! -f "$PROJECT_FILE" ]; then
    echo "ERROR: Project not found: $PROJECT_FILE"
    exit 1
fi

if [ ! -d "$UE_ROOT" ]; then
    echo "ERROR: Unreal Engine not found: $UE_ROOT"
    exit 1
fi

# Sync latest from Perforce (unless --no-sync)
if [ "$SKIP_SYNC" = false ]; then
    echo "📥 Syncing latest from Perforce..."
    cd "$PROJECT_ROOT"
    export P4PORT=ssl:falsework.helixcore.io:1666
    export P4USER=machine
    export P4CLIENT=spiderlily_build_workspace_macmini

    /opt/homebrew/bin/p4 sync

    if [ $? -ne 0 ]; then
        echo "⚠️  P4 sync failed, continuing with existing files..."
    fi
else
    echo "⏭️  Skipping Perforce sync (--no-sync flag)"
    cd "$PROJECT_ROOT"
fi

# Fix FMOD compilation bug (uninitialized variable)
echo ""
echo "🔧 Applying FMOD plugin fix..."
sed -i.bak 's/FMOD_STUDIO_PLAYBACK_STATE pS;/FMOD_STUDIO_PLAYBACK_STATE pS = FMOD_STUDIO_PLAYBACK_STOPPED;/' \
    "$PROJECT_ROOT/Plugins/FMODStudio/Source/FMODStudio/Private/FMODBlueprintStatics.cpp" 2>/dev/null || echo "  (already applied)"

echo ""
echo "🔨 Building and cooking with BuildCookRun..."
echo "   This will take several minutes..."
echo ""

# Clean old builds
rm -rf "$OUTPUT_DIR"
rm -rf "$PROJECT_ROOT/Saved/Cooked/Mac"
rm -rf "$PROJECT_ROOT/Saved/Shaders"
rm -rf "$PROJECT_ROOT/Saved/StagedBuilds"

# Use BuildCookRun for complete packaging
"$UE_ROOT/Engine/Build/BatchFiles/RunUAT.sh" BuildCookRun \
    -project="$PROJECT_FILE" \
    -platform=Mac \
    -clientconfig=Development \
    -cook \
    -build \
    -stage \
    -pak \
    -noP4 \
    -utf8output

if [ $? -ne 0 ]; then
    echo ""
    echo "❌ Build failed"
    exit 1
fi

# Manually copy staged build to output (archive doesn't include cooked content properly)
echo ""
echo "📦 Copying staged build to archive..."
mkdir -p "$OUTPUT_DIR"
cp -R "$PROJECT_ROOT/Saved/StagedBuilds/Mac" "$OUTPUT_DIR/"

echo ""
echo "✅ Build complete!"
echo "📦 Output: $OUTPUT_DIR/Mac"

BUILD_SIZE=$(du -sh "$OUTPUT_DIR/Mac" 2>/dev/null | cut -f1)
echo "📊 Size: $BUILD_SIZE"
