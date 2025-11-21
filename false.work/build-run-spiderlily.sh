#!/bin/bash

# SpiderLily Mac Build Script
# Builds SpiderLily using BuildCookRun for packaging
# Run directly on Mac (not in dev container)

set -e

# Accept version parameter (for automated builds)
BUILD_VERSION="${1:-$(date +%Y.%m.%d-%H%M)}"

# Mac paths
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
PROJECT_FILE="$PROJECT_ROOT/SpiderLily.uproject"
UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
OUTPUT_DIR="$HOME/Builds/$BUILD_VERSION"

echo "========================================="
echo "SpiderLily - Mac Build"
echo "========================================="
echo "Version: $BUILD_VERSION"
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

# Sync latest from Perforce
echo "📥 Syncing latest from Perforce..."
cd "$PROJECT_ROOT"
export P4PORT=ssl:falsework.helixcore.io:1666
export P4USER=machine
export P4CLIENT=spiderlily_build_workspace_macmini

/opt/homebrew/bin/p4 sync

if [ $? -ne 0 ]; then
    echo "⚠️  P4 sync failed, continuing with existing files..."
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
    -archive \
    -archivedirectory="$OUTPUT_DIR" \
    -noP4 \
    -utf8output

if [ $? -ne 0 ]; then
    echo ""
    echo "❌ Build failed"
    exit 1
fi

echo ""
echo "✅ Build complete!"
echo "📦 Output: $OUTPUT_DIR/Mac"

BUILD_SIZE=$(du -sh "$OUTPUT_DIR/Mac" 2>/dev/null | cut -f1)
echo "📊 Size: $BUILD_SIZE"
