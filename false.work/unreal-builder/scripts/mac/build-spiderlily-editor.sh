#!/bin/bash
# Build SpiderLily for Mac
set -e

UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
UBT="$UE_ROOT/Engine/Binaries/DotNET/UnrealBuildTool/UnrealBuildTool"
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
PROJECT_FILE="$PROJECT_ROOT/SpiderLily.uproject"

echo "========================================="
echo "Building SpiderLily for Mac"
echo "========================================="
echo ""
echo "UE Root: $UE_ROOT"
echo "Project: $PROJECT_FILE"
echo "UnrealBuildTool: $UBT"
echo ""

# Check if files exist
if [ ! -f "$UBT" ]; then
    echo "❌ ERROR: UnrealBuildTool not found at $UBT"
    exit 1
fi

if [ ! -f "$PROJECT_FILE" ]; then
    echo "❌ ERROR: Project file not found at $PROJECT_FILE"
    exit 1
fi

echo "✓ Files verified, starting build..."
echo ""

# Run UnrealBuildTool to build the editor
echo "🔨 Building SpiderLily Editor (Development Mac)..."
"$UBT" SpiderLilyEditor Mac Development "$PROJECT_FILE" -waitmutex -NoHotReload

echo ""
echo "✅ Build complete!"
