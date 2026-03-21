#!/bin/bash
# Build SpiderLily for Mac with FMOD header fixes
set -e

UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
UBT="$UE_ROOT/Engine/Binaries/DotNET/UnrealBuildTool/UnrealBuildTool"
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
PROJECT_FILE="$PROJECT_ROOT/SpiderLily.uproject"

echo "========================================="
echo "Building SpiderLily for Mac"
echo "========================================="
echo ""

# Clean FMOD intermediate to force regeneration
echo "🧹 Cleaning FMOD intermediate files..."
rm -rf "$PROJECT_ROOT/Plugins/FMODStudio/Intermediate/Build/Mac"
rm -rf "$PROJECT_ROOT/Intermediate/Build/Mac"

echo "🔨 Running UnrealBuildTool..."
"$UBT" SpiderLilyEditor Mac Development "$PROJECT_FILE" -waitmutex -NoHotReload 2>&1 | tee /tmp/build.log

# Check if it failed with the callback handler error
if grep -q "allbackHandler.h" /tmp/build.log; then
    echo ""
    echo "⚠️  Detected FMOD header generation bug, applying fix..."
    
    # Fix the malformed include
    CALLBACK_FILE="$PROJECT_ROOT/Plugins/FMODStudio/Intermediate/Build/Mac/UnrealEditor/Inc/FMODStudio/UHT/FMODCallbackHandler.gen.cpp"
    if [ -f "$CALLBACK_FILE" ]; then
        sed -i.bak 's/allbackHandler\.h/FMODCallbackHandler.h/g' "$CALLBACK_FILE"
        echo "✅ Fixed: $CALLBACK_FILE"
    fi
    
    echo ""
    echo "🔨 Retrying build..."
    "$UBT" SpiderLilyEditor Mac Development "$PROJECT_FILE" -waitmutex -NoHotReload
fi

echo ""
echo "✅ Build complete!"
