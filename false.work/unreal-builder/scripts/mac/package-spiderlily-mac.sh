#!/bin/bash
# Package SpiderLily for Mac Distribution
set -e

UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
UAT="$UE_ROOT/Engine/Build/BatchFiles/RunUAT.sh"
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
PROJECT_FILE="$PROJECT_ROOT/SpiderLily.uproject"
OUTPUT_DIR="$PROJECT_ROOT/Packaged/Mac"

echo "========================================="
echo "Packaging SpiderLily for Mac"
echo "========================================="
echo ""
echo "Project: $PROJECT_FILE"
echo "Output: $OUTPUT_DIR"
echo ""

# Check if files exist
if [ ! -f "$UAT" ]; then
    echo "❌ ERROR: RunUAT not found at $UAT"
    exit 1
fi

if [ ! -f "$PROJECT_FILE" ]; then
    echo "❌ ERROR: Project file not found at $PROJECT_FILE"
    exit 1
fi

echo "✓ Files verified, starting package..."
echo ""

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Function to fix FMOD header bug
fix_fmod_headers() {
    echo "🔧 Checking for FMOD header generation bugs..."
    local callback_file_game="$PROJECT_ROOT/Plugins/FMODStudio/Intermediate/Build/Mac/UnrealGame/Inc/FMODStudio/UHT/FMODCallbackHandler.gen.cpp"
    local callback_file_editor="$PROJECT_ROOT/Plugins/FMODStudio/Intermediate/Build/Mac/UnrealEditor/Inc/FMODStudio/UHT/FMODCallbackHandler.gen.cpp"
    
    if [ -f "$callback_file_game" ] && grep -q "allbackHandler.h" "$callback_file_game"; then
        sed -i.bak 's/allbackHandler\.h/FMODCallbackHandler.h/g' "$callback_file_game"
        echo "  ✓ Fixed: UnrealGame/FMODCallbackHandler.gen.cpp"
    fi
    
    if [ -f "$callback_file_editor" ] && grep -q "allbackHandler.h" "$callback_file_editor"; then
        sed -i.bak 's/allbackHandler\.h/FMODCallbackHandler.h/g' "$callback_file_editor"
        echo "  ✓ Fixed: UnrealEditor/FMODCallbackHandler.gen.cpp"
    fi
}

# Run Unreal Automation Tool to build and package
echo "🚀 Running BuildCookRun (attempt 1)..."
"$UAT" BuildCookRun \
    -project="$PROJECT_FILE" \
    -platform=Mac \
    -clientconfig=Development \
    -serverconfig=Development \
    -cook \
    -allmaps \
    -build \
    -stage \
    -pak \
    -archive \
    -archivedirectory="$OUTPUT_DIR" \
    -noP4 \
    -utf8output 2>&1 | tee /tmp/build.log

# Check if it failed with FMOD bug
if grep -q "allbackHandler.h" /tmp/build.log; then
    echo ""
    echo "⚠️  Detected FMOD header bug, applying fix and retrying..."
    fix_fmod_headers
    
    echo ""
    echo "🚀 Running BuildCookRun (attempt 2)..."
    "$UAT" BuildCookRun \
        -project="$PROJECT_FILE" \
        -platform=Mac \
        -clientconfig=Development \
        -serverconfig=Development \
        -cook \
        -allmaps \
        -build \
        -stage \
        -pak \
        -archive \
        -archivedirectory="$OUTPUT_DIR" \
        -noP4 \
        -utf8output
fi

echo ""
echo "✅ Build complete!"
echo ""

# Fix archiving - UE's archive step copies the wrong .app (from Binaries instead of StagedBuilds)
# So we need to manually copy the correct one with all the UE content
STAGED_APP="$PROJECT_ROOT/Saved/StagedBuilds/Mac/SpiderLily.app"
if [ -d "$STAGED_APP" ]; then
    echo "📦 Copying staged .app to archive directory..."
    rm -rf "$OUTPUT_DIR/SpiderLily.app"
    cp -R "$STAGED_APP" "$OUTPUT_DIR/SpiderLily.app"
    echo "  ✓ Copied: $STAGED_APP -> $OUTPUT_DIR/SpiderLily.app"
fi

echo ""
echo "✅ Package complete!"
echo ""
echo "📦 Output location:"
echo "   $OUTPUT_DIR/SpiderLily.app"
echo ""
echo "🎮 To test, run:"
echo "   open $OUTPUT_DIR/SpiderLily.app"
