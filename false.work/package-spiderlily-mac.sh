#!/bin/bash

set -e

# Project paths
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
PROJECT_FILE="$PROJECT_ROOT/SpiderLily.uproject"
OUTPUT_DIR="$PROJECT_ROOT/Packaged/Mac"
UE_ROOT="/Users/Shared/Epic Games/UE_5.6"

echo "========================================="
echo "Packaging SpiderLily for Mac"
echo "========================================="
echo ""
echo "Project: $PROJECT_FILE"
echo "Output: $OUTPUT_DIR"
echo ""

# Verify files exist
if [ ! -f "$PROJECT_FILE" ]; then
    echo "ERROR: Project file not found: $PROJECT_FILE"
    exit 1
fi

if [ ! -d "$UE_ROOT" ]; then
    echo "ERROR: Unreal Engine not found: $UE_ROOT"
    exit 1
fi

echo "✓ Files verified, starting package..."
echo ""

# Run BuildCookRun for Mac
echo "🔧 Building for Mac..."
echo "🚀 Running BuildCookRun..."
echo ""

"$UE_ROOT/Engine/Build/BatchFiles/RunUAT.sh" BuildCookRun \
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
    -utf8output \
    -nocodesign

if [ $? -eq 0 ]; then
    echo ""
    echo "========================================="
    echo "✅ Mac build completed successfully!"
    echo "========================================="
    echo ""
    echo "Output location: $OUTPUT_DIR"
    
    # Find the .app
    APP_PATH=$(find "$OUTPUT_DIR" -name "*.app" -type d | head -n 1)
    if [ -n "$APP_PATH" ]; then
        echo "App bundle: $APP_PATH"
        APP_SIZE=$(du -sh "$APP_PATH" | cut -f1)
        echo "Size: $APP_SIZE"
    fi
else
    echo ""
    echo "========================================="
    echo "❌ Build failed!"
    echo "========================================="
    exit 1
fi
