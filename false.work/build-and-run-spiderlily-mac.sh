#!/bin/bash

set -e

# Project paths
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
PROJECT_FILE="$PROJECT_ROOT/SpiderLily.uproject"
UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
APP_PATH="$PROJECT_ROOT/Binaries/Mac/SpiderLily.app"

echo "========================================="
echo "Building and Running SpiderLily for Mac"
echo "========================================="
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

# Kill any running instances
echo "🛑 Stopping any running instances..."
pkill -9 SpiderLily 2>/dev/null || true
pkill -9 UnrealEditor 2>/dev/null || true

# Clean intermediate files for fresh build
echo "🧹 Cleaning intermediate files..."
rm -rf "$PROJECT_ROOT/Intermediate/Build"
rm -rf "$PROJECT_ROOT/Binaries/Mac/SpiderLily.app"

# Build the project
echo ""
echo "🔨 Building SpiderLily Editor target..."
"$UE_ROOT/Engine/Build/BatchFiles/Mac/Build.sh" \
    SpiderLilyEditor \
    Mac \
    Development \
    -Project="$PROJECT_FILE" \
    -WaitMutex

if [ $? -ne 0 ]; then
    echo "ERROR: Editor build failed!"
    exit 1
fi

echo ""
echo "🔨 Building SpiderLily Game target..."
"$UE_ROOT/Engine/Build/BatchFiles/Mac/Build.sh" \
    SpiderLily \
    Mac \
    Development \
    -Project="$PROJECT_FILE" \
    -WaitMutex

if [ $? -ne 0 ]; then
    echo "ERROR: Game build failed!"
    exit 1
fi

# The compiled binary is already in Binaries/Mac from the Build.sh step
# We just need to verify the .app bundle exists
echo ""
echo "📦 Verifying .app bundle..."

# Check if we need to create the bundle structure
if [ ! -d "$APP_PATH" ]; then
    echo "Creating .app bundle structure..."
    mkdir -p "$APP_PATH/Contents/MacOS"
    mkdir -p "$APP_PATH/Contents/Resources"
    
    # Copy Info.plist template
    cp "$PROJECT_ROOT/Build/Mac/Resources/Info.Template.plist" "$APP_PATH/Contents/Info.plist"
    
    # Create PkgInfo
    echo "APPL????" > "$APP_PATH/Contents/PkgInfo"
    
    # Copy icon if it exists
    if [ -f "$UE_ROOT/Engine/Build/Mac/Resources/AppIcon.icns" ]; then
        cp "$UE_ROOT/Engine/Build/Mac/Resources/AppIcon.icns" "$APP_PATH/Contents/Resources/"
    fi
fi

# Copy the compiled binary if it exists in the staged builds
if [ -f "$PROJECT_ROOT/Binaries/Mac/SpiderLily" ]; then
    echo "Copying binary to .app bundle..."
    cp "$PROJECT_ROOT/Binaries/Mac/SpiderLily" "$APP_PATH/Contents/MacOS/"
    chmod +x "$APP_PATH/Contents/MacOS/SpiderLily"
fi

# Copy any dylibs
if [ -d "$PROJECT_ROOT/Binaries/Mac" ]; then
    echo "Copying dynamic libraries..."
    find "$PROJECT_ROOT/Binaries/Mac" -name "*.dylib" -exec cp {} "$APP_PATH/Contents/MacOS/" \;
fi

# Copy staged content if it exists
if [ -d "$PROJECT_ROOT/Saved/StagedBuilds/Mac" ]; then
    echo "Syncing staged content..."
    rsync -a "$PROJECT_ROOT/Saved/StagedBuilds/Mac/" "$APP_PATH/Contents/UE/"
fi

# Check if the binary was created
if [ ! -f "$APP_PATH/Contents/MacOS/SpiderLily" ]; then
    echo "ERROR: App binary not found at $APP_PATH/Contents/MacOS/SpiderLily"
    exit 1
fi

# Ad-hoc sign the app bundle
echo ""
echo "✍️  Signing app bundle..."
codesign -s - --deep --force "$APP_PATH"
if [ $? -ne 0 ]; then
    echo "ERROR: Codesigning failed!"
    exit 1
fi

# Remove quarantine attributes
echo "🔓 Removing quarantine attributes..."
xattr -cr "$APP_PATH"

# Verify app is ready
echo ""
echo "✅ Build complete!"
echo "App: $APP_PATH"
APP_SIZE=$(du -sh "$APP_PATH" | cut -f1)
echo "Size: $APP_SIZE"

# Launch the app
echo ""
echo "🚀 Launching SpiderLily..."
open "$APP_PATH"

# Wait a moment and check if it's running
sleep 3
if ps aux | grep -v grep | grep "SpiderLily.app" > /dev/null; then
    echo ""
    echo "========================================="
    echo "✅ SpiderLily is running!"
    echo "========================================="
else
    echo ""
    echo "⚠️  App may not have started. Check logs:"
    echo "   Console.app for crash logs"
    echo "   Or run manually: open $APP_PATH"
fi
