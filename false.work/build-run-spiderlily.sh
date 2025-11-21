#!/bin/bash

# SpiderLily Mac Build & Run Script
# Builds and launches SpiderLily in Unreal Editor on Mac
# Run directly on Mac (not in dev container)

set -e

# Mac paths
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
PROJECT_FILE="$PROJECT_ROOT/SpiderLily.uproject"
UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
EDITOR="$UE_ROOT/Engine/Binaries/Mac/UnrealEditor.app/Contents/MacOS/UnrealEditor"

echo "========================================="
echo "SpiderLily - Build & Run"
echo "========================================="
echo ""

# Verify paths
if [ ! -f "$PROJECT_FILE" ]; then
    echo "ERROR: Project not found: $PROJECT_FILE"
    exit 1
fi

if [ ! -f "$EDITOR" ]; then
    echo "ERROR: Unreal Editor not found: $EDITOR"
    exit 1
fi

# Kill any running instances
echo "🛑 Stopping existing instances..."
pkill -9 SpiderLily 2>/dev/null || true
pkill -9 UnrealEditor 2>/dev/null || true
sleep 1

# Sync latest from Perforce
echo ""
echo "📥 Syncing latest from Perforce..."
cd "$PROJECT_ROOT"
export P4PORT=ssl:falsework.helixcore.io:1666
export P4USER=machine
export P4CLIENT=spiderlily_build_workspace_macmini

# Regular sync (only updates files that changed on server)
/opt/homebrew/bin/p4 sync

if [ $? -ne 0 ]; then
    echo "⚠️  P4 sync failed, continuing with existing files..."
fi

# Fix FMOD compilation bug (uninitialized variable)
echo "🔧 Applying FMOD plugin fix..."
sed -i.bak 's/FMOD_STUDIO_PLAYBACK_STATE pS;/FMOD_STUDIO_PLAYBACK_STATE pS = FMOD_STUDIO_PLAYBACK_STOPPED;/' \
    "$PROJECT_ROOT/Plugins/FMODStudio/Source/FMODStudio/Private/FMODBlueprintStatics.cpp"

# Build the project modules
echo ""
echo "🔨 Building SpiderLilyEditor..."
echo ""
"$UE_ROOT/Engine/Build/BatchFiles/Mac/Build.sh" \
    SpiderLilyEditor \
    Mac \
    Development \
    -Project="$PROJECT_FILE" \
    -WaitMutex

if [ $? -ne 0 ]; then
    echo ""
    echo "❌ Editor build failed"
    exit 1
fi

echo ""
echo "🔨 Building SpiderLily Game..."
echo ""
"$UE_ROOT/Engine/Build/BatchFiles/Mac/Build.sh" \
    SpiderLily \
    Mac \
    Development \
    -Project="$PROJECT_FILE" \
    -WaitMutex || true

# Check if binary was created (build might fail at codesigning step but binary is there)
if [ ! -f "$PROJECT_ROOT/Binaries/Mac/SpiderLily" ]; then
    echo ""
    echo "❌ Game binary not found"
    exit 1
fi

echo ""
echo "🍳 Building and Cooking with UAT (BuildCookRun)..."
echo ""

# Use Unreal's automation tool which handles everything properly
"$UE_ROOT/Engine/Build/BatchFiles/RunUAT.sh" BuildCookRun \
    -project="$PROJECT_FILE" \
    -platform=Mac \
    -clientconfig=Development \
    -cook \
    -stage \
    -pak \
    -archive \
    -archivedirectory="$PROJECT_ROOT/Archived" \
    -build \
    -noP4 \
    -utf8output

if [ $? -ne 0 ]; then
    echo "⚠️  BuildCookRun had errors, but continuing..."
fi

# Copy the archived build to our .app location
if [ -d "$PROJECT_ROOT/Archived/MacNoEditor" ]; then
    echo "Copying archived build to standard location..."
    rm -rf "$APP_PATH"
    cp -R "$PROJECT_ROOT/Archived/MacNoEditor/SpiderLily.app" "$APP_PATH"
fi

echo ""
echo "✅ Build complete!"

# Create/update .app bundle
APP_PATH="$PROJECT_ROOT/Binaries/Mac/SpiderLily.app"
echo ""
echo "📦 Packaging .app bundle..."

# Ensure .app structure exists
mkdir -p "$APP_PATH/Contents/MacOS"
mkdir -p "$APP_PATH/Contents/Resources"

# Copy binary
if [ -f "$PROJECT_ROOT/Binaries/Mac/SpiderLily" ]; then
    cp "$PROJECT_ROOT/Binaries/Mac/SpiderLily" "$APP_PATH/Contents/MacOS/"
    chmod +x "$APP_PATH/Contents/MacOS/SpiderLily"
fi

# Copy dylibs
find "$PROJECT_ROOT/Binaries/Mac" -maxdepth 1 -name "*.dylib" -exec cp {} "$APP_PATH/Contents/MacOS/" \; 2>/dev/null || true

# Copy Info.plist
if [ -f "$PROJECT_ROOT/Build/Mac/Resources/Info.Template.plist" ]; then
    cp "$PROJECT_ROOT/Build/Mac/Resources/Info.Template.plist" "$APP_PATH/Contents/Info.plist"
fi

# Create PkgInfo
echo "APPL????" > "$APP_PATH/Contents/PkgInfo"

# Copy icon
if [ -f "$UE_ROOT/Engine/Build/Mac/Resources/AppIcon.icns" ]; then
    cp "$UE_ROOT/Engine/Build/Mac/Resources/AppIcon.icns" "$APP_PATH/Contents/Resources/"
fi

# Copy staged content
if [ -d "$PROJECT_ROOT/Saved/StagedBuilds/Mac" ]; then
    echo "Copying staged content..."
    rsync -a "$PROJECT_ROOT/Saved/StagedBuilds/Mac/" "$APP_PATH/Contents/UE/"
fi

# Copy cooked content  
if [ -d "$PROJECT_ROOT/Saved/Cooked/Mac" ]; then
    echo "Copying cooked content..."
    rsync -av "$PROJECT_ROOT/Saved/Cooked/Mac/" "$APP_PATH/Contents/UE/" | grep -E "(metallib|metalmap|sending)"
fi

# Verify binary exists
if [ ! -f "$APP_PATH/Contents/MacOS/SpiderLily" ]; then
    echo "❌ Binary not found at $APP_PATH/Contents/MacOS/SpiderLily"
    exit 1
fi

# Ad-hoc sign
echo "✍️  Ad-hoc signing..."
codesign -s - --deep --force "$APP_PATH"
xattr -cr "$APP_PATH" 2>/dev/null || true

APP_SIZE=$(du -sh "$APP_PATH" | cut -f1)
echo "✅ App ready: $APP_SIZE"

# Launch the game
echo ""
echo "🚀 Launching SpiderLily..."
"$APP_PATH/Contents/MacOS/SpiderLily" > ~/spiderlily-launch.log 2>&1 &
GAME_PID=$!

# Wait a moment and check if it's running  
sleep 3
if ps -p $GAME_PID > /dev/null 2>&1; then
    echo ""
    echo "========================================="
    echo "✅ SpiderLily is running!"
    echo "========================================="
    echo "PID: $GAME_PID"
    echo "Logs: ~/spiderlily-launch.log"
else
    echo ""
    echo "⚠️  Game may not have started"
    echo "Check logs:"
    echo "  tail ~/spiderlily-launch.log"
    echo "  Console.app"
fi
