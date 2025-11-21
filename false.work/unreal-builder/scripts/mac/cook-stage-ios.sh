#!/bin/bash
# Cook and Stage SpiderLily for iOS (without building - for use with Xcode builds)
set -e

UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
UAT="$UE_ROOT/Engine/Build/BatchFiles/RunUAT.sh"
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
PROJECT_FILE="$PROJECT_ROOT/SpiderLily.uproject"

# Default to device, can override with: ./cook-stage-ios.sh simulator
BUILD_TYPE="${1:-device}"

echo "========================================="
echo "Cooking & Staging SpiderLily for iOS ($BUILD_TYPE)"
echo "========================================="
echo ""
echo "Project: $PROJECT_FILE"
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

echo "✓ Files verified, starting cook & stage..."
echo ""

# Determine platform flags based on build type
if [ "$BUILD_TYPE" = "simulator" ]; then
    echo "🔧 Cooking for iOS Simulator..."
    PLATFORM_FLAGS="-platform=IOS -cookflavor=Multi -device=IOS_Simulator"
    EXPECTED_STAGED_PATH="$PROJECT_ROOT/Saved/StagedBuilds/IOS_Simulator"
else
    echo "🔧 Cooking for iOS Device..."
    PLATFORM_FLAGS="-platform=IOS -device=IOS"
    EXPECTED_STAGED_PATH="$PROJECT_ROOT/Saved/StagedBuilds/IOS"
fi

# Run Unreal Automation Tool to cook only (no build, no stage, no archive)
echo "🚀 Running Cook command..."
"$UAT" BuildCookRun \
    -project="$PROJECT_FILE" \
    $PLATFORM_FLAGS \
    -clientconfig=Development \
    -cook \
    -allmaps \
    -skipstage \
    -noP4 \
    -utf8output

echo ""
echo "✅ Cook complete!"
echo ""

# Determine cooked content directory
if [ "$BUILD_TYPE" = "simulator" ]; then
    COOKED_DIR="$PROJECT_ROOT/Saved/Cooked/IOS"
else
    COOKED_DIR="$PROJECT_ROOT/Saved/Cooked/IOS"
fi

echo "📦 Cooked content location:"
echo "   $COOKED_DIR"
echo ""

# Create the directory structure that the app expects
echo "� Creating project directory structure for Xcode builds..."
mkdir -p "$PROJECT_ROOT/Binaries/SpiderLily"
cp "$PROJECT_FILE" "$PROJECT_ROOT/Binaries/SpiderLily/"
echo "  ✓ Copied: SpiderLily.uproject -> Binaries/SpiderLily/"

# Also copy .uproject relative to where the iOS binary will run
mkdir -p "$PROJECT_ROOT/Binaries/IOS"
if [ -d "$PROJECT_ROOT/Binaries/IOS/SpiderLily.app" ]; then
    echo "� iOS app bundle found, setting up .uproject structure..."
    mkdir -p "$PROJECT_ROOT/Binaries/IOS/SpiderLily.app/../../../SpiderLily"
    cp "$PROJECT_FILE" "$PROJECT_ROOT/Binaries/IOS/SpiderLily.app/../../../SpiderLily/"
    echo "  ✓ Copied: SpiderLily.uproject -> Binaries/IOS/SpiderLily.app/../../../SpiderLily/"
fi

echo ""
echo "✅ Ready for Xcode build!"
echo ""
echo "📱 Next steps:"
echo "   1. Open Xcode workspace:"
echo "      open '$PROJECT_ROOT/Intermediate/ProjectFilesIOS/SpiderLily_IOS_SpiderLily.xcworkspace'"
echo "   2. Select SpiderLily target and your device"
echo "   3. Build and Run (⌘+R)"
echo ""
echo "💡 The cooked content is now available for your Xcode build"
