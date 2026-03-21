#!/bin/bash
# Automated Mac build + upload pipeline for SpiderLily
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VERSION=${1:-$(date +%Y.%m.%d.%H.%M)}

echo "========================================="
echo "SpiderLily Mac Build Pipeline"
echo "========================================="
echo "Version: $VERSION"
echo ""

# Step 1: Build the Mac app
echo "🔨 Step 1/3: Building Mac app..."
"$SCRIPT_DIR/package-spiderlily-mac.sh"

if [ $? -ne 0 ]; then
    echo "❌ Build failed!"
    exit 1
fi

# Step 2: Find the built app
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
BUILD_PATH="$PROJECT_ROOT/Packaged/Mac/SpiderLily.app"

if [ ! -d "$BUILD_PATH" ]; then
    echo "❌ Build output not found at: $BUILD_PATH"
    exit 1
fi

echo ""
echo "✅ Build complete!"
echo "  Path: $BUILD_PATH"
echo ""

# Step 3: Upload to Spaces
echo "☁️  Step 2/3: Uploading to DigitalOcean Spaces..."
"$SCRIPT_DIR/../upload-to-spaces.sh" mac "$BUILD_PATH" "$VERSION"

if [ $? -ne 0 ]; then
    echo "❌ Upload failed!"
    exit 1
fi

# Step 4: Update builds.false.work index
echo ""
echo "📝 Step 3/3: Updating builds.false.work..."
"$SCRIPT_DIR/../update-builds-index.sh" mac "$VERSION"

echo ""
echo "🎉 Pipeline complete!"
echo ""
echo "🌐 View builds at: https://builds.false.work"
