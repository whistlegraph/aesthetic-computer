#!/bin/bash
# Deploy SpiderLily to connected iOS device
set -e

APP_PATH="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app"

echo "========================================="
echo "Deploying SpiderLily to iOS Device"
echo "========================================="
echo ""

# Find connected device
DEVICE_ID=$(xcrun devicectl list devices | grep "connected" | grep -o '[0-9A-F-]\{36\}' | head -1)

if [ -z "$DEVICE_ID" ]; then
    echo "❌ No connected iOS device found"
    echo ""
    echo "Available devices:"
    xcrun devicectl list devices
    exit 1
fi

echo "📱 Found device: $DEVICE_ID"
echo "📦 Deploying: $APP_PATH"
echo ""

# Install the app
xcrun devicectl device install app --device "$DEVICE_ID" "$APP_PATH"

echo ""
echo "✅ Deployment complete!"
echo ""
echo "🎮 The app should now be on your device. Launch it from the home screen."
