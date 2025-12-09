#!/usr/bin/env bash
# Launch Chromium with extension loaded for testing
# Usage: ./test-extension.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DIST_DIR="$SCRIPT_DIR/dist"
PROFILE_DIR="$SCRIPT_DIR/.chrome-test-profile"

if [ ! -d "$DIST_DIR" ]; then
  echo "❌ dist/ not found. Run 'npm run build' first."
  exit 1
fi

echo "🚀 Launching Chromium with Keeps Wallet extension..."
echo "   Extension: $DIST_DIR"
echo "   Profile: $PROFILE_DIR"
echo ""
echo "📋 In Chromium:"
echo "   1. Go to chrome://extensions"
echo "   2. Enable 'Developer mode'"
echo "   3. Click 'Load unpacked' → select dist/"
echo "   4. Or the extension may auto-load from the --load-extension flag"
echo ""

# Create profile dir if needed
mkdir -p "$PROFILE_DIR"

# Launch Chromium with:
# - Separate test profile (doesn't affect your main browser)
# - Extension auto-loaded
# - Allowed to run in devcontainer
chromium-browser \
  --user-data-dir="$PROFILE_DIR" \
  --load-extension="$DIST_DIR" \
  --no-first-run \
  --no-default-browser-check \
  --disable-gpu \
  --disable-software-rasterizer \
  "https://localhost:8888" \
  2>/dev/null &

echo "✅ Chromium launched! Check the browser window."
echo "   (If no window appears, you may need X11 forwarding or a display)"
