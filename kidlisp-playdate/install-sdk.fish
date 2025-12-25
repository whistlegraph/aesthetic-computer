#!/usr/bin/env fish
# install-sdk.fish - Download and install Playdate SDK for KidLisp development
#
# Usage: ./install-sdk.fish
#
# This script downloads the Playdate SDK if not already present.
# The SDK is gitignored and should be installed on each dev machine.

set SDK_VERSION "2.6.2"
set SDK_DIR (dirname (status filename))/sdk
set SDK_URL "https://download.panic.com/playdate_sdk/Linux/PlaydateSDK-$SDK_VERSION.tar.gz"

echo "🎮 KidLisp Playdate SDK Installer"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Check if SDK already exists
if test -d "$SDK_DIR"
    echo "✅ SDK already installed at $SDK_DIR"
    echo "   Version:" (cat "$SDK_DIR/VERSION" 2>/dev/null || echo "unknown")
    exit 0
end

echo "📥 Downloading Playdate SDK v$SDK_VERSION..."

# Create temp directory
set TMP_DIR (mktemp -d)
set TMP_FILE "$TMP_DIR/playdate-sdk.tar.gz"

# Download SDK
if not curl -L -o "$TMP_FILE" "$SDK_URL"
    echo "❌ Failed to download SDK"
    rm -rf "$TMP_DIR"
    exit 1
end

echo "📦 Extracting SDK..."

# Extract to temp first
if not tar -xzf "$TMP_FILE" -C "$TMP_DIR"
    echo "❌ Failed to extract SDK"
    rm -rf "$TMP_DIR"
    exit 1
end

# Move to final location
set EXTRACTED_DIR (ls -d "$TMP_DIR"/PlaydateSDK-* 2>/dev/null | head -1)
if test -z "$EXTRACTED_DIR"
    echo "❌ Could not find extracted SDK directory"
    rm -rf "$TMP_DIR"
    exit 1
end

mv "$EXTRACTED_DIR" "$SDK_DIR"
rm -rf "$TMP_DIR"

echo "✅ SDK installed to $SDK_DIR"
echo "   Version:" (cat "$SDK_DIR/VERSION" 2>/dev/null || echo "$SDK_VERSION")
echo ""
echo "You can now use: ac-playdate <file.lisp>"
