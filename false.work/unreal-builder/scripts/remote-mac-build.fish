#!/usr/bin/env fish
# Remote Mac Build Trigger Script
# Runs from dev container, triggers build on Mac, waits for completion

set VERSION (date +%Y.%m.%d.%H.%M)
if test (count $argv) -ge 1
    set VERSION $argv[1]
end

set CONFIG "Development"
if test (count $argv) -ge 2
    set CONFIG $argv[2]
end

echo "========================================="
echo "Remote Mac Build & Upload"
echo "========================================="
echo "Version: $VERSION"
echo "Config: $CONFIG"
echo ""

# Load Mac credentials from vault
set VAULT_DIR /workspaces/aesthetic-computer/aesthetic-computer-vault/false.work
set MAC_CREDS $VAULT_DIR/mac-builder-credentials.env

if not test -f $MAC_CREDS
    echo "❌ Mac credentials not found: $MAC_CREDS"
    exit 1
end

# Parse credentials (bash format, so we need to convert)
set MAC_HOST (grep '^MAC_HOST=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
set MAC_USERNAME (grep '^MAC_USERNAME=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
set MAC_PASSWORD (grep '^MAC_PASSWORD=' $MAC_CREDS | cut -d= -f2 | tr -d '"')

if test -z "$MAC_HOST" -o -z "$MAC_USERNAME" -o -z "$MAC_PASSWORD"
    echo "❌ Failed to load Mac credentials"
    exit 1
end

echo "🔗 Connecting to Mac build machine..."
echo "   Host: $MAC_USERNAME@$MAC_HOST"
echo ""

# Check if sshpass is installed
if not command -v sshpass >/dev/null
    echo "⚠️  sshpass not found, installing..."
    sudo apt-get update -qq && sudo apt-get install -y -qq sshpass
end

# Copy build script to Mac
set LOCAL_SCRIPT /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/mac-build-and-upload.sh
set REMOTE_SCRIPT /tmp/mac-build-and-upload.sh

echo "📤 Uploading build script..."
sshpass -p "$MAC_PASSWORD" scp -o StrictHostKeyChecking=no $LOCAL_SCRIPT "$MAC_USERNAME@$MAC_HOST:$REMOTE_SCRIPT"

if test $status -ne 0
    echo "❌ Failed to upload build script"
    exit 1
end

# Execute build script on Mac
echo ""
echo "🚀 Starting remote build process..."
echo "   This will take several minutes..."
echo ""

sshpass -p "$MAC_PASSWORD" ssh -o StrictHostKeyChecking=no "$MAC_USERNAME@$MAC_HOST" \
    "chmod +x $REMOTE_SCRIPT && $REMOTE_SCRIPT $VERSION $CONFIG"

set BUILD_STATUS $status

if test $BUILD_STATUS -eq 0
    echo ""
    echo "✅ Remote Mac build completed successfully!"
    echo ""
else
    echo ""
    echo "❌ Remote Mac build failed with exit code $BUILD_STATUS"
    echo ""
    exit $BUILD_STATUS
end
