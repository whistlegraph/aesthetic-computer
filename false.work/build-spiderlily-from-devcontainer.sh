#!/bin/bash

# Wrapper script to build SpiderLily on Mac from dev container
# Loads credentials from vault and executes build remotely

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
VAULT_DIR="$SCRIPT_DIR/../aesthetic-computer-vault"

# Load Mac credentials
if [ -f "$VAULT_DIR/false.work/mac-builder-credentials.env" ]; then
    source "$VAULT_DIR/false.work/mac-builder-credentials.env"
else
    echo "ERROR: Credentials not found at $VAULT_DIR/false.work/mac-builder-credentials.env"
    exit 1
fi

echo "========================================="
echo "SpiderLily Remote Build (Dev Container → Mac)"
echo "========================================="
echo "Connecting to: $MAC_USERNAME@$MAC_HOST"
echo ""

# Check if sshpass is available
if ! command -v sshpass &> /dev/null; then
    echo "Installing sshpass..."
    sudo dnf install -y sshpass
fi

# Copy the build script to Mac
echo "📤 Uploading build script..."
sshpass -p "$MAC_PASSWORD" scp \
    "$SCRIPT_DIR/build-run-spiderlily.sh" \
    "$MAC_USERNAME@$MAC_HOST:~/build-run-spiderlily.sh"

# Make it executable and run it
echo "🚀 Starting build on Mac..."
echo ""
sshpass -p "$MAC_PASSWORD" ssh \
    "$MAC_USERNAME@$MAC_HOST" \
    "chmod +x ~/build-run-spiderlily.sh && ~/build-run-spiderlily.sh"
