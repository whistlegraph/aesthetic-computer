#!/usr/bin/env fish
# Cook and stage iOS build on Mac via SSH
# Usage: ./cook-stage-ios-remote.fish [device|simulator]

set SCRIPT_DIR (dirname (status -f))
set VAULT_DIR /workspaces/aesthetic-computer/aesthetic-computer-vault/false.work

# Load credentials from vault
if not test -f $VAULT_DIR/mac-builder-credentials.env
    echo "❌ ERROR: Credentials file not found at $VAULT_DIR/mac-builder-credentials.env"
    exit 1
end

set MAC_HOST (grep MAC_HOST $VAULT_DIR/mac-builder-credentials.env | cut -d'=' -f2 | head -1)
set MAC_USERNAME (grep MAC_USERNAME $VAULT_DIR/mac-builder-credentials.env | cut -d'=' -f2)
set MAC_PASSWORD (grep MAC_PASSWORD $VAULT_DIR/mac-builder-credentials.env | cut -d'=' -f2)

if test -z "$MAC_HOST" -o -z "$MAC_USERNAME" -o -z "$MAC_PASSWORD"
    echo "❌ ERROR: Failed to load credentials from vault"
    exit 1
end

echo "🔧 Cooking & staging iOS build on Mac..."
echo ""

# Copy the script to Mac
sshpass -p "$MAC_PASSWORD" scp $SCRIPT_DIR/cook-stage-ios.sh $MAC_USERNAME@$MAC_HOST:~/cook-stage-ios.sh
or begin
    echo "❌ Failed to copy script to Mac"
    exit 1
end

# Make it executable and run it
set BUILD_TYPE device
if test (count $argv) -gt 0
    set BUILD_TYPE $argv[1]
end

sshpass -p "$MAC_PASSWORD" ssh $MAC_USERNAME@$MAC_HOST "chmod +x ~/cook-stage-ios.sh && ~/cook-stage-ios.sh $BUILD_TYPE"
