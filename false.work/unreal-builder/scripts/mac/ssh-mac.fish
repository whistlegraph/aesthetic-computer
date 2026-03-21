#!/usr/bin/env fish
# Quick SSH to Mac Builder
# Usage: ./ssh-mac.fish [command]
# Note: Password is stored in aesthetic-computer-vault/false.work/mac-builder-credentials.env

set MAC_USER falsework
set MAC_HOST host.docker.internal

# Load credentials from vault
set VAULT_FILE "/workspaces/aesthetic-computer/aesthetic-computer-vault/false.work/mac-builder-credentials.env"
if test -f $VAULT_FILE
    source $VAULT_FILE
    set MAC_PASS $MAC_PASSWORD
else
    echo "❌ Error: Credentials file not found at $VAULT_FILE"
    echo "   Password should be stored there for security."
    exit 1
end

if test (count $argv) -gt 0
    # Run command on Mac
    sshpass -p $MAC_PASS ssh -o StrictHostKeyChecking=no $MAC_USER@$MAC_HOST $argv
else
    # Interactive SSH session
    sshpass -p $MAC_PASS ssh -o StrictHostKeyChecking=no $MAC_USER@$MAC_HOST
end
