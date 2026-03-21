#!/usr/bin/env bash
# Interactive build on Mac - syncs files and gives you SSH command
set -euo pipefail

VAULT_JSON="/workspaces/aesthetic-computer/aesthetic-computer-vault/machines.json"
MAC_USER=$(jq -r '.machines."jeffrey-macbook".user' "$VAULT_JSON")
MAC_IP=$(jq -r '.machines."jeffrey-macbook".ip' "$VAULT_JSON")
MAC_REPO=$(jq -r '.machines."jeffrey-macbook".repoPath' "$VAULT_JSON")

echo "🔄 Syncing build files to Mac..."
rsync -avz --delete \
  /workspaces/aesthetic-computer/utilities/ffos-build/ \
  "$MAC_USER@$MAC_IP:$MAC_REPO/utilities/ffos-build/"

echo ""
echo "✅ Files synced!"
echo ""
echo "════════════════════════════════════════════════════════"
echo "  Now run this command to build on your Mac:"
echo "════════════════════════════════════════════════════════"
echo ""
echo "ssh -t $MAC_USER@$MAC_IP \"cd $MAC_REPO && sudo bash utilities/ffos-build/build.sh\""
echo ""
echo "Or copy-paste these commands:"
echo ""
echo "# 1. SSH to Mac"
echo "ssh $MAC_USER@$MAC_IP"
echo ""
echo "# 2. Build the ISO"
echo "cd $MAC_REPO"
echo "sudo bash utilities/ffos-build/build.sh"
echo ""
echo "# 3. When done, test with:"
echo "qemu-system-x86_64 -cdrom utilities/ffos-build/.ffos-cache/out/*.iso -m 2048 -boot d"
echo ""
echo "════════════════════════════════════════════════════════"
