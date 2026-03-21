#!/usr/bin/env bash
# Build FFOS ISO on Mac host via SSH
set -euo pipefail

VAULT_JSON="/workspaces/aesthetic-computer/aesthetic-computer-vault/machines.json"
MAC_USER=$(jq -r '.machines."jeffrey-macbook".user' "$VAULT_JSON")
MAC_IP=$(jq -r '.machines."jeffrey-macbook".ip' "$VAULT_JSON")
MAC_HOSTNAME=$(jq -r '.machines."jeffrey-macbook".hostname' "$VAULT_JSON")
MAC_REPO=$(jq -r '.machines."jeffrey-macbook".repoPath' "$VAULT_JSON")

echo "📡 Connecting to Mac: $MAC_USER@$MAC_HOSTNAME ($MAC_IP)"
echo "📂 Repo path: $MAC_REPO"
echo ""

# Try hostname first, fallback to IP
MAC_HOST="$MAC_HOSTNAME"
if ! ssh -o ConnectTimeout=3 -o BatchMode=yes "$MAC_USER@$MAC_HOST" "echo connected" 2>/dev/null; then
  echo "⚠️  Hostname unreachable, trying IP..."
  MAC_HOST="$MAC_IP"
fi

echo "🔄 Syncing latest changes to Mac..."
# Rsync the utilities/ffos-build directory to Mac
rsync -avz --delete \
  /workspaces/aesthetic-computer/utilities/ffos-build/ \
  "$MAC_USER@$MAC_HOST:$MAC_REPO/utilities/ffos-build/"

echo ""
echo "🏗️  Starting build on Mac..."
echo "════════════════════════════════════════════════════════"
echo ""

# SSH to Mac and run the build
ssh -t "$MAC_USER@$MAC_HOST" "cd $MAC_REPO && sudo bash utilities/ffos-build/build.sh"

BUILD_EXIT=$?

if [ $BUILD_EXIT -eq 0 ]; then
  echo ""
  echo "════════════════════════════════════════════════════════"
  echo "✅ Build completed successfully on Mac!"
  echo ""
  echo "📥 Copying ISO back to devcontainer..."

  # Copy the ISO back
  rsync -avz --progress \
    "$MAC_USER@$MAC_HOST:$MAC_REPO/utilities/ffos-build/.ffos-cache/out/" \
    /workspaces/aesthetic-computer/utilities/ffos-build/.ffos-cache/out/

  echo ""
  echo "✅ ISO available locally at:"
  ls -lh /workspaces/aesthetic-computer/utilities/ffos-build/.ffos-cache/out/*.iso 2>/dev/null || echo "  (no ISO found)"
  echo ""
  echo "🧪 Test on Mac with:"
  echo "   bash utilities/ffos-build/test-iso-mac.sh"
else
  echo ""
  echo "❌ Build failed with exit code $BUILD_EXIT"
  exit $BUILD_EXIT
fi
