#!/bin/bash
# Remove SpiderLily from iOS device

source /workspaces/aesthetic-computer/aesthetic-computer-vault/false.work/mac-builder-credentials.env

DEVICE_ID="46720BDD-8807-53C8-AA2E-6977BCA606D3"
BUNDLE_ID="work.false.SpiderLily"

echo "🗑️  Removing SpiderLily from aesthetic.computer iPhone..."
echo ""

sshpass -p "$MAC_PASSWORD" ssh -o StrictHostKeyChecking=no falsework@host.docker.internal \
  "xcrun devicectl device uninstall app --device $DEVICE_ID $BUNDLE_ID"

if [ $? -eq 0 ]; then
  echo ""
  echo "✅ SpiderLily removed successfully!"
else
  echo ""
  echo "⚠️  App may not have been installed or already removed"
fi
