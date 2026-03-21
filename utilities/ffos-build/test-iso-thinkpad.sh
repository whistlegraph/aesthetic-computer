#!/usr/bin/env bash
# Copy ISO to ThinkPad and test with QEMU
set -euo pipefail

TARGET="${1:-jas-fedora}"  # or x1-nano-g2
ISO_PATH="utilities/ffos-build/.ffos-cache/out/*.iso"

# Expand glob
ISO_FILE=$(ls $ISO_PATH 2>/dev/null | head -1)

if [ ! -f "$ISO_FILE" ]; then
  echo "❌ No ISO found. Build it first: bash utilities/ffos-build/build.sh"
  exit 1
fi

# Get target machine info from vault
MACHINES_JSON="/workspaces/aesthetic-computer/aesthetic-computer-vault/machines.json"
TARGET_IP=$(jq -r ".machines.\"$TARGET\".ip" "$MACHINES_JSON")
TARGET_USER=$(jq -r ".machines.\"$TARGET\".user" "$MACHINES_JSON")
TARGET_HOST=$(jq -r ".machines.\"$TARGET\".hostname // .machines.\"$TARGET\".ip" "$MACHINES_JSON")

if [ "$TARGET_IP" = "null" ]; then
  echo "❌ Machine '$TARGET' not found in vault/machines.json"
  echo ""
  echo "Available ThinkPads:"
  jq -r '.machines | to_entries[] | select(.value.label | contains("ThinkPad")) | "  - \(.key): \(.value.label)"' "$MACHINES_JSON"
  exit 1
fi

ISO_NAME=$(basename "$ISO_FILE")
ISO_SIZE=$(du -h "$ISO_FILE" | cut -f1)

echo "📦 ISO: $ISO_FILE ($ISO_SIZE)"
echo "🎯 Target: $TARGET ($TARGET_USER@$TARGET_IP)"
echo ""

# Copy ISO to ThinkPad
echo "📤 Copying ISO to ThinkPad..."
scp "$ISO_FILE" "$TARGET_USER@$TARGET_IP:/tmp/$ISO_NAME"

echo ""
echo "✅ ISO copied to /tmp/$ISO_NAME on $TARGET"
echo ""
echo "════════════════════════════════════════════════════"
echo "  To test on $TARGET, SSH in and run:"
echo "════════════════════════════════════════════════════"
echo ""
echo "ssh $TARGET_USER@$TARGET_IP"
echo ""
echo "# Install QEMU if needed:"
echo "sudo dnf install -y qemu-system-x86"
echo ""
echo "# Boot the ISO:"
echo "qemu-system-x86_64 \\"
echo "  -cdrom /tmp/$ISO_NAME \\"
echo "  -m 2048 \\"
echo "  -smp 2 \\"
echo "  -boot d \\"
echo "  -enable-kvm"
echo ""
echo "════════════════════════════════════════════════════"
echo ""
echo "Or run this to SSH and auto-launch:"
echo "ssh -t $TARGET_USER@$TARGET_IP \"qemu-system-x86_64 -cdrom /tmp/$ISO_NAME -m 2048 -smp 2 -boot d -enable-kvm\""
