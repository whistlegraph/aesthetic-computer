#!/usr/bin/env bash
# Test FFOS ISO with QEMU in Docker container
set -euo pipefail

ISO_PATH="${1:-utilities/ffos-build/.ffos-cache/out/*.iso}"

# Expand glob
ISO_FILE=$(ls $ISO_PATH 2>/dev/null | head -1)

if [ ! -f "$ISO_FILE" ]; then
  echo "❌ No ISO found at: $ISO_PATH"
  echo "   Build one first: bash utilities/ffos-build/build.sh"
  exit 1
fi

echo "🚀 Testing ISO: $ISO_FILE"
echo ""
echo "This will boot the ISO in QEMU (headless, VNC on :0)"
echo "Connect with a VNC client to: localhost:5900"
echo ""
echo "Press Ctrl+C to stop"
echo ""

# Install QEMU if not present
if ! command -v qemu-system-x86_64 &> /dev/null; then
  echo "📦 Installing QEMU..."
  sudo dnf install -y qemu-system-x86 2>&1 | tail -5
fi

# Run QEMU with VNC server (no GUI needed in container)
# VNC will be available on localhost:5900
qemu-system-x86_64 \
  -cdrom "$ISO_FILE" \
  -m 2048 \
  -smp 2 \
  -boot d \
  -vnc :0 \
  -vga std \
  -net nic -net user \
  -enable-kvm 2>/dev/null || \
  qemu-system-x86_64 \
    -cdrom "$ISO_FILE" \
    -m 2048 \
    -smp 2 \
    -boot d \
    -vnc :0 \
    -vga std \
    -net nic -net user
