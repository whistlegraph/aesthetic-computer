#!/usr/bin/env bash
# Transfer ISO to Mac and create UTM/QEMU test command
set -euo pipefail

ISO_PATH="${1:-utilities/ffos-build/.ffos-cache/out/*.iso}"
DEST_DIR="${2:-~/Downloads}"

# Expand glob
ISO_FILE=$(ls $ISO_PATH 2>/dev/null | head -1)

if [ ! -f "$ISO_FILE" ]; then
  echo "❌ No ISO found at: $ISO_PATH"
  exit 1
fi

ISO_NAME=$(basename "$ISO_FILE")
ISO_SIZE=$(du -h "$ISO_FILE" | cut -f1)

echo "📦 ISO ready: $ISO_FILE ($ISO_SIZE)"
echo ""
echo "═══════════════════════════════════════════════════════"
echo "  Test on Mac with QEMU (simple, no install needed):"
echo "═══════════════════════════════════════════════════════"
echo ""
echo "# Copy ISO to Mac (run this on your Mac):"
echo "scp $(whoami)@localhost:$(pwd)/$ISO_FILE ~/Downloads/"
echo ""
echo "# Boot with QEMU:"
echo "qemu-system-x86_64 \\"
echo "  -cdrom ~/Downloads/$ISO_NAME \\"
echo "  -m 2048 \\"
echo "  -smp 2 \\"
echo "  -boot d"
echo ""
echo "═══════════════════════════════════════════════════════"
echo "  Or use UTM (graphical, recommended):"
echo "═══════════════════════════════════════════════════════"
echo ""
echo "1. Install: brew install --cask utm"
echo "2. Open UTM → Create VM → Virtualize → Linux"
echo "3. Point to: ~/Downloads/$ISO_NAME"
echo "4. Allocate 2GB RAM"
echo "5. Boot → should see starfield! ✨"
echo ""
echo "ISO is at: $ISO_FILE"
