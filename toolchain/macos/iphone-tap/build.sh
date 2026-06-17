#!/usr/bin/env bash
# Build the iphone-tap CLI. Unsigned, local-use only — it needs Accessibility
# (to read window frames + inject clicks) and Screen Recording (for `shot`)
# permissions granted to whatever terminal runs it.
set -euo pipefail
cd "$(dirname "$0")"

swiftc -O -o iphone-tap main.swift \
  -framework Cocoa \
  -framework ApplicationServices \
  -framework Vision

echo "✓ built ./iphone-tap"
echo
echo "First run will need permissions for your terminal app:"
echo "  • System Settings → Privacy & Security → Accessibility   (tap + frame)"
echo "  • System Settings → Privacy & Security → Screen Recording (shot)"
echo
echo "Smoke test (open iPhone Mirroring first):"
echo "  ./iphone-tap frame"
echo "  ./iphone-tap shot /tmp/phone.png && open /tmp/phone.png"
echo "  ./iphone-tap ocr /tmp/phone.png"
