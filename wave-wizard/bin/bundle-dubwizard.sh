#!/bin/bash
# Package DubWizard as a real .app so macOS will actually hand it microphone input.
#
# Three things here are load-bearing, and getting any of them wrong makes the mic
# go silent with no error and no prompt:
#
#   1. A real bundle in a normal location. TCC will not prompt for a loose binary
#      or for an .app living under /tmp — requestAccess just returns false.
#   2. Developer ID signing, not ad-hoc. An ad-hoc signature is identified by its
#      cdhash, so every rebuild looks like a brand-new app and the existing grant
#      stops applying. Developer ID pins identity to team + bundle id instead, so
#      the grant survives every rebuild.
#   3. The com.apple.security.device.audio-input entitlement. Under the hardened
#      runtime, an app without it is denied outright — silently, no dialog.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONFIG="${CONFIG:-release}"
APP="${APP_DEST:-$HOME/Applications/AesthetiVox.app}"
IDENTITY="${DUBWIZARD_IDENTITY:-Developer ID Application: Jeffrey Scudder (FB5948YR3S)}"
BUNDLE_ID="computer.aesthetic.dubwizard"

cd "$ROOT"
echo "▸ building ($CONFIG)"
swift build -c "$CONFIG" --product DubWizard
BIN="$(swift build -c "$CONFIG" --product DubWizard --show-bin-path)/DubWizard"

echo "▸ assembling $APP"
rm -rf "$APP"
mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"
cp "$BIN" "$APP/Contents/MacOS/DubWizard"
[ -f "$ROOT/.build/DubWizard.app/Contents/Resources/DubWizard.png" ] &&
  cp "$ROOT/.build/DubWizard.app/Contents/Resources/DubWizard.png" "$APP/Contents/Resources/" || true

cat > "$APP/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>CFBundleDisplayName</key><string>AesthetiVox</string>
  <key>CFBundleExecutable</key><string>DubWizard</string>
  <key>CFBundleIconFile</key><string>DubWizard.png</string>
  <key>CFBundleIdentifier</key><string>$BUNDLE_ID</string>
  <key>CFBundleName</key><string>AesthetiVox</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleShortVersionString</key><string>1.0</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>LSMinimumSystemVersion</key><string>13.0</string>
  <key>NSMicrophoneUsageDescription</key><string>DubWizard records live vocal dubs through your audio interface.</string>
</dict></plist>
PLIST

ENT="$(mktemp -t dubwizard-ent).plist"
cat > "$ENT" <<'ENTITLEMENTS'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>com.apple.security.device.audio-input</key><true/>
</dict></plist>
ENTITLEMENTS

echo "▸ signing as $IDENTITY"
codesign --force --options runtime --timestamp=none \
  --entitlements "$ENT" --identifier "$BUNDLE_ID" \
  --sign "$IDENTITY" "$APP"
rm -f "$ENT"

codesign -dv --verbose=2 "$APP" 2>&1 | grep -E "Identifier|TeamIdentifier|Signature="
echo "▸ done: $APP"
echo "  run: open -a '$APP' --args <track.wav> [dubs-dir]"
