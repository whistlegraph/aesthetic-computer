#!/usr/bin/env bash
# build.sh — compile ArenaNative (release) and wrap it into a double-clickable
# .app bundle at build/ArenaNative.app. Ad-hoc signed.
#
#   ./build.sh            build build/ArenaNative.app
#   ./build.sh --dock     build, then pin the app to the Dock
#
# SwiftPM builds the binary; we assemble the bundle by hand (the icon lives at
# Resources/AppIcon.icns — regenerate with make-icon.sh).
set -euo pipefail
cd "$(dirname "$0")"

APP_NAME="ArenaNative"
APP="build/$APP_NAME.app"

echo "› swift build -c release"
swift build -c release

echo "› assembling $APP"
rm -rf "$APP"
mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"
cp ".build/release/$APP_NAME" "$APP/Contents/MacOS/$APP_NAME"
[ -f Resources/AppIcon.icns ] && cp Resources/AppIcon.icns "$APP/Contents/Resources/AppIcon.icns"

cat > "$APP/Contents/Info.plist" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key><string>ArenaNative</string>
  <key>CFBundleDisplayName</key><string>arena — native</string>
  <key>CFBundleExecutable</key><string>ArenaNative</string>
  <key>CFBundleIdentifier</key><string>computer.aesthetic.arena-native</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>CFBundleShortVersionString</key><string>0.1</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleIconFile</key><string>AppIcon</string>
  <key>LSMinimumSystemVersion</key><string>12.0</string>
  <key>NSHighResolutionCapable</key><true/>
  <key>NSPrincipalClass</key><string>NSApplication</string>
</dict>
</plist>
PLIST

codesign --force --sign - "$APP" >/dev/null 2>&1 || true
echo "✓ built $APP"

if [[ "${1:-}" == "--dock" ]]; then
  ABS="$(cd "$(dirname "$APP")" && pwd)/$APP_NAME.app"
  echo "› pinning to Dock: $ABS"
  defaults write com.apple.dock persistent-apps -array-add \
    "<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>file://$ABS</string><key>_CFURLStringType</key><integer>15</integer></dict></dict></dict>"
  killall Dock
  echo "✓ pinned"
fi
