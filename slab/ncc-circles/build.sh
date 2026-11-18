#!/usr/bin/env bash
# build.sh — compile NCC Circles into a double-clickable .app bundle.
#
#   ./build.sh            build slab/ncc-circles/build/NCC Circles.app
#   ./build.sh --run      build, then launch it
#
# Single swiftc build (no SwiftPM) — the house style of the slab Swift apps.
# Signs with a Developer ID identity when the keychain has one, so the stored
# NCC login survives a rebuild (an ad-hoc signature changes every build, and
# macOS then asks for the keychain password). DEV_ID overrides; "-" = ad-hoc.
set -euo pipefail
cd "$(dirname "$0")"

APP="build/NCC Circles.app"
rm -rf "$APP"
mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"

echo "› compiling Sources/*.swift"
swiftc -O -swift-version 5 \
    -framework AppKit -framework Network -framework Security -framework CoreImage -framework AVFoundation -framework ImageIO \
    Sources/*.swift -o "$APP/Contents/MacOS/NCCCircles"

cp Resources/Info.plist "$APP/Contents/Info.plist"
cp Resources/AppIcon.icns "$APP/Contents/Resources/"
# The Signal Desktop bridge rides in the bundle, so the history import works
# on a Mac whose checkout is elsewhere or older.
cp ../bin/signal.mjs "$APP/Contents/Resources/signal.mjs"
SIGN_ID="${DEV_ID:-$(security find-identity -v -p codesigning 2>/dev/null |
    sed -n 's/.*"\(Developer ID Application: [^"]*\)".*/\1/p' | head -1)}"
codesign --force --sign "${SIGN_ID:--}" "$APP" 2>/dev/null
echo "› signed (${SIGN_ID:-ad-hoc})"
echo "✓ built $APP"
[[ "${1:-}" == "--run" ]] && open "$APP"
exit 0
