#!/usr/bin/env bash
# build.sh — compile MacPal into a double-clickable .app bundle.
#
#   ./build.sh            build macpal/build/MacPal.app
#   ./build.sh --install  build, copy to /Applications, and launch it
#
# Single-file swiftc build (no SwiftPM) — matches the house style of the
# slab Swift apps. Signs ad-hoc by default; pass DEV_ID to use a Developer
# ID identity (required before notarizing / App Store submission).
set -euo pipefail

cd "$(dirname "$0")"

APP_NAME="MacPal"
BUILD_DIR="build"
APP="$BUILD_DIR/$APP_NAME.app"
SIGN_ID="${DEV_ID:--}"   # "-" = ad-hoc signature

echo "› cleaning $APP"
rm -rf "$APP"
mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"

echo "› compiling Sources/MacPal.swift"
swiftc -O \
    -framework AppKit \
    -framework ServiceManagement \
    Sources/MacPal.swift \
    -o "$APP/Contents/MacOS/$APP_NAME"

echo "› assembling bundle"
cp Resources/Info.plist "$APP/Contents/Info.plist"
cp Resources/*.svg "$APP/Contents/Resources/"
[[ -f Resources/AppIcon.icns ]] && cp Resources/AppIcon.icns "$APP/Contents/Resources/"

echo "› signing ($SIGN_ID)"
codesign --force --options runtime --sign "$SIGN_ID" "$APP"

echo "✓ built $APP"

if [[ "${1:-}" == "--install" ]]; then
    DEST="/Applications/$APP_NAME.app"
    echo "› installing to $DEST"
    rm -rf "$DEST"
    cp -R "$APP" "$DEST"
    echo "› launching (first run adds it to Login Items)"
    open "$DEST"
    echo "✓ installed and launched"
fi
