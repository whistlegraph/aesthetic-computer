#!/usr/bin/env bash
# build.sh — compile NCCd into a double-clickable .app bundle.
#
#   ./build.sh            build slab/nccd/build/NCCd.app (this Mac's architecture)
#   ./build.sh --run      build, then launch it
#   ./dist.sh             the release: universal, notarized, in a DMG
#
# Single swiftc build (no SwiftPM) — the house style of the slab Swift apps.
# Signs with a Developer ID identity when the keychain has one, so the stored
# wiki session survives a rebuild (an ad-hoc signature changes every build, and
# macOS then asks for the keychain password). DEV_ID overrides; "-" = ad-hoc.
#
# DONUT_ARCHS ("arm64 x86_64") makes a universal binary; DONUT_HARDENED=1
# signs with the hardened runtime and a timestamp, which notarization needs.
set -euo pipefail
cd "$(dirname "$0")"

APP="build/NCCd.app"
rm -rf "$APP"
mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"

FRAMEWORKS=(-framework AppKit -framework Network -framework Security -framework CoreImage
            -framework AVFoundation -framework ImageIO -framework SceneKit -framework WebKit -framework UserNotifications)
if [[ -n "${DONUT_ARCHS:-}" ]]; then
    SDK="$(xcrun --sdk macosx --show-sdk-path)"
    SLICES=()
    for arch in $DONUT_ARCHS; do
        echo "› compiling Sources/*.swift ($arch)"
        xcrun swiftc -O -swift-version 5 -suppress-warnings -sdk "$SDK" -target "$arch-apple-macosx13.0" \
            "${FRAMEWORKS[@]}" Sources/*.swift Launch/main.swift -o "build/NCCd-$arch"
        SLICES+=("build/NCCd-$arch")
    done
    lipo -create -output "$APP/Contents/MacOS/NCCd" "${SLICES[@]}"
    rm -f "${SLICES[@]}"
else
    echo "› compiling Sources/*.swift"
    swiftc -O -swift-version 5 -suppress-warnings "${FRAMEWORKS[@]}" Sources/*.swift Launch/main.swift -o "$APP/Contents/MacOS/NCCd"
fi

cp Resources/Info.plist "$APP/Contents/Info.plist"
cp Resources/AppIcon.icns "$APP/Contents/Resources/"
# The Signal Desktop bridge rides in the bundle, so the history import works
# on a Mac whose checkout is elsewhere or older.
cp ../bin/signal.mjs "$APP/Contents/Resources/signal.mjs"
SIGN_ID="${DEV_ID:-$(security find-identity -v -p codesigning 2>/dev/null |
    sed -n 's/.*"\(Developer ID Application: [^"]*\)".*/\1/p' | head -1)}"
if [[ "${DONUT_HARDENED:-}" == "1" ]]; then
    codesign --force --options runtime --timestamp --sign "${SIGN_ID:?a Developer ID identity is required}" "$APP"
else
    codesign --force --sign "${SIGN_ID:--}" "$APP" 2>/dev/null
fi
echo "› signed (${SIGN_ID:-ad-hoc})"
echo "✓ built $APP"
[[ "${1:-}" == "--run" ]] && open "$APP"
exit 0
