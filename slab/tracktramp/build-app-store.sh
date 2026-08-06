#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODE="${1:---verify}"
DERIVED="${SCRIPT_DIR}/build/appstore-derived"
ARCHIVE="${SCRIPT_DIR}/build/TrackDrum.xcarchive"
EXPORT="${SCRIPT_DIR}/build/appstore-export"

case "${MODE}" in
    --verify|--archive) ;;
    *) echo "usage: ./build-app-store.sh [--verify|--archive]" >&2; exit 2 ;;
esac

cd "${SCRIPT_DIR}"
xcodegen generate
"${SCRIPT_DIR}/test.sh"

if [[ "${MODE}" == "--verify" ]]; then
    xcodebuild \
        -project TrackDrum.xcodeproj \
        -scheme TrackDrum \
        -configuration Release \
        -destination 'generic/platform=macOS' \
        -derivedDataPath "${DERIVED}" \
        ONLY_ACTIVE_ARCH=NO \
        "ARCHS=arm64 x86_64" \
        CODE_SIGNING_ALLOWED=NO \
        build
    APP="${DERIVED}/Build/Products/Release/TrackDrum.app"
    codesign --force --entitlements "${SCRIPT_DIR}/TrackDrum.entitlements" \
        --sign - "${APP}"
    "${SCRIPT_DIR}/verify-app.sh" "${APP}"
    echo "✓ App Store target verified: ${APP}"
    exit 0
fi

xcodebuild \
    -project TrackDrum.xcodeproj \
    -scheme TrackDrum \
    -configuration Release \
    -destination 'generic/platform=macOS' \
    -archivePath "${ARCHIVE}" \
    ONLY_ACTIVE_ARCH=NO \
    "ARCHS=arm64 x86_64" \
    -allowProvisioningUpdates \
    archive

APP="${ARCHIVE}/Products/Applications/TrackDrum.app"
"${SCRIPT_DIR}/verify-app.sh" "${APP}"

xcodebuild \
    -exportArchive \
    -archivePath "${ARCHIVE}" \
    -exportPath "${EXPORT}" \
    -exportOptionsPlist "${SCRIPT_DIR}/ExportOptions-AppStore.plist" \
    -allowProvisioningUpdates

PKG="${EXPORT}/TrackDrum.pkg"
pkgutil --check-signature "${PKG}" | grep -q \
    '3rd Party Mac Developer Installer'

echo "✓ App Store archive: ${ARCHIVE}"
echo "✓ App Store package: ${PKG}"
