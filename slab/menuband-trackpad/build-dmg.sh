#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
NOTARIZE=false
APP_ONLY=false
for argument in "$@"; do
    case "${argument}" in
        --notarize) NOTARIZE=true ;;
        --app-only) APP_ONLY=true ;;
        *) echo "usage: ./build-dmg.sh [--app-only] [--notarize]" >&2; exit 2 ;;
    esac
done

BUILD_DIR="${SCRIPT_DIR}/build"
APP="${BUILD_DIR}/TrackDrum for Menu Band.app"
CONTENTS="${APP}/Contents"
MACOS="${CONTENTS}/MacOS"
RESOURCES="${CONTENTS}/Resources"
MENUBAND_SOURCES="${SCRIPT_DIR}/../menuband/Sources/MenuBand"
SDK="$(xcrun --sdk macosx --show-sdk-path)"
ARM_BIN="${BUILD_DIR}/MenuBandTrackpad-arm64"
X86_BIN="${BUILD_DIR}/MenuBandTrackpad-x86_64"
UNIVERSAL_BIN="${MACOS}/MenuBandTrackpad"
DMG="${BUILD_DIR}/TrackDrum-for-Menu-Band.dmg"

rm -rf "${APP:?}"
mkdir -p "${BUILD_DIR}" "${MACOS}" "${RESOURCES}"

SOURCES=(
    "${SCRIPT_DIR}/Sources/main.swift"
    "${SCRIPT_DIR}/Sources/Updater.swift"
    "${MENUBAND_SOURCES}/MultitouchTrackpad.swift"
)

echo "• building arm64 helper"
xcrun swiftc -O -whole-module-optimization -sdk "${SDK}" \
    -target arm64-apple-macosx12.0 "${SOURCES[@]}" -o "${ARM_BIN}"

echo "• building x86_64 helper"
xcrun swiftc -O -whole-module-optimization -sdk "${SDK}" \
    -target x86_64-apple-macosx12.0 "${SOURCES[@]}" -o "${X86_BIN}"

lipo -create -output "${UNIVERSAL_BIN}" "${ARM_BIN}" "${X86_BIN}"
cp "${SCRIPT_DIR}/Info.plist" "${CONTENTS}/Info.plist"

echo "• drawing TrackDrum icon"
"${SCRIPT_DIR}/../tracktramp/render-icon.sh" \
    "${RESOURCES}/TrackDrumIcon.icns" \
    "${BUILD_DIR}/icon-work"

SIGN_IDENTITY="$(security find-identity -v -p codesigning 2>/dev/null \
    | awk -F\" '/Developer ID Application/{print $2; exit}')"
if [[ -n "${SIGN_IDENTITY}" ]]; then
    echo "• signing helper with Developer ID"
    codesign --force --options runtime --timestamp \
        --sign "${SIGN_IDENTITY}" "${APP}"
else
    echo "• signing helper ad hoc"
    codesign --force --options runtime --sign - "${APP}"
fi

"${SCRIPT_DIR}/verify-app.sh" "${APP}"

if [[ "${NOTARIZE}" == true ]]; then
    APPLE_NOTARY_PASSWORD="${APPLE_APP_PASSWORD:-${APPLE_APP_SPECIFIC_PASSWORD:-${APP_SPECIFIC_PASSWORD:-}}}"
    : "${APPLE_ID:?APPLE_ID is required for --notarize}"
    : "${APPLE_NOTARY_PASSWORD:?an Apple app-specific password is required for --notarize}"
    if [[ -z "${APPLE_TEAM_ID:-}" ]]; then
        APPLE_TEAM_ID="$(security find-identity -v -p codesigning 2>/dev/null \
            | sed -nE 's/.*Developer ID Application: [^(]+\(([A-Z0-9]+)\).*/\1/p' \
            | head -1)"
    fi
    : "${APPLE_TEAM_ID:?APPLE_TEAM_ID is required for --notarize}"
    [[ -n "${SIGN_IDENTITY}" ]] || {
        echo "Developer ID signing identity is required for --notarize" >&2
        exit 1
    }

    APP_ZIP="${BUILD_DIR}/MenuBandTrackpad-notary.zip"
    ditto -c -k --keepParent "${APP}" "${APP_ZIP}"
    xcrun notarytool submit "${APP_ZIP}" \
        --apple-id "${APPLE_ID}" --team-id "${APPLE_TEAM_ID}" \
        --password "${APPLE_NOTARY_PASSWORD}" --wait
    xcrun stapler staple "${APP}"
    xcrun stapler validate "${APP}"
fi

if [[ "${APP_ONLY}" == true ]]; then
    echo "✓ ${APP}"
    exit 0
fi

STAGE="$(mktemp -d /tmp/menuband-trackpad-dmg.XXXXXX)"
cleanup_stage() { rm -rf "${STAGE:?}"; }
trap cleanup_stage EXIT
cp -R "${APP}" "${STAGE}/TrackDrum for Menu Band.app"
ln -s /Applications "${STAGE}/Applications"

[[ -f "${DMG}" ]] && rm "${DMG}"
hdiutil create -fs HFS+ -volname "TrackDrum for Menu Band" \
    -srcfolder "${STAGE}" -ov -format UDZO "${DMG}" >/dev/null

if [[ -n "${SIGN_IDENTITY}" ]]; then
    codesign --force --timestamp --sign "${SIGN_IDENTITY}" "${DMG}"
fi

if [[ "${NOTARIZE}" == true ]]; then
    xcrun notarytool submit "${DMG}" \
        --apple-id "${APPLE_ID}" --team-id "${APPLE_TEAM_ID}" \
        --password "${APPLE_NOTARY_PASSWORD}" --wait
    xcrun stapler staple "${DMG}"
    xcrun stapler validate "${DMG}"
    spctl --assess --type install --verbose=2 "${DMG}"
fi

echo "✓ ${DMG}"
echo "  $(du -h "${DMG}" | awk '{print $1}') · $(lipo -archs "${UNIVERSAL_BIN}")"
