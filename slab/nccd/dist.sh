#!/usr/bin/env bash
# dist.sh — the NCCd release: a universal app, signed with the Developer ID
# and the hardened runtime, notarized and stapled, in a signed and notarized
# drag-to-Applications DMG that opens on a Mac that has never seen it.
#
#   ./dist.sh                  → dist/NCCd-<version>.dmg   the first install
#                                dist/NCCd-<version>.zip   what a running copy updates from
#                                dist/latest.json               the update manifest
#   ./release.sh               publish all three (see that file)
#   SKIP_NOTARIZE=1 ./dist.sh  sign only (the result will NOT open elsewhere)
#
# The version is CFBundleShortVersionString in Resources/Info.plist.
# Notary credentials come from the vault (apple/app-specific-password.env:
# APPLE_ID and APP_SPECIFIC_PASSWORD), or from the environment.
set -euo pipefail
cd "$(dirname "$0")"

VERSION="$(/usr/libexec/PlistBuddy -c 'Print :CFBundleShortVersionString' Resources/Info.plist)"
APP="build/NCCd.app"
DMG="dist/NCCd-${VERSION}.dmg"
IDENTITY="$(security find-identity -v -p codesigning 2>/dev/null |
    sed -n 's/.*"\(Developer ID Application: [^"]*\)".*/\1/p' | head -1)"
[[ -n "$IDENTITY" ]] || { echo "✗ no Developer ID Application identity in the keychain" >&2; exit 1; }

DONUT_ARCHS="arm64 x86_64" DONUT_HARDENED=1 DEV_ID="$IDENTITY" ./build.sh
codesign --verify --deep --strict "$APP"
# Buffer, do not pipe into grep -q: under pipefail grep's early exit can
# SIGPIPE codesign and fail the pipeline on a match.
SIG="$(codesign -dv --verbose=2 "$APP" 2>&1)"
grep -q "flags=.*runtime" <<<"$SIG" || { echo "✗ hardened runtime flag missing" >&2; exit 1; }
lipo -archs "$APP/Contents/MacOS/NCCd"

notarize() {    # $1 = a zip or a dmg
    xcrun notarytool submit "$1" --apple-id "$APPLE_ID" --team-id "$APPLE_TEAM_ID" \
        --password "$APPLE_APP_PASSWORD" --wait
}

if [[ "${SKIP_NOTARIZE:-}" != "1" ]]; then
    VAULT_ENV="../../aesthetic-computer-vault/apple/app-specific-password.env"
    [[ -f "$VAULT_ENV" ]] || VAULT_ENV="$HOME/aesthetic-computer-vault/apple/app-specific-password.env"
    # shellcheck disable=SC1090
    if [[ -f "$VAULT_ENV" ]]; then set -a; source "$VAULT_ENV"; set +a; fi
    : "${APPLE_ID:?APPLE_ID is required (vault apple/app-specific-password.env)}"
    : "${APPLE_APP_PASSWORD:=${APP_SPECIFIC_PASSWORD:-}}"
    : "${APPLE_APP_PASSWORD:?APPLE_APP_PASSWORD or APP_SPECIFIC_PASSWORD is required}"
    : "${APPLE_TEAM_ID:=$(sed -nE 's/.*\(([A-Z0-9]+)\)$/\1/p' <<<"$IDENTITY")}"
    echo "› notarizing the app (a minute or two)"
    ditto -c -k --keepParent "$APP" build/NCCd-notary.zip
    notarize build/NCCd-notary.zip
    rm -f build/NCCd-notary.zip
    xcrun stapler staple "$APP"
else
    echo "⚠ SKIP_NOTARIZE: the result will NOT open on another Mac"
fi

echo "› building the DMG"
ROOT="build/dmg"
rm -rf "$ROOT" "$DMG"
mkdir -p "$ROOT" dist
cp -R "$APP" "$ROOT/"
ln -s /Applications "$ROOT/Applications"
hdiutil create -fs HFS+ -volname "NCCd ${VERSION}" -srcfolder "$ROOT" -ov -format UDZO "$DMG" >/dev/null
rm -rf "$ROOT"
codesign --force --timestamp --sign "$IDENTITY" "$DMG"

if [[ "${SKIP_NOTARIZE:-}" != "1" ]]; then
    echo "› notarizing the DMG"
    notarize "$DMG"
    xcrun stapler staple "$DMG"
    xcrun stapler validate "$DMG"
    echo "› Gatekeeper, as a stranger's Mac would ask it"
    spctl -a -vv -t open --context context:primary-signature "$DMG"
    spctl -a -vv -t exec "$APP"
fi

# The update: the stapled app, zipped with ditto so its signature survives,
# and the manifest a running copy reads (Sources/Updater.swift).
ZIP="dist/NCCd-${VERSION}.zip"
rm -f "$ZIP"
ditto -c -k --keepParent "$APP" "$ZIP"
cat > dist/latest.json <<JSON
{
  "version": "${VERSION}",
  "url": "https://assets.aesthetic.computer/nccd/NCCd-${VERSION}.zip",
  "sha256": "$(shasum -a 256 "$ZIP" | awk '{print $1}')",
  "dmg": "https://assets.aesthetic.computer/nccd/NCCd-${VERSION}.dmg",
  "dmgSha256": "$(shasum -a 256 "$DMG" | awk '{print $1}')",
  "dmgBytes": $(stat -f %z "$DMG")
}
JSON

echo
echo "✓ $ZIP + dist/latest.json"
echo "✓ $DMG  ($(du -h "$DMG" | awk '{print $1}'))"
echo "  sha256 $(shasum -a 256 "$DMG" | awk '{print $1}')"
