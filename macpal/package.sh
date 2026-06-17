#!/usr/bin/env bash
# package.sh — build a signed, notarized, stapled MacPal.app ready to hand to
# someone (AirDrop, email, download) and open with no Gatekeeper block.
#
#   ./package.sh            sign + notarize + staple → dist/ + copy to ~/Desktop
#   SKIP_NOTARIZE=1 ./...   sign only (faster; recipient must right-click→Open)
#
# Notary credentials come from the vault app-specific-password env (APPLE_ID,
# APP_SPECIFIC_PASSWORD). Team ID is the Developer ID team (not a secret).
set -euo pipefail
cd "$(dirname "$0")"

APP_NAME="MacPal"
APP="dist/$APP_NAME.app"
SIGN_ID="Developer ID Application: Jeffrey Scudder (FB5948YR3S)"
TEAM_ID="FB5948YR3S"
BUNDLE_ID="computer.aesthetic.macpal"
NOTARY_ENV="../aesthetic-computer-vault/apple/app-specific-password.env"

command -v rsvg-convert >/dev/null && [[ ! -f Resources/AppIcon.icns ]] && ./make-icon.sh

echo "› clean build of $APP"
rm -rf "$APP"; mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"
swiftc -O -framework AppKit -framework ServiceManagement \
    Sources/*.swift -o "$APP/Contents/MacOS/$APP_NAME"
cp Resources/Info.plist "$APP/Contents/Info.plist"
# Only the star's art ships; fuser machine glyphs in Resources/ are installer
# source (macpal/fuser), loaded from disk on the fleet, not from the bundle.
cp Resources/star-*.svg "$APP/Contents/Resources/"
cp Resources/AppIcon.icns "$APP/Contents/Resources/"

echo "› signing with Developer ID (hardened runtime)"
codesign --force --options runtime --timestamp \
    --sign "$SIGN_ID" "$APP"
codesign --verify --strict --verbose=2 "$APP"

if [[ "${SKIP_NOTARIZE:-}" == "1" ]]; then
    echo "⚠ SKIP_NOTARIZE — signed only, not notarized"
else
    echo "› notarizing (this submits to Apple and waits — ~1-3 min)"
    # shellcheck disable=SC1090
    source "$NOTARY_ENV"
    ZIP="dist/$APP_NAME-notarize.zip"
    ditto -c -k --keepParent "$APP" "$ZIP"
    xcrun notarytool submit "$ZIP" \
        --apple-id "$APPLE_ID" \
        --password "$APP_SPECIFIC_PASSWORD" \
        --team-id "$TEAM_ID" \
        --wait
    rm -f "$ZIP"
    echo "› stapling ticket"
    xcrun stapler staple "$APP"
fi

echo "› verifying Gatekeeper acceptance"
spctl -a -vvv -t exec "$APP" || true

# Final ready-to-AirDrop zip + a loose copy on the Desktop.
OUT="dist/$APP_NAME.zip"
ditto -c -k --keepParent "$APP" "$OUT"
DESK="$HOME/Desktop/$APP_NAME.app"
rm -rf "$DESK"; cp -R "$APP" "$DESK"

echo
echo "✓ done"
echo "  • $APP        (signed bundle)"
echo "  • $OUT        (zipped, for email/download)"
echo "  • $DESK  (loose .app on your Desktop — AirDrop this)"
