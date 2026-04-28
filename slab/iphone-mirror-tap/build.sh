#!/usr/bin/env bash
# build.sh — compile, bundle, sign, notarize, staple → MirrorTap.app
#
# Pulls the Developer ID cert + notarization creds from the vault at
# aesthetic-computer-vault/ac-electron/.env. The .p12 is imported into a
# build-only keychain on first run so we don't pollute the login keychain.
#
# Usage:
#   ./build.sh              # build + sign + notarize + staple
#   ./build.sh --no-notary  # build + sign only (faster; opens with right-click)
#
# Output: MirrorTap.app in this directory.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

VAULT_ENV="$(cd "$SCRIPT_DIR/../../aesthetic-computer-vault/ac-electron" && pwd)/.env"
NOTARIZE=1
[[ "${1:-}" == "--no-notary" ]] && NOTARIZE=0

if [[ ! -f "$VAULT_ENV" ]]; then
  echo "vault env missing at $VAULT_ENV" >&2
  exit 1
fi
# shellcheck disable=SC1090
source "$VAULT_ENV"
: "${CSC_KEY_PASSWORD:?missing CSC_KEY_PASSWORD}"
: "${APPLE_ID:?missing APPLE_ID}"
: "${APPLE_APP_SPECIFIC_PASSWORD:?missing APPLE_APP_SPECIFIC_PASSWORD}"
: "${APPLE_TEAM_ID:?missing APPLE_TEAM_ID}"

P12="$(cd "$(dirname "$VAULT_ENV")" && pwd)/mac-developer-id.p12"
[[ -f "$P12" ]] || { echo "missing $P12" >&2; exit 1; }

APP_NAME="MirrorTap"
APP_BUNDLE="$SCRIPT_DIR/$APP_NAME.app"
BUILD_DIR="$SCRIPT_DIR/build"
KEYCHAIN="$BUILD_DIR/build.keychain"
KEYCHAIN_PASS="mirrortap-build"

rm -rf "$BUILD_DIR" "$APP_BUNDLE"
mkdir -p "$BUILD_DIR" "$APP_BUNDLE/Contents/MacOS" "$APP_BUNDLE/Contents/Resources"

echo "==> compile"
swiftc -O -o "$APP_BUNDLE/Contents/MacOS/$APP_NAME" tap.swift \
  -framework Cocoa -framework ApplicationServices -framework Carbon

cp Info.plist "$APP_BUNDLE/Contents/Info.plist"

echo "==> import cert into ephemeral keychain"
# Capture the original search list before we touch anything so we can
# always restore it. If a stale build keychain is registered, drop it.
ORIG_SEARCH=$(security list-keychains -d user \
  | sed -e 's/"//g' -e 's/^[[:space:]]*//' \
  | grep -v 'iphone-mirror-tap/build/build.keychain' || true)
security delete-keychain "$KEYCHAIN" 2>/dev/null || true

security create-keychain -p "$KEYCHAIN_PASS" "$KEYCHAIN"
security set-keychain-settings -lut 21600 "$KEYCHAIN"
security unlock-keychain -p "$KEYCHAIN_PASS" "$KEYCHAIN"
security import "$P12" -k "$KEYCHAIN" -P "$CSC_KEY_PASSWORD" -A >/dev/null
security set-key-partition-list -S apple-tool:,apple:,codesign: \
  -s -k "$KEYCHAIN_PASS" "$KEYCHAIN" >/dev/null

# The .p12 holds only the leaf — fetch Apple's Developer ID G2 intermediate
# so the chain validates. Cached after the first build.
G2_CA="$BUILD_DIR/DeveloperIDG2CA.cer"
if [[ ! -f "$G2_CA" ]]; then
  curl -fsSL -o "$G2_CA" \
    https://www.apple.com/certificateauthority/DeveloperIDG2CA.cer
fi
security import "$G2_CA" -k "$KEYCHAIN" >/dev/null

# Prepend the build keychain to the search list so codesign can find it.
security list-keychains -d user -s "$KEYCHAIN" $ORIG_SEARCH

IDENTITY=$(security find-identity -p codesigning -v "$KEYCHAIN" \
  | grep -E 'Developer ID Application' | head -1 \
  | sed -E 's/.*"(Developer ID Application: [^"]+)".*/\1/')
[[ -n "$IDENTITY" ]] || { echo "no Developer ID Application identity found" >&2; exit 1; }
echo "    identity: $IDENTITY"

echo "==> codesign (hardened runtime)"
codesign --force --options runtime --timestamp \
  --entitlements MirrorTap.entitlements \
  --sign "$IDENTITY" --keychain "$KEYCHAIN" \
  "$APP_BUNDLE/Contents/MacOS/$APP_NAME"
codesign --force --options runtime --timestamp \
  --entitlements MirrorTap.entitlements \
  --sign "$IDENTITY" --keychain "$KEYCHAIN" \
  "$APP_BUNDLE"

echo "==> verify signature"
codesign --verify --deep --strict --verbose=2 "$APP_BUNDLE"

if [[ "$NOTARIZE" == "1" ]]; then
  ZIP="$BUILD_DIR/$APP_NAME.zip"
  echo "==> zip for notarization"
  /usr/bin/ditto -c -k --keepParent "$APP_BUNDLE" "$ZIP"

  echo "==> submit to notary (this can take a minute or three)"
  xcrun notarytool submit "$ZIP" \
    --apple-id "$APPLE_ID" \
    --team-id "$APPLE_TEAM_ID" \
    --password "$APPLE_APP_SPECIFIC_PASSWORD" \
    --wait

  echo "==> staple"
  xcrun stapler staple "$APP_BUNDLE"
  spctl -a -vv "$APP_BUNDLE" || true
fi

# Restore original keychain search list (don't leave the build keychain pinned).
security list-keychains -d user -s $ORIG_SEARCH
security delete-keychain "$KEYCHAIN" 2>/dev/null || true

echo
echo "✓ built $APP_BUNDLE"
if [[ "$NOTARIZE" == "1" ]]; then
  echo "  (signed + notarized — Gatekeeper will allow on any Mac)"
else
  echo "  (signed only — first launch needs right-click → Open)"
fi
