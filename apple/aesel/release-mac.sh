#!/usr/bin/env bash
# Build, sign, notarize and publish Aesel for Mac: the app and the terminal in
# one DMG. `aesel` opens the app; `aes` runs the TUI the app carries.
#
#   ./release-mac.sh            → dist/aesel-<version>-arm64.dmg, notarized + stapled
#   ./release-mac.sh publish    → upload that DMG, then pack the TUI tarball
#
# Version = aesel/package.json (the TUI and app share it); build = commit count.
# Notarization reads APPLE_ID / APPLE_APP_SPECIFIC_PASSWORD / APPLE_TEAM_ID;
# publishing reads SPACES_KEY / SPACES_SECRET. Neither is stored here.
set -euo pipefail
cd "$(dirname "$0")"
HERE=$(pwd)
REPO=$(cd ../.. && pwd)
VERSION=$(node -p "require('$REPO/aesel/package.json').version")
BUILD=$(git -C "$REPO" rev-list --count HEAD)
IDENTITY="Developer ID Application: Jeffrey Scudder (FB5948YR3S)"
NODE_VERSION=${NODE_VERSION:-v24.18.1}
DIST="$HERE/dist"
DMG="$DIST/aesel-$VERSION-arm64.dmg"
DERIVED=${DERIVED:-/tmp/aesel-release-dd}
CACHE="$HOME/Library/Caches/aesel-release"

publish() {
    [[ -f "$DMG" ]] || { echo "no $DMG — build first" >&2; exit 1; }
    xcrun stapler validate "$DMG"
    spctl -a -t open --context context:primary-signature "$DMG"
    local built_version built_number
    built_version=$(/usr/libexec/PlistBuddy -c Print:CFBundleShortVersionString "$DIST/Aesel.app/Contents/Info.plist")
    built_number=$(/usr/libexec/PlistBuddy -c Print:CFBundleVersion "$DIST/Aesel.app/Contents/Info.plist")
    [[ "$built_version" == "$VERSION" ]] || { echo "build version differs from source; rebuild first" >&2; exit 1; }
    local sha; sha=$(shasum -a 256 "$DMG" | cut -d' ' -f1)
    printf '{\n  "version": "%s",\n  "build": %s,\n  "dmg": "aesel-%s-arm64.dmg",\n  "sha256": "%s"\n}\n' \
        "$built_version" "$built_number" "$built_version" "$sha" > "$DIST/latest.json"
    export AWS_ACCESS_KEY_ID="$SPACES_KEY" AWS_SECRET_ACCESS_KEY="$SPACES_SECRET"
    local s3=(aws s3 cp --endpoint-url "${SPACES_ENDPOINT:-https://sfo3.digitaloceanspaces.com}" --acl public-read)
    "${s3[@]}" "$DMG" "s3://releases-aesthetic-computer/aesel/mac/$(basename "$DMG")" \
        --content-type application/x-apple-diskimage --cache-control "public, max-age=31536000, immutable"
    # The feed goes up last, so it never names a DMG that is not there yet.
    "${s3[@]}" "$DIST/latest.json" "s3://releases-aesthetic-computer/aesel/mac/latest.json" \
        --content-type application/json --cache-control no-cache
    # The same version for terminal-only installs (easel.sh, Linux).
    node "$REPO/aesel/bin/pack.mjs"
    echo "published https://releases.aesthetic.computer/aesel/mac/$(basename "$DMG")"
}
[[ "${1:-}" == publish ]] && { publish; exit 0; }

: "${APPLE_ID:?}" "${APPLE_APP_SPECIFIC_PASSWORD:?}" "${APPLE_TEAM_ID:?}"
notarize() {
    xcrun notarytool submit "$1" --apple-id "$APPLE_ID" --password "$APPLE_APP_SPECIFIC_PASSWORD" \
        --team-id "$APPLE_TEAM_ID" --wait --output-format json | tee "$DIST/notary.json"
    grep -q '"status" *: *"Accepted"' "$DIST/notary.json" || { echo "notarization not accepted" >&2; exit 1; }
}

# A release must include the fleet work already preserved upstream.
if git -C "$REPO" rev-parse --verify origin/main >/dev/null 2>&1; then
    git -C "$REPO" merge-base --is-ancestor origin/main HEAD || {
        echo "checkout is behind or diverged from origin/main; integrate fleet work before releasing" >&2
        exit 1
    }
fi
echo "aesel $VERSION ($BUILD)"
./bundle-session.sh
xcodegen generate >/dev/null
xcodebuild -project Aesel.xcodeproj -scheme AeselMac -configuration Release \
    -destination 'generic/platform=macOS' -derivedDataPath "$DERIVED" -jobs 2 \
    PRODUCT_NAME=Aesel PRODUCT_BUNDLE_IDENTIFIER=computer.aesthetic.aesel.native CODE_SIGN_ENTITLEMENTS=MacDirect.entitlements \
    MARKETING_VERSION="$VERSION" CURRENT_PROJECT_VERSION="$BUILD" \
    CODE_SIGN_STYLE=Manual CODE_SIGN_IDENTITY="$IDENTITY" OTHER_CODE_SIGN_FLAGS=--timestamp \
    build
BUILT="$DERIVED/Build/Products/Release/Aesel.app"
[[ -d "$BUILT" ]] || { echo "build failed" >&2; exit 1; }

rm -rf "$DIST"; mkdir -p "$DIST"
APP="$DIST/Aesel.app"
ditto "$BUILT" "$APP"

# The terminal half: the TUI's source tree, without install.json, so its
# self-updater leaves the bundle alone — the app's release updates it.
AESEL="$APP/Contents/Resources/aesel"
mkdir -p "$AESEL"
for entry in bin src shell context media layouts native shared package.json README.md LICENSE; do
    [[ -e "$REPO/aesel/$entry" ]] && rsync -a --exclude '.update-check.json' "$REPO/aesel/$entry" "$AESEL/"
done
rm -f "$AESEL/install.json"

# Node, from nodejs.org, checked against its published SHA-256.
mkdir -p "$CACHE"
TARBALL="node-$NODE_VERSION-darwin-arm64.tar.gz"
[[ -f "$CACHE/$TARBALL" ]] || curl -fsSL -o "$CACHE/$TARBALL" "https://nodejs.org/dist/$NODE_VERSION/$TARBALL"
curl -fsSL "https://nodejs.org/dist/$NODE_VERSION/SHASUMS256.txt" | grep " $TARBALL\$" > "$CACHE/$TARBALL.sha256"
(cd "$CACHE" && shasum -a 256 -c "$TARBALL.sha256" >/dev/null) || { echo "node checksum mismatch" >&2; exit 1; }
mkdir -p "$APP/Contents/Helpers"
tar -xzf "$CACHE/$TARBALL" -C "$APP/Contents/Helpers" --strip-components 2 "node-$NODE_VERSION-darwin-arm64/bin/node"

# Inside out: the TUI's own binaries (frame-ocr), Node, then the app, whose
# seal now covers Resources/aesel.
find "$AESEL" -type f -perm -u+x -print0 | while IFS= read -r -d '' file; do
    if file -b "$file" | grep -q Mach-O; then
        codesign --force --options runtime --timestamp -s "$IDENTITY" "$file"
    fi
done
codesign --force --options runtime --timestamp --entitlements Node.entitlements -s "$IDENTITY" "$APP/Contents/Helpers/node"
codesign --force --options runtime --timestamp --entitlements MacDirect.entitlements -s "$IDENTITY" "$APP"
codesign --verify --deep --strict "$APP"
"$APP/Contents/Helpers/node" --version >/dev/null

# Staple the app too, so Gatekeeper passes offline after it leaves the DMG.
ditto -c -k --keepParent "$APP" "$DIST/Aesel.zip"
notarize "$DIST/Aesel.zip"
xcrun stapler staple "$APP"
rm "$DIST/Aesel.zip"

STAGE="$DIST/stage"; mkdir -p "$STAGE"
ditto "$APP" "$STAGE/Aesel.app"
ln -s /Applications "$STAGE/Applications"
hdiutil create -volname Aesel -srcfolder "$STAGE" -format UDZO -ov "$DMG" >/dev/null
rm -rf "$STAGE"
codesign --force --timestamp -s "$IDENTITY" "$DMG"
notarize "$DMG"
xcrun stapler staple "$DMG"
xcrun stapler validate "$DMG"
spctl -a -t open --context context:primary-signature "$DMG"
echo "$DMG"
