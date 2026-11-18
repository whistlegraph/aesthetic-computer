#!/usr/bin/env bash
# release.sh — publish an NCCd release that ./dist.sh has built.
#
#   1. Raise CFBundleShortVersionString in Resources/Info.plist.
#   2. ./dist.sh             build, sign, notarize
#   3. ./release.sh          upload (--dry-run shows what would go up)
#
# The DMG and the zip go up before the manifest that names them; the other
# order would point every running copy at a file that is not there yet. Each
# running copy reads the manifest within half an hour, checks the download
# (sha256, code signature, Developer ID team) and restarts into the new version.
# The download page (system/public/nccd) reads the same manifest, so its
# button, size and sha256 follow the release with no edit.
set -euo pipefail
cd "$(dirname "$0")"

VERSION="$(/usr/libexec/PlistBuddy -c 'Print :CFBundleShortVersionString' Resources/Info.plist)"
DMG="dist/NCCd-${VERSION}.dmg"
ZIP="dist/NCCd-${VERSION}.zip"
BUCKET="s3://assets-aesthetic-computer/nccd"
ENDPOINT="https://sfo3.digitaloceanspaces.com"
PUBLIC="https://assets.aesthetic.computer/nccd"

for f in "$DMG" "$ZIP" dist/latest.json; do
    [[ -f "$f" ]] || { echo "✗ $f is missing: run ./dist.sh first" >&2; exit 1; }
done
grep -q "\"version\": \"${VERSION}\"" dist/latest.json || { echo "✗ dist/latest.json is for another version" >&2; exit 1; }
xcrun stapler validate "$DMG" >/dev/null || { echo "✗ $DMG is not notarized" >&2; exit 1; }
spctl -a -t open --context context:primary-signature "$DMG" || { echo "✗ Gatekeeper refuses $DMG" >&2; exit 1; }

if [[ "${1:-}" == "--dry-run" ]]; then
    echo "would upload to $BUCKET:"; ls -l "$DMG" "$ZIP" dist/latest.json; cat dist/latest.json; exit 0
fi

aws s3 cp "$DMG" "$BUCKET/" --endpoint-url "$ENDPOINT" --acl public-read
aws s3 cp "$ZIP" "$BUCKET/" --endpoint-url "$ENDPOINT" --acl public-read
aws s3 cp dist/latest.json "$BUCKET/latest.json" --endpoint-url "$ENDPOINT" --acl public-read \
    --cache-control "public, max-age=300" --content-type application/json

echo "✓ NCCd ${VERSION} is published"
echo "  $PUBLIC/latest.json"
