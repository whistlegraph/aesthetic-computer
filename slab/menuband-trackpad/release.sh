#!/usr/bin/env bash
# release.sh — publish a TrackDrum helper release that installed copies can
# find on their own.
#
# The updater in Sources/Updater.swift polls
#   https://assets.aesthetic.computer/menuband/trackdrum-latest.json
# and installs any newer version whose zip matches the manifest's sha256. This
# script produces that pair — a NOTARIZED zip and the manifest naming it — and
# uploads both beside the DMG that /advanced still links for first installs.
#
# ORDER MATTERS: the zip goes up before the manifest. A manifest naming a zip
# that is not there yet sends every installed helper into a failed download.
#
#   ./release.sh              # build, notarize, upload zip + dmg + manifest
#   ./release.sh --dry-run    # build + stage locally, upload nothing
#
# Credentials come from the vault the same way publish-release.fish takes them:
#   set -a; . ~/aesthetic-computer/aesthetic-computer-vault/apple/app-specific-password.env; set +a

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="${SCRIPT_DIR}/build"
APP="${BUILD_DIR}/TrackDrum for Menu Band.app"
DRY_RUN=false
[[ "${1:-}" == "--dry-run" ]] && DRY_RUN=true

BUCKET="s3://assets-aesthetic-computer/menuband"
ENDPOINT="https://sfo3.digitaloceanspaces.com"
PUBLIC="https://assets.aesthetic.computer/menuband"

echo "• building + notarizing"
if [[ "${DRY_RUN}" == true ]]; then
    "${SCRIPT_DIR}/build-dmg.sh"
else
    "${SCRIPT_DIR}/build-dmg.sh" --notarize
fi

VERSION="$(/usr/libexec/PlistBuddy -c "Print CFBundleShortVersionString" \
    "${APP}/Contents/Info.plist")"
ZIP="${BUILD_DIR}/TrackDrum-for-Menu-Band-${VERSION}.zip"
DMG_SRC="${BUILD_DIR}/TrackDrum-for-Menu-Band.dmg"
DMG="${BUILD_DIR}/TrackDrum-for-Menu-Band-${VERSION}.dmg"
MANIFEST="${BUILD_DIR}/trackdrum-latest.json"

# The updater unpacks with `ditto -xk`, so pack with `ditto -ck --keepParent`:
# it is the only zip that round-trips a signed bundle byte-for-byte.
echo "• packing ${ZIP##*/}"
rm -f "${ZIP}"
ditto -ck --keepParent --rsrc --sequesterRsrc "${APP}" "${ZIP}"
[[ -f "${DMG_SRC}" ]] && cp "${DMG_SRC}" "${DMG}"

SHA="$(shasum -a 256 "${ZIP}" | cut -d' ' -f1)"
SIZE="$(stat -f%z "${ZIP}")"
cat > "${MANIFEST}" <<JSON
{
  "version": "${VERSION}",
  "url": "${PUBLIC}/${ZIP##*/}",
  "sha256": "${SHA}",
  "size": ${SIZE},
  "releasedAt": "$(date -u +%Y-%m-%d)",
  "requirements": "macOS 12+ · Universal (Intel + Apple Silicon)"
}
JSON

echo "• ${VERSION} · ${SIZE} bytes · sha256 ${SHA:0:12}…"

if [[ "${DRY_RUN}" == true ]]; then
    echo "✓ dry run — staged in ${BUILD_DIR}, nothing uploaded"
    exit 0
fi

# Refuse to publish a zip the updater would reject on arrival.
echo "• verifying the packed bundle is notarized + stapled"
WORK="$(mktemp -d)"
ditto -xk "${ZIP}" "${WORK}"
spctl -a -vv -t install "${WORK}/TrackDrum for Menu Band.app"
xcrun stapler validate "${WORK}/TrackDrum for Menu Band.app"
rm -rf "${WORK}"

echo "• uploading zip + dmg (before the manifest names them)"
aws s3 cp "${ZIP}" "${BUCKET}/" --endpoint-url "${ENDPOINT}" --acl public-read
[[ -f "${DMG}" ]] && aws s3 cp "${DMG}" "${BUCKET}/" --endpoint-url "${ENDPOINT}" --acl public-read

echo "• verifying the zip is reachable"
curl -fsI "${PUBLIC}/${ZIP##*/}" > /dev/null

echo "• uploading manifest (installed helpers pick this up within the hour)"
aws s3 cp "${MANIFEST}" "${BUCKET}/trackdrum-latest.json" \
    --endpoint-url "${ENDPOINT}" --acl public-read \
    --cache-control "public, max-age=300"

echo
echo "✓ TrackDrum ${VERSION} published"
echo "  manifest: ${PUBLIC}/trackdrum-latest.json"
echo "  zip:      ${PUBLIC}/${ZIP##*/}"
echo
echo "Next: point /advanced's first-install download at ${DMG##*/}"
echo "      (system/public/menuband/advanced.html), then: fish lith/deploy.fish"
