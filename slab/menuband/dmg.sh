#!/usr/bin/env bash
# dmg.sh — package the notarized Menu Band.app into a Developer ID-signed
# DMG with a /Applications drag-target alias. Gatekeeper-safe (the .app
# inside is already notarized + stapled by notarize.sh).
#
# Usage:
#   ./dmg.sh                                # uses ~/Applications/Menu\ Band.app
#   ./dmg.sh path/to/Menu\ Band.app         # custom bundle
#
# Output:
#   slab/menuband/Menu-Band-<version>.dmg
#
# Optional follow-up (notarize the DMG itself for cleanest first-run UX):
#   xcrun notarytool submit "<DMG>" --apple-id "$APPLE_ID" \
#     --team-id "$APPLE_TEAM_ID" --password "$APPLE_APP_PASSWORD" --wait
#   xcrun stapler staple "<DMG>"

set -euo pipefail

CYAN=$'\033[1;36m'
GREEN=$'\033[1;32m'
RED=$'\033[1;31m'
RESET=$'\033[0m'
say() { printf "%s• %s%s\n" "$CYAN" "$1" "$RESET"; }
ok()  { printf "%s✓ %s%s\n" "$GREEN" "$1" "$RESET"; }
err() { printf "%s✗ %s%s\n" "$RED" "$1" "$RESET"; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP="${1:-${HOME}/Applications/Menu Band.app}"
if [[ ! -d "${APP}" ]]; then
    err "no .app at ${APP}"
    exit 1
fi

VERSION="$(/usr/libexec/PlistBuddy -c "Print CFBundleShortVersionString" "${APP}/Contents/Info.plist")"
VOLNAME="Menu Band ${VERSION}"
OUT="${SCRIPT_DIR}/Menu-Band-${VERSION}.dmg"

# Refuse to package a .app whose Resources/ is missing the SwiftPM module
# bundle — that's the bug that shipped in Menu-Band-1.0.dmg, where the
# Bundle.module accessor crashed on every fresh machine looking for
# /Users/<dev>/.../.build/.../MenuBand_MenuBand.bundle.
if ! "${SCRIPT_DIR}/bin/verify-bundle.sh" "${APP}"; then
    err "${APP} failed self-containment check — refusing to build DMG"
    err "re-run ./install.sh first"
    exit 1
fi

# Check the inner .app's notarization staple before packaging — distributing
# an unstapled bundle inside a DMG works but gives a network-required first-run
# Gatekeeper check; stapled is smoother.
if ! /usr/bin/xcrun stapler validate "${APP}" >/dev/null 2>&1; then
    err "${APP} is not stapled — run ./notarize.sh first for offline Gatekeeper trust"
    exit 1
fi
ok "inner .app is stapled"

STAGE="$(mktemp -d)"
trap 'rm -rf "${STAGE}"' EXIT
cp -R "${APP}" "${STAGE}/"
# Drag-to-install convenience: /Applications symlink so the user can drop
# the .app onto it from the mounted DMG window.
ln -s /Applications "${STAGE}/Applications"

say "creating DMG → ${OUT}"
[[ -f "${OUT}" ]] && rm "${OUT}"
hdiutil create -fs HFS+ -volname "${VOLNAME}" \
    -srcfolder "${STAGE}" -ov -format UDZO "${OUT}" >/dev/null
ok "DMG created"

# Sign the DMG with the same Developer ID identity that signed the .app.
# Without this step, downloading the DMG triggers a Gatekeeper warning on
# the *DMG* even though the .app inside is fine.
SIGN_HASH="$(security find-identity -v -p codesigning 2>/dev/null \
    | awk '/Developer ID Application/{print $2; exit}')"
if [[ -n "${SIGN_HASH}" ]]; then
    say "signing DMG with Developer ID"
    codesign --force --sign "${SIGN_HASH}" --timestamp "${OUT}" >/dev/null 2>&1
    ok "DMG signed"
else
    err "no Developer ID Application identity found — DMG is unsigned"
    err "users will see a Gatekeeper warning on download"
fi

# Belt-and-braces: also verify the final DMG mounts to a self-contained
# .app. Catches packaging mistakes (wrong staging dir, missing copy step)
# even when the source .app was clean.
if ! "${SCRIPT_DIR}/bin/verify-bundle.sh" --dmg "${OUT}"; then
    err "DMG failed post-build verification — DO NOT upload"
    err "the .app passed but something inside the DMG is broken"
    exit 1
fi

SIZE="$(du -h "${OUT}" | awk '{print $1}')"
echo
ok "Menu-Band-${VERSION}.dmg (${SIZE})"
echo "  path: ${OUT}"
echo
echo "Next steps:"
echo "  1. (optional) notarize + staple the DMG itself:"
echo "       xcrun notarytool submit \"${OUT}\" \\"
echo "         --apple-id \"\${APPLE_ID}\" \\"
echo "         --team-id \"\${APPLE_TEAM_ID}\" \\"
echo "         --password \"\${APPLE_APP_PASSWORD}\" --wait"
echo "       xcrun stapler staple \"${OUT}\""
echo "  2. host the DMG (Spaces / notepat.com / GitHub releases) and link it."
