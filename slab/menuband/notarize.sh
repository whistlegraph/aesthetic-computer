#!/usr/bin/env bash
# notarize.sh — submit Menu Band.app to Apple's notary service and staple
# the ticket to the bundle so Gatekeeper accepts it offline.
#
# Prerequisites:
#   1. The .app must already be code-signed with a Developer ID Application
#      certificate AND with hardened runtime + the entitlements file. The
#      install.sh now does this automatically when SIGN_IDENTITY is set or
#      when a Developer ID is found in the keychain.
#   2. Apple ID + Team ID + an app-specific password.
#      The vault has the app-specific password at:
#        ~/aesthetic-computer/aesthetic-computer-vault/apple/app-specific-password.env.gpg
#      Decrypt it once with `devault.fish` (or by hand) and source the
#      resulting variable into your shell env, OR pass via the env vars
#      below. Required env:
#        APPLE_ID         — your Apple Developer email (e.g. me@jas.life)
#        APPLE_TEAM_ID    — 10-char team identifier from developer.apple.com
#        APPLE_APP_PASSWORD — app-specific password
#   3. Xcode CLT installed (provides notarytool + stapler).
#
# Usage:
#   ./notarize.sh                  # uses ~/Applications/Menu Band.app
#   ./notarize.sh path/to/MyApp.app

set -euo pipefail

CYAN=$'\033[1;36m'
GREEN=$'\033[1;32m'
RED=$'\033[1;31m'
RESET=$'\033[0m'
say() { printf "%s• %s%s\n" "$CYAN" "$1" "$RESET"; }
ok()  { printf "%s✓ %s%s\n" "$GREEN" "$1" "$RESET"; }
err() { printf "%s✗ %s%s\n" "$RED" "$1" "$RESET"; }

APP="${1:-${HOME}/Applications/Menu Band.app}"
if [[ ! -d "${APP}" ]]; then
    err "app bundle not found at ${APP}"
    exit 1
fi

: "${APPLE_ID:?APPLE_ID env var is required}"

# Vault uses APP_SPECIFIC_PASSWORD (Apple's own terminology); accept either.
: "${APPLE_APP_PASSWORD:=${APP_SPECIFIC_PASSWORD:-}}"
: "${APPLE_APP_PASSWORD:?APPLE_APP_PASSWORD (or APP_SPECIFIC_PASSWORD) env var is required}"

# Auto-discover team ID from the Developer ID cert in the keychain so the
# vault file doesn't have to carry it. Override by exporting APPLE_TEAM_ID.
if [[ -z "${APPLE_TEAM_ID:-}" ]]; then
    APPLE_TEAM_ID="$(security find-identity -v -p codesigning 2>/dev/null \
        | sed -nE 's/.*Developer ID Application: [^(]+\(([A-Z0-9]+)\).*/\1/p' \
        | head -1)"
fi
: "${APPLE_TEAM_ID:?APPLE_TEAM_ID env var is required (or install a Developer ID cert)}"

WORK="$(mktemp -d)"
trap "rm -rf ${WORK}" EXIT
ZIP="${WORK}/MenuBand.zip"

say "verifying signature + hardened runtime"
# Capture once instead of `codesign | grep -q`: under `set -o pipefail`,
# grep -q's early exit SIGPIPEs codesign, which propagates as a failed
# pipeline even though the pattern matched. Buffering sidesteps the race.
SIG_OUTPUT="$(codesign -dv --verbose=2 "${APP}" 2>&1)"
if ! echo "${SIG_OUTPUT}" | grep -q "flags=.*runtime"; then
    err "bundle is not signed with hardened runtime — re-run install.sh with a Developer ID first"
    exit 1
fi
echo "${SIG_OUTPUT}" | head -10

say "zipping bundle for upload"
ditto -c -k --keepParent "${APP}" "${ZIP}"
ok "zipped: $(du -h "${ZIP}" | awk '{print $1}')"

say "submitting to notary service (this can take a minute or two)"
xcrun notarytool submit "${ZIP}" \
    --apple-id "${APPLE_ID}" \
    --team-id "${APPLE_TEAM_ID}" \
    --password "${APPLE_APP_PASSWORD}" \
    --wait

ok "notarization accepted"

say "stapling ticket to bundle"
xcrun stapler staple "${APP}"
xcrun stapler validate "${APP}"
ok "stapled — bundle now passes Gatekeeper offline"

echo
echo "Distribute by zipping or DMG-ing the bundle. Try: ./dmg.sh"
