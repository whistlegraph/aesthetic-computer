#!/usr/bin/env bash
# release-solo.sh — build the Slab a stranger can install.
#
# `install.sh` builds the Slab that belongs to this studio: symlinks back into
# the checkout, hooks that shell out to `jq`, fleet config, a self-signed
# certificate chosen so Accessibility survives a rebuild. That app is a
# development tool and it cannot leave this desk.
#
# This builds the other one: a Developer ID-signed, notarized, stapled bundle
# inside a DMG with an /Applications drag target, carrying its own hooks (the
# `hook` subcommand in HookCLI.swift) and its own Terminal seed, with the fleet
# half dormant until somebody configures it. It is the same binary; what
# differs is how it is signed and what it can assume about the machine.
#
# The two must not be confused for each other, so this never writes to
# ~/Applications and never touches the running install. Everything lands in
# a staging directory and then in dist/.
#
#   ./release-solo.sh                 # build, sign, notarize, staple, DMG
#   ./release-solo.sh --no-notarize   # local smoke test; produces an UNSHIPPABLE dmg
#
# Credentials come from the vault, same as Menu Band:
#   aesthetic-computer-vault/apple/app-specific-password.env
#     APPLE_ID, APP_SPECIFIC_PASSWORD   (APPLE_APP_PASSWORD also accepted)
# The team ID is read off the Developer ID certificate in the keychain.

set -euo pipefail

BOLD=$'\033[1m'; CYAN=$'\033[1;36m'; GREEN=$'\033[1;32m'
YELLOW=$'\033[1;33m'; RED=$'\033[1;31m'; RESET=$'\033[0m'
say()  { printf "%s• %s%s\n" "$CYAN" "$1" "$RESET"; }
ok()   { printf "%s✓ %s%s\n" "$GREEN" "$1" "$RESET"; }
warn() { printf "%s! %s%s\n" "$YELLOW" "$1" "$RESET"; }
err()  { printf "%s✗ %s%s\n" "$RED" "$1" "$RESET"; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SLAB_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
DIST="${SCRIPT_DIR}/dist"
STAGE="${DIST}/stage"
APP="${STAGE}/Slab.app"
IDENTITY="Developer ID Application: Jeffrey Scudder"
BUNDLE_ID="computer.slab.menubar"

NOTARIZE=1
for arg in "$@"; do
    case "$arg" in
        --no-notarize) NOTARIZE=0 ;;
        *) err "unknown flag: $arg"; exit 2 ;;
    esac
done

# ── 1. Build ──────────────────────────────────────────────────────────────
say "building release binary"
cd "${SCRIPT_DIR}"
swift build -c release >/dev/null
BUILT="$(swift build -c release --show-bin-path)/slab-menubar-swift"
[[ -x "${BUILT}" ]] || { err "no binary at ${BUILT}"; exit 1; }
ok "built $(du -h "${BUILT}" | awk '{print $1}')"

VERSION="$(/usr/libexec/PlistBuddy -c "Print CFBundleShortVersionString" "${SCRIPT_DIR}/Info.plist")"

# ── 2. Assemble the bundle ────────────────────────────────────────────────
# Assembled from scratch every time. An app bundle updated in place keeps
# whatever a previous build left in Resources, and a stale resource that no
# longer exists in source is the kind of thing that only fails on somebody
# else's Mac.
say "assembling Slab.app ${VERSION}"
rm -rf "${STAGE}"
mkdir -p "${APP}/Contents/MacOS" "${APP}/Contents/Resources"
cp "${BUILT}" "${APP}/Contents/MacOS/slab-menubar"
chmod +x "${APP}/Contents/MacOS/slab-menubar"
cp "${SCRIPT_DIR}/Info.plist" "${APP}/Contents/Info.plist"
[[ -f "${SCRIPT_DIR}/AppIcon.icns" ]] && cp "${SCRIPT_DIR}/AppIcon.icns" "${APP}/Contents/Resources/"
if compgen -G "${SCRIPT_DIR}/Resources/*.ttf" >/dev/null; then
    cp "${SCRIPT_DIR}/Resources/"*.ttf "${APP}/Contents/Resources/"
fi

# The solo payload. FirstRun runs `bin/slab-seed-terminal`, which resolves its
# seed as ../seed relative to its own directory — so the repo's shape is
# preserved here rather than flattened.
SOLO="${APP}/Contents/Resources/solo"
mkdir -p "${SOLO}/bin" "${SOLO}/seed"
cp "${SLAB_DIR}/bin/slab-seed-terminal" "${SOLO}/bin/"
chmod +x "${SOLO}/bin/slab-seed-terminal"
cp "${SLAB_DIR}/seed/terminal-profiles.plist" "${SOLO}/seed/"
cp "${SLAB_DIR}/seed/terminal-profiles.list" "${SOLO}/seed/"
cp -R "${SLAB_DIR}/seed/fonts" "${SOLO}/seed/"
ok "bundle assembled ($(du -sh "${APP}" | awk '{print $1}'))"

# ── 3. Self-containment ───────────────────────────────────────────────────
say "verifying self-containment"
"${SCRIPT_DIR}/bin/verify-solo-bundle.sh" "${APP}" || {
    err "bundle is not self-contained — refusing to sign"
    exit 1
}

# ── 4. Sign ───────────────────────────────────────────────────────────────
# Developer ID + hardened runtime, which is what notarization requires. This
# is a DIFFERENT identity from the one install.sh uses, so a Mac that has both
# will be asked to re-grant Accessibility once. That is unavoidable: the
# designated requirement is part of what TCC remembers.
say "signing with Developer ID + hardened runtime"
if ! security find-identity -v -p codesigning | grep -q "${IDENTITY}"; then
    err "no '${IDENTITY}' certificate in the keychain"
    err "this must run on a Mac that holds the Developer ID key (blueberry or neo)"
    exit 1
fi
codesign --force --timestamp --options runtime \
    --entitlements "${SCRIPT_DIR}/SlabMenubar.entitlements" \
    --identifier "${BUNDLE_ID}" \
    --sign "${IDENTITY}" \
    "${APP}"
codesign --verify --deep --strict --verbose=2 "${APP}" 2>&1 | sed 's/^/    /'
# Buffer rather than pipe into grep -q: under pipefail, grep's early exit
# SIGPIPEs codesign and fails the pipeline even on a match.
SIG="$(codesign -dv --verbose=2 "${APP}" 2>&1)"
echo "${SIG}" | grep -q "flags=.*runtime" || { err "hardened runtime flag missing"; exit 1; }
ok "signed"

# ── 5. Notarize + staple the app ──────────────────────────────────────────
if [[ "${NOTARIZE}" -eq 1 ]]; then
    VAULT_ENV="${SLAB_DIR}/../aesthetic-computer-vault/apple/app-specific-password.env"
    if [[ -f "${VAULT_ENV}" ]]; then
        # shellcheck disable=SC1090
        set -a; source "${VAULT_ENV}"; set +a
    fi
    : "${APPLE_ID:?APPLE_ID is required (vault apple/app-specific-password.env)}"
    : "${APPLE_APP_PASSWORD:=${APP_SPECIFIC_PASSWORD:-}}"
    : "${APPLE_APP_PASSWORD:?APPLE_APP_PASSWORD or APP_SPECIFIC_PASSWORD is required}"
    if [[ -z "${APPLE_TEAM_ID:-}" ]]; then
        APPLE_TEAM_ID="$(security find-identity -v -p codesigning \
            | sed -nE 's/.*Developer ID Application: [^(]+\(([A-Z0-9]+)\).*/\1/p' | head -1)"
    fi
    : "${APPLE_TEAM_ID:?APPLE_TEAM_ID could not be determined}"

    say "notarizing the app (a minute or two)"
    ZIP="${STAGE}/Slab.zip"
    ditto -c -k --keepParent "${APP}" "${ZIP}"
    xcrun notarytool submit "${ZIP}" \
        --apple-id "${APPLE_ID}" --team-id "${APPLE_TEAM_ID}" \
        --password "${APPLE_APP_PASSWORD}" --wait
    rm -f "${ZIP}"
    xcrun stapler staple "${APP}"
    xcrun stapler validate "${APP}"
    ok "app notarized + stapled"
else
    warn "skipping notarization — the result will NOT open on another Mac"
fi

# ── 6. DMG ────────────────────────────────────────────────────────────────
say "building DMG"
DMG="${DIST}/Slab-${VERSION}.dmg"
DMG_ROOT="${STAGE}/dmg"
rm -rf "${DMG_ROOT}" "${DMG}"
mkdir -p "${DMG_ROOT}"
cp -R "${APP}" "${DMG_ROOT}/"
ln -s /Applications "${DMG_ROOT}/Applications"
hdiutil create -fs HFS+ -volname "Slab ${VERSION}" \
    -srcfolder "${DMG_ROOT}" -ov -format UDZO "${DMG}" >/dev/null
ok "created $(basename "${DMG}") ($(du -h "${DMG}" | awk '{print $1}'))"

say "signing DMG"
codesign --force --timestamp --sign "${IDENTITY}" "${DMG}"

if [[ "${NOTARIZE}" -eq 1 ]]; then
    say "notarizing the DMG"
    xcrun notarytool submit "${DMG}" \
        --apple-id "${APPLE_ID}" --team-id "${APPLE_TEAM_ID}" \
        --password "${APPLE_APP_PASSWORD}" --wait
    xcrun stapler staple "${DMG}"
    xcrun stapler validate "${DMG}"
    ok "DMG notarized + stapled"
fi

# ── 7. Verify like a stranger's Mac would ─────────────────────────────────
say "verifying Gatekeeper acceptance"
FAILED=0
spctl -a -vv -t open --context context:primary-signature "${DMG}" 2>&1 | sed 's/^/    /' || FAILED=1
spctl -a -vv -t exec "${APP}" 2>&1 | sed 's/^/    /' || FAILED=1
if [[ "${NOTARIZE}" -eq 1 && "${FAILED}" -ne 0 ]]; then
    err "Gatekeeper refused the result"
    exit 1
fi

rm -rf "${DMG_ROOT}"
echo
ok "${BOLD}${DMG}${RESET}"
echo "   Test it the way a stranger would: open the DMG on a Mac that has"
echo "   never run Slab, drag it to Applications, and launch it from there."
