#!/usr/bin/env bash
# install.sh — build and install the Swift slab menubar, replacing the Python one.
#
# Steps:
#   1. swift build -c release
#   2. Unload the existing python menubar (if loaded)
#   3. Copy the compiled binary to ~/.local/bin/slab-menubar
#   4. Install the launchd plist pointing at the new binary
#   5. launchctl load
#
# Idempotent. Safe to re-run after edits.

set -euo pipefail

BOLD=$'\033[1m'
CYAN=$'\033[1;36m'
GREEN=$'\033[1;32m'
YELLOW=$'\033[1;33m'
RESET=$'\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_HOME="${HOME}"
LAUNCH_AGENTS="${REPO_HOME}/Library/LaunchAgents"
PLIST_PATH="${LAUNCH_AGENTS}/computer.slab.menubar.plist"
PLIST_TMPL="${SCRIPT_DIR}/computer.slab.menubar.plist.tmpl"
INFO_PLIST="${SCRIPT_DIR}/Info.plist"
APPS_DIR="${REPO_HOME}/Applications"
APP_DIR="${APPS_DIR}/SlabMenubar.app"
APP_BIN_DIR="${APP_DIR}/Contents/MacOS"
APP_BIN="${APP_BIN_DIR}/slab-menubar"
LEGACY_BIN="${REPO_HOME}/.local/bin/slab-menubar"

say() { printf "%s• %s%s\n" "$CYAN" "$1" "$RESET"; }
ok()  { printf "%s✓ %s%s\n" "$GREEN" "$1" "$RESET"; }
warn(){ printf "%s! %s%s\n" "$YELLOW" "$1" "$RESET"; }

command -v swift >/dev/null 2>&1 || {
    echo "swift not found — install Xcode Command Line Tools first:"
    echo "    xcode-select --install"
    exit 1
}

SIGN_CN="Slab Menubar Self-Signed"
SIGN_KEYCHAIN="${HOME}/Library/Keychains/login.keychain-db"

ensure_signing_identity() {
    # find-certificate works for self-signed certs even if untrusted, unlike
    # `find-identity -p codesigning` which filters by trust.
    if security find-certificate -c "${SIGN_CN}" "${SIGN_KEYCHAIN}" >/dev/null 2>&1; then
        return 0
    fi
    say "creating self-signed code signing identity '${SIGN_CN}' (one-time)"
    local tmpdir
    tmpdir="$(mktemp -d)"
    cat > "${tmpdir}/openssl.cnf" <<EOF
[req]
distinguished_name = dn
prompt = no
[dn]
CN = ${SIGN_CN}
[v3_ext]
keyUsage = critical,digitalSignature
extendedKeyUsage = critical,codeSigning
basicConstraints = critical,CA:FALSE
EOF
    if ! openssl req -x509 -newkey rsa:2048 -nodes -days 36500 \
        -keyout "${tmpdir}/key.pem" \
        -out "${tmpdir}/cert.pem" \
        -config "${tmpdir}/openssl.cnf" \
        -extensions v3_ext >/dev/null 2>&1; then
        warn "openssl req failed; will fall back to ad-hoc signing"
        rm -rf "${tmpdir}"
        return 1
    fi
    # Apple's Security framework PKCS12 importer needs SHA1-MAC + 3DES PBE,
    # AND a non-empty password (empty pass triggers MAC verification quirks
    # with OpenSSL 3 even with the legacy MAC algorithm).
    local p12pass="slab-import-$$"
    if ! openssl pkcs12 -export \
        -macalg sha1 -keypbe PBE-SHA1-3DES -certpbe PBE-SHA1-3DES \
        -out "${tmpdir}/bundle.p12" \
        -inkey "${tmpdir}/key.pem" \
        -in "${tmpdir}/cert.pem" \
        -name "${SIGN_CN}" \
        -passout "pass:${p12pass}" >/dev/null 2>&1; then
        warn "pkcs12 export failed; will fall back to ad-hoc signing"
        rm -rf "${tmpdir}"
        return 1
    fi
    if ! security import "${tmpdir}/bundle.p12" \
        -k "${SIGN_KEYCHAIN}" \
        -P "${p12pass}" \
        -T /usr/bin/codesign \
        -T /usr/bin/security >/dev/null 2>&1; then
        warn "keychain import failed; will fall back to ad-hoc signing"
        rm -rf "${tmpdir}"
        return 1
    fi
    rm -rf "${tmpdir}"
    ok "code signing identity created"
    warn "first signing may prompt for keychain access — click 'Always Allow'"
    return 0
}

# Provision iTerm2 profiles for the per-session tiled topic wallpapers: every
# profile needs tiled-image mode + a Blend so the dim topic image shows behind
# the status color while text stays legible. iTerm2 caches its prefs in memory
# and rewrites the plist on quit, so this is only safe while it's NOT running.
provision_iterm2_profiles() {
    local pl="${HOME}/Library/Preferences/com.googlecode.iterm2.plist"
    [[ -e "${pl}" ]] || { warn "iTerm2 prefs not found — skipping profile provisioning"; return 0; }
    if pgrep -f "iTerm.app/Contents/MacOS" >/dev/null 2>&1; then
        warn "iTerm2 is running — skipping profile provisioning (quit iTerm2 and re-run to set tiled wallpaper Blend)"
        return 0
    fi
    local pb=/usr/libexec/PlistBuddy i=0 n=0
    while ${pb} -c "Print :'New Bookmarks':${i}:'Guid'" "${pl}" >/dev/null 2>&1; do
        ${pb} -c "Set :'New Bookmarks':${i}:'Blend' 0.5" "${pl}" 2>/dev/null \
            || ${pb} -c "Add :'New Bookmarks':${i}:'Blend' real 0.5" "${pl}" 2>/dev/null || true
        ${pb} -c "Set :'New Bookmarks':${i}:'Background Image Mode' 1" "${pl}" 2>/dev/null \
            || ${pb} -c "Add :'New Bookmarks':${i}:'Background Image Mode' integer 1" "${pl}" 2>/dev/null || true
        ${pb} -c "Set :'New Bookmarks':${i}:'Background Image Is Tiled' true" "${pl}" 2>/dev/null \
            || ${pb} -c "Add :'New Bookmarks':${i}:'Background Image Is Tiled' bool true" "${pl}" 2>/dev/null || true
        i=$((i+1)); n=$((n+1))
    done
    ok "provisioned ${n} iTerm2 profile(s): tiled bg image + Blend 0.5"
}

say "building slab-menubar (swift build -c release)"
cd "${SCRIPT_DIR}"
swift build -c release >/dev/null

BUILT="$(swift build -c release --show-bin-path)/slab-menubar-swift"
if [[ ! -x "${BUILT}" ]]; then
    echo "build succeeded but binary not found at ${BUILT}"
    exit 1
fi
ok "built: ${BUILT}"

provision_iterm2_profiles

say "unloading any existing menubar launch agent"
if launchctl list | grep -q computer.slab.menubar; then
    launchctl unload "${PLIST_PATH}" 2>/dev/null || true
    ok "unloaded computer.slab.menubar"
else
    warn "no running menubar to unload — skipping"
fi

say "installing app bundle → ${APP_DIR}"
mkdir -p "${APP_BIN_DIR}"
mkdir -p "${APP_DIR}/Contents/Resources"
cp "${BUILT}" "${APP_BIN}"
chmod +x "${APP_BIN}"
cp "${INFO_PLIST}" "${APP_DIR}/Contents/Info.plist"
if [[ -f "${SCRIPT_DIR}/AppIcon.icns" ]]; then
    cp "${SCRIPT_DIR}/AppIcon.icns" "${APP_DIR}/Contents/Resources/AppIcon.icns"
fi

# Sign the bundle. Prefer a stable self-signed certificate so the TCC
# Accessibility grant survives rebuilds (ad-hoc embeds the cdhash in the
# designated requirement, which changes every build).
SIGN_OK=0
if ensure_signing_identity; then
    if codesign --force --deep --sign "${SIGN_CN}" \
        --identifier computer.slab.menubar \
        "${APP_DIR}" >/dev/null 2>&1; then
        SIGN_OK=1
        ok "signed with '${SIGN_CN}' (stable identity — TCC grant should persist)"
    else
        warn "codesign with '${SIGN_CN}' failed; falling back to ad-hoc"
    fi
fi
if [[ "${SIGN_OK}" -eq 0 ]]; then
    codesign --force --deep --sign - \
        --identifier computer.slab.menubar \
        "${APP_DIR}" >/dev/null 2>&1 || warn "ad-hoc codesign also failed"
fi
ok "app bundle installed"

if [[ -e "${LEGACY_BIN}" ]]; then
    rm -f "${LEGACY_BIN}"
    warn "removed legacy binary at ${LEGACY_BIN} — you may want to clear its old Accessibility entry in System Settings"
fi

say "writing launchd plist → ${PLIST_PATH}"
mkdir -p "${LAUNCH_AGENTS}"
sed "s|@HOME@|${REPO_HOME}|g" "${PLIST_TMPL}" > "${PLIST_PATH}"
ok "plist written"

say "loading launch agent"
launchctl load "${PLIST_PATH}"
sleep 1
if launchctl list | grep -q computer.slab.menubar; then
    ok "computer.slab.menubar is running"
else
    warn "launchctl did not register the agent — check /tmp/slab-menubar.err"
fi

printf "\n%sdone.%s\n" "${BOLD}" "${RESET}"
echo "  bundle:       ${APP_DIR}"
echo "  binary:       ${APP_BIN}"
echo "  plist:        ${PLIST_PATH}"
echo "  stdout:       /tmp/slab-menubar.out"
echo "  stderr:       /tmp/slab-menubar.err"
echo
echo "  rebuild+reload after edits:  $(basename "$0")"
echo "  tail logs:                   tail -f /tmp/slab-menubar.err"
