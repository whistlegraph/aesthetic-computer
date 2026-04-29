#!/usr/bin/env bash
# install.sh — build, sign, and install Menu Band.app.
#
# Steps:
#   1. swift build -c release
#   2. Wrap binary in a proper .app bundle at ~/Applications/Menu Band.app
#   3. Sign with stable identity (Apple Developer ID if available, else
#      self-signed; configurable via SIGN_IDENTITY env)
#   4. Install + load LaunchAgent so Menu Band auto-starts at login
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
PLIST_PATH="${LAUNCH_AGENTS}/computer.aestheticcomputer.menuband.plist"
PLIST_TMPL="${SCRIPT_DIR}/computer.aestheticcomputer.menuband.plist.tmpl"
INFO_PLIST="${SCRIPT_DIR}/Info.plist"
APP_DIR="${REPO_HOME}/Applications/Menu Band.app"
APP_BIN_DIR="${APP_DIR}/Contents/MacOS"
APP_BIN="${APP_BIN_DIR}/MenuBand"
APP_RES="${APP_DIR}/Contents/Resources"

say() { printf "%s• %s%s\n" "$CYAN" "$1" "$RESET"; }
ok()  { printf "%s✓ %s%s\n" "$GREEN" "$1" "$RESET"; }
warn(){ printf "%s! %s%s\n" "$YELLOW" "$1" "$RESET"; }

command -v swift >/dev/null 2>&1 || {
    echo "swift not found — install Xcode Command Line Tools first:"
    echo "    xcode-select --install"
    exit 1
}

# Choose signing identity. If SIGN_IDENTITY is set in env, use it. Otherwise
# look for a Developer ID Application or Mac App Distribution cert in the
# keychain. Falls back to a self-signed local identity.
SELF_SIGN_CN="MenuBand Self-Signed"

discover_identity() {
    local kc="${HOME}/Library/Keychains/login.keychain-db"
    if [[ -n "${SIGN_IDENTITY:-}" ]]; then
        echo "${SIGN_IDENTITY}"
        return
    fi
    # Prefer Apple-issued certs.
    for prefix in "Developer ID Application" "Apple Distribution" "3rd Party Mac Developer Application" "Mac Developer"; do
        local found
        found="$(security find-identity -v -p codesigning "${kc}" 2>/dev/null | awk -F\" -v p="${prefix}" '$0 ~ p {print $2; exit}')"
        if [[ -n "${found}" ]]; then
            echo "${found}"
            return
        fi
    done
    # Self-signed fallback.
    if security find-certificate -c "${SELF_SIGN_CN}" "${kc}" >/dev/null 2>&1; then
        echo "${SELF_SIGN_CN}"
        return
    fi
    echo ""
}

ensure_self_signed_identity() {
    local kc="${HOME}/Library/Keychains/login.keychain-db"
    if security find-certificate -c "${SELF_SIGN_CN}" "${kc}" >/dev/null 2>&1; then
        return 0
    fi
    say "creating self-signed code signing identity '${SELF_SIGN_CN}' (one-time)"
    local tmpdir
    tmpdir="$(mktemp -d)"
    cat > "${tmpdir}/openssl.cnf" <<EOF
[req]
distinguished_name = dn
prompt = no
[dn]
CN = ${SELF_SIGN_CN}
[v3_ext]
keyUsage = critical,digitalSignature
extendedKeyUsage = critical,codeSigning
basicConstraints = critical,CA:FALSE
EOF
    openssl req -x509 -newkey rsa:2048 -nodes -days 36500 \
        -keyout "${tmpdir}/key.pem" -out "${tmpdir}/cert.pem" \
        -config "${tmpdir}/openssl.cnf" -extensions v3_ext >/dev/null 2>&1 || {
        rm -rf "${tmpdir}"; warn "openssl req failed"; return 1; }
    local pwd="menuband-import-$$"
    openssl pkcs12 -export -macalg sha1 -keypbe PBE-SHA1-3DES -certpbe PBE-SHA1-3DES \
        -out "${tmpdir}/bundle.p12" -inkey "${tmpdir}/key.pem" -in "${tmpdir}/cert.pem" \
        -name "${SELF_SIGN_CN}" -passout "pass:${pwd}" >/dev/null 2>&1 || {
        rm -rf "${tmpdir}"; warn "pkcs12 export failed"; return 1; }
    security import "${tmpdir}/bundle.p12" -k "${kc}" -P "${pwd}" \
        -T /usr/bin/codesign -T /usr/bin/security >/dev/null 2>&1 || {
        rm -rf "${tmpdir}"; warn "keychain import failed"; return 1; }
    rm -rf "${tmpdir}"
    ok "self-signed identity created"
}

say "building MenuBand (swift build -c release)"
cd "${SCRIPT_DIR}"
swift build -c release >/dev/null

BUILT="$(swift build -c release --show-bin-path)/MenuBand"
if [[ ! -x "${BUILT}" ]]; then
    echo "build succeeded but binary not found at ${BUILT}"
    exit 1
fi
ok "built: ${BUILT}"

say "unloading any existing MenuBand launch agent"
if launchctl list | grep -q computer.aestheticcomputer.menuband; then
    launchctl unload "${PLIST_PATH}" 2>/dev/null || true
    ok "unloaded computer.aestheticcomputer.menuband"
else
    warn "no running MenuBand to unload — skipping"
fi

say "installing app bundle → ${APP_DIR}"
mkdir -p "${APP_BIN_DIR}" "${APP_RES}"
cp "${BUILT}" "${APP_BIN}"
chmod +x "${APP_BIN}"
cp "${INFO_PLIST}" "${APP_DIR}/Contents/Info.plist"
if [[ -f "${SCRIPT_DIR}/AppIcon.icns" ]]; then
    cp "${SCRIPT_DIR}/AppIcon.icns" "${APP_RES}/AppIcon.icns"
fi

# Sign with the best available identity.
SIGN_ID="$(discover_identity)"
if [[ -z "${SIGN_ID}" ]]; then
    ensure_self_signed_identity
    SIGN_ID="${SELF_SIGN_CN}"
fi
# Strip any custom-icon detritus before signing. The IconTinter writes an
# `Icon\r` file with a resource fork to give the bundle an accent-colored
# Finder icon at runtime; that resource fork makes codesign refuse the
# bundle ("resource fork, Finder information, or similar detritus not
# allowed"), silently leaving an ad-hoc signature behind.
rm -f "${APP_DIR}/Icon"$'\r'
xattr -cr "${APP_DIR}" 2>/dev/null || true

say "signing with: ${SIGN_ID}"
# --options runtime + --entitlements + --timestamp are all required for
# Apple's notary service to accept the bundle. Without them notarytool
# rejects with "The signature does not include a secure timestamp" or
# "The executable does not have the hardened runtime enabled."
ENTITLEMENTS="${SCRIPT_DIR}/MenuBand.entitlements"
if ! codesign --force --deep --sign "${SIGN_ID}" \
    --identifier computer.aestheticcomputer.menuband \
    --options runtime \
    --entitlements "${ENTITLEMENTS}" \
    --timestamp \
    "${APP_DIR}" 2>&1; then
    warn "codesign failed — bundle is not signed with hardened runtime"
    exit 1
fi
ok "signed"

say "writing launchd plist → ${PLIST_PATH}"
mkdir -p "${LAUNCH_AGENTS}"
sed "s|@HOME@|${REPO_HOME}|g" "${PLIST_TMPL}" > "${PLIST_PATH}"
ok "plist written"

say "loading launch agent"
launchctl load "${PLIST_PATH}"
sleep 1
if launchctl list | grep -q computer.aestheticcomputer.menuband; then
    ok "computer.aestheticcomputer.menuband is running"
else
    warn "launchctl did not register the agent — check /tmp/menuband.err"
fi

printf "\n%sdone.%s\n" "${BOLD}" "${RESET}"
echo "  bundle:       ${APP_DIR}"
echo "  binary:       ${APP_BIN}"
echo "  plist:        ${PLIST_PATH}"
echo "  signed by:    ${SIGN_ID}"
echo "  stdout:       /tmp/menuband.out"
echo "  stderr:       /tmp/menuband.err"
echo
echo "  rebuild+reload after edits:  ./install.sh"
echo "  override signing identity:   SIGN_IDENTITY=\"...\" ./install.sh"
echo "  tail logs:                   tail -f /tmp/menuband.err"
