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
LAUNCHER_PLIST_PATH="${LAUNCH_AGENTS}/computer.aestheticcomputer.menubandlauncher.plist"
LAUNCHER_PLIST_TMPL="${SCRIPT_DIR}/computer.aestheticcomputer.menubandlauncher.plist.tmpl"
INFO_PLIST="${SCRIPT_DIR}/Info.plist"
APP_DIR="${REPO_HOME}/Applications/Menu Band.app"
APP_BIN_DIR="${APP_DIR}/Contents/MacOS"
APP_BIN="${APP_BIN_DIR}/MenuBand"
APP_LAUNCHER_BIN="${APP_BIN_DIR}/MenuBandLauncher"
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

cd "${SCRIPT_DIR}"
# Build a universal binary so Intel Macs can launch the .app.
# Without this, an arm64-only build fails on Intel hardware with the
# cryptic "application can't be opened. -609" dialog (LaunchServices
# can't establish a connection to a binary it can't even load).
#
# We can't use `swift build --arch arm64 --arch x86_64` here because
# the multi-arch path requires a full Xcode toolchain (xcbuild + the
# Metal toolchain). On a CLT-only machine SwiftPM happily cross-
# compiles each slice via --triple, so we build each separately and
# lipo them together — works equally well with full Xcode or CLT.
ARM_TRIPLE="arm64-apple-macosx11.0"
X86_TRIPLE="x86_64-apple-macosx11.0"

say "building MenuBand arm64 slice"
swift build -c release --triple "${ARM_TRIPLE}" >/dev/null
ARM_BIN="$(swift build -c release --triple "${ARM_TRIPLE}" --show-bin-path)/MenuBand"
[[ -x "${ARM_BIN}" ]] || { echo "arm64 build missing at ${ARM_BIN}"; exit 1; }

say "building MenuBand x86_64 slice (Intel Macs)"
swift build -c release --triple "${X86_TRIPLE}" >/dev/null
X86_BIN="$(swift build -c release --triple "${X86_TRIPLE}" --show-bin-path)/MenuBand"
[[ -x "${X86_BIN}" ]] || { echo "x86_64 build missing at ${X86_BIN}"; exit 1; }

say "lipo'ing slices into universal binary"
BUILT="${SCRIPT_DIR}/.build/universal/MenuBand"
mkdir -p "$(dirname "${BUILT}")"
lipo -create -output "${BUILT}" "${ARM_BIN}" "${X86_BIN}"
ARCHS="$(lipo -archs "${BUILT}" 2>/dev/null || echo "")"
if [[ "${ARCHS}" != *"arm64"* ]] || [[ "${ARCHS}" != *"x86_64"* ]]; then
    warn "binary is not universal (got: ${ARCHS}) — Intel Macs will fail with -609"
    exit 1
fi
ok "built universal (${ARCHS}): ${BUILT}"

# MenuBandLauncher — same two-slice + lipo dance for the tiny helper
# that relaunches Menu Band when the double-tap right-⌘ gesture fires
# while the main process isn't running.
say "building MenuBandLauncher arm64 slice"
swift build -c release --target MenuBandLauncher --triple "${ARM_TRIPLE}" >/dev/null
ARM_LAUNCHER="$(swift build -c release --target MenuBandLauncher --triple "${ARM_TRIPLE}" --show-bin-path)/MenuBandLauncher"
[[ -x "${ARM_LAUNCHER}" ]] || { echo "launcher arm64 build missing at ${ARM_LAUNCHER}"; exit 1; }

say "building MenuBandLauncher x86_64 slice (Intel Macs)"
swift build -c release --target MenuBandLauncher --triple "${X86_TRIPLE}" >/dev/null
X86_LAUNCHER="$(swift build -c release --target MenuBandLauncher --triple "${X86_TRIPLE}" --show-bin-path)/MenuBandLauncher"
[[ -x "${X86_LAUNCHER}" ]] || { echo "launcher x86_64 build missing at ${X86_LAUNCHER}"; exit 1; }

say "lipo'ing launcher slices"
BUILT_LAUNCHER="${SCRIPT_DIR}/.build/universal/MenuBandLauncher"
lipo -create -output "${BUILT_LAUNCHER}" "${ARM_LAUNCHER}" "${X86_LAUNCHER}"
ok "built universal launcher: ${BUILT_LAUNCHER}"

say "unloading any existing MenuBand launch agents"
if launchctl list | grep -q computer.aestheticcomputer.menubandlauncher; then
    launchctl unload "${LAUNCHER_PLIST_PATH}" 2>/dev/null || true
    ok "unloaded computer.aestheticcomputer.menubandlauncher"
fi
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
cp "${BUILT_LAUNCHER}" "${APP_LAUNCHER_BIN}"
chmod +x "${APP_LAUNCHER_BIN}"
# Strip DWARF off the launcher too, same rationale as the main binary.
strip -S "${APP_LAUNCHER_BIN}"
# Strip DWARF debug symbols from the shipped binary. Without this the
# release binary embeds every source file's absolute path under
# /Users/<dev>/aesthetic-computer/slab/menuband/Sources/... — harmless
# at runtime but a privacy leak in shipped DMGs and a tripwire for the
# verify-bundle.sh /Users/... scan. `-S` removes debug symbols only and
# leaves regular symbols intact for crash report symbolication.
strip -S "${APP_BIN}"
cp "${INFO_PLIST}" "${APP_DIR}/Contents/Info.plist"
if [[ -f "${SCRIPT_DIR}/AppIcon.icns" ]]; then
    cp "${SCRIPT_DIR}/AppIcon.icns" "${APP_RES}/AppIcon.icns"
fi
# Copy the SwiftPM-generated module resource bundle so files
# accessed via `Bundle.module.url(forResource:)` resolve at
# runtime. SwiftPM emits a `MenuBand_MenuBand.bundle` next to
# the binary; without this the verovio toolkit / sheet harness
# never load (Bundle.module returns nil) and SheetMusicView
# silently shows an empty WKWebView.
# Resources are arch-independent — pull from the arm64 build dir
# (BUILT is the lipo'd universal binary which has no sibling
# resource bundle of its own).
PKG_BUNDLE_NAME="MenuBand_MenuBand.bundle"
PKG_BUNDLE_SRC="$(dirname "${ARM_BIN}")/${PKG_BUNDLE_NAME}"
if [[ -d "${PKG_BUNDLE_SRC}" ]]; then
    # FLATTEN the SwiftPM resource bundle's contents directly into
    # Contents/Resources. The app reads them via Bundle.appResources →
    # Bundle.main (see MASBundleShim.swift), so they live in a codesign-sealable
    # location. We deliberately do NOT keep the nested .bundle: Swift 6.3's
    # generated accessor resolves it at `Bundle.main.bundleURL` = the .app ROOT,
    # and anything at the bundle root fails `codesign --strict` ("unsealed
    # contents present in the bundle root") and breaks notarization.
    rm -rf "${APP_DIR}/${PKG_BUNDLE_NAME}" "${APP_RES}/${PKG_BUNDLE_NAME}"
    cp -R "${PKG_BUNDLE_SRC}/." "${APP_RES}/"
fi

# --- QuickLook preview extension (optional; needs Xcode + xcodegen) ---
# Build ScorePreview.appex and embed it under Contents/PlugIns so ⌘-Space
# renders the animated graphic score. Signed below (inner-to-outer) and thus
# covered by the notarized outer seal. Skipped gracefully on CLT-only machines.
QL_DIR="${SCRIPT_DIR}/quicklook"
if [[ -d "${QL_DIR}" ]] && command -v xcodegen >/dev/null 2>&1 && xcodebuild -version >/dev/null 2>&1; then
    say "building QuickLook extension"
    if ( cd "${QL_DIR}" \
         && xcodegen generate >/dev/null 2>&1 \
         && xcodebuild -project MenuBandQuickLook.xcodeproj -scheme MenuBandQuickLook \
              -configuration Release -derivedDataPath .build build \
              CODE_SIGNING_ALLOWED=NO >/dev/null 2>&1 ); then
        QL_APPEX="${QL_DIR}/.build/Build/Products/Release/MenuBandQuickLook.app/Contents/PlugIns/ScorePreview.appex"
        if [[ -d "${QL_APPEX}" ]]; then
            mkdir -p "${APP_DIR}/Contents/PlugIns"
            rm -rf "${APP_DIR}/Contents/PlugIns/ScorePreview.appex"
            cp -R "${QL_APPEX}" "${APP_DIR}/Contents/PlugIns/"
            ok "embedded ScorePreview.appex"
        else
            warn "QuickLook build produced no appex — skipping"
        fi
    else
        warn "QuickLook extension build failed — skipping (app still installs)"
    fi
else
    say "skipping QuickLook extension (needs Xcode + xcodegen)"
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
# Sign INNER-TO-OUTER. The nested MenuBandLauncher must be signed FIRST,
# with its OWN distinct identifier, then the outer bundle sealed around
# it. Two reasons:
#   1. The launcher gets identifier computer.aestheticcomputer.menubandlauncher
#      (not the bundle's) so TCC tracks it independently — otherwise macOS
#      merges both binaries into one Accessibility entry and silently
#      revokes it on every rebuild when the bundle hash changes.
#   2. Order matters for the seal. The old flow signed the bundle with
#      --deep (which stamped the bundle identifier onto the launcher and
#      recorded THAT hash in the outer signature), then re-signed the
#      launcher with its own identifier — changing the launcher's hash and
#      leaving the outer seal pointing at a now-stale nested signature.
#      `codesign --verify --deep --strict` then reported "nested code is
#      modified or invalid" and the bundle would FAIL notarization /
#      Gatekeeper. Signing the launcher first, then the outer bundle
#      WITHOUT --deep (the launcher is already signed, so there's no
#      unsigned nested code to recurse into), seals the final launcher
#      hash correctly.
if ! codesign --force --sign "${SIGN_ID}" \
    --identifier computer.aestheticcomputer.menubandlauncher \
    --options runtime \
    --entitlements "${ENTITLEMENTS}" \
    --timestamp \
    "${APP_LAUNCHER_BIN}" 2>&1; then
    warn "launcher sign failed"
    exit 1
fi
# Sign the embedded QuickLook extension (if present) BEFORE the outer bundle,
# with its own identifier + hardened runtime, so the outer seal covers a stable
# nested signature (same inner-to-outer rule as the launcher above).
QL_APPEX_DEST="${APP_DIR}/Contents/PlugIns/ScorePreview.appex"
if [[ -d "${QL_APPEX_DEST}" ]]; then
    if ! codesign --force --sign "${SIGN_ID}" \
        --identifier computer.aestheticcomputer.menuband.quicklook.ScorePreview \
        --options runtime \
        --timestamp \
        "${QL_APPEX_DEST}" 2>&1; then
        warn "QuickLook appex sign failed"
    fi
fi
if ! codesign --force --sign "${SIGN_ID}" \
    --identifier computer.aestheticcomputer.menuband \
    --options runtime \
    --entitlements "${ENTITLEMENTS}" \
    --timestamp \
    "${APP_DIR}" 2>&1; then
    warn "codesign failed — bundle is not signed with hardened runtime"
    exit 1
fi
ok "signed"

# Verify the signed bundle BEFORE launchctl load. IconTinter.swift calls
# NSWorkspace.setIcon(forFile:) on the bundle path at startup, which
# writes a com.apple.FinderInfo xattr + Icon\r resource fork onto the
# .app — that's a known cosmetic detritus that fails codesign --strict
# but is harmless to users (Apple's notarytool already accepted the
# bundle). If we verify after launch, the strict check fails on a
# bundle we just signed cleanly. Verifying here catches the problems
# that actually matter (missing resource bundle, /Users/... runtime
# leaks) on a still-pristine signed tree.
say "verifying bundle is self-contained (no /Users/<dev>/... runtime deps)"
if "${SCRIPT_DIR}/bin/verify-bundle.sh" "${APP_DIR}"; then
    ok "bundle verification passed"
else
    warn "bundle verification failed — DO NOT package or ship this build"
    exit 1
fi

say "writing launchd plists → ${PLIST_PATH}, ${LAUNCHER_PLIST_PATH}"
mkdir -p "${LAUNCH_AGENTS}"
sed "s|@HOME@|${REPO_HOME}|g" "${PLIST_TMPL}" > "${PLIST_PATH}"
sed "s|@HOME@|${REPO_HOME}|g" "${LAUNCHER_PLIST_TMPL}" > "${LAUNCHER_PLIST_PATH}"
ok "plists written"

say "loading launch agents"
launchctl load "${PLIST_PATH}"
launchctl load "${LAUNCHER_PLIST_PATH}"
sleep 1
if launchctl list | grep -q computer.aestheticcomputer.menuband; then
    ok "computer.aestheticcomputer.menuband is running"
else
    warn "launchctl did not register the agent — check /tmp/menuband.err"
fi
if launchctl list | grep -q computer.aestheticcomputer.menubandlauncher; then
    ok "computer.aestheticcomputer.menubandlauncher is running"
else
    warn "launcher agent did not register — check /tmp/menubandlauncher.err"
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
