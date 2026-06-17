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
# A DEDICATED keychain, not login.keychain. Over SSH the login keychain is
# locked and can't be unlocked without user interaction ("User interaction is
# not allowed"), so identity import there fails on the headless minis. A
# dedicated keychain with a known passphrase unlocks non-interactively. The
# passphrase guards only a local self-signed code-signing cert (no real
# secret), so a constant default is fine; override with SLAB_SIGN_KC_PASS.
SIGN_KEYCHAIN="${HOME}/Library/Keychains/slab-signing.keychain-db"
SIGN_KC_PASS="${SLAB_SIGN_KC_PASS:-slab-signing}"

# Sign an app bundle with the stable identity. codesign needs a real Security
# session to reach the private key; over SSH (notty) it returns
# errSecInternalComponent, so a direct sign only works when install.sh runs in
# a GUI Terminal. When it fails we run the exact same sign INSIDE the logged-in
# Aqua session via a one-shot launchd job — the same gui-bootstrap trick the
# frame capture uses to reach the WindowServer. Returns 1 if neither path
# produces a "${SIGN_CN}" signature (caller then falls back to ad-hoc).
codesign_app() {
    local app="$1"
    # Only try a direct sign when we're already in an Aqua session (install.sh
    # run from a GUI Terminal). Over SSH a direct sign fails with
    # errSecInternalComponent AND can pop a keychain dialog on the machine's
    # screen, so skip straight to the gui-bootstrap path below.
    if [ -n "${SECURITYSESSIONID:-}" ] && \
       codesign --force --deep --sign "${SIGN_CN}" \
        --identifier computer.slab.menubar "${app}" >/dev/null 2>&1; then
        return 0
    fi
    local uid label script plist log i
    uid="$(id -u)"; label="ac.slabsign.$$"
    # Use shared /tmp, NOT mktemp: over SSH mktemp lands in the ssh session's
    # private $TMPDIR (/var/folders/…), which the GUI-session launchd job can't
    # read — so the helper would never run. /tmp is visible to both sessions.
    script="/tmp/slab-sign.$$.sh"; plist="/tmp/slab-sign.$$.plist"; log="/tmp/slab-sign.$$.log"
    # Self-contained recipe run IN the Aqua session: create the dedicated
    # keychain if missing, create the identity ONCE (find-certificate guard →
    # the cert is reused on every later rebuild, so grants stay bound), set the
    # partition list (no GUI prompt), keep it on the search list, then sign.
    cat > "${script}" <<EOS
#!/bin/bash
KC='${SIGN_KEYCHAIN}'; PASS='${SIGN_KC_PASS}'; CN='${SIGN_CN}'; APP='${app}'
exec > '${log}' 2>&1
[ -f "\$KC" ] || security create-keychain -p "\$PASS" "\$KC"
security set-keychain-settings "\$KC"
security unlock-keychain -p "\$PASS" "\$KC"
if ! security find-certificate -c "\$CN" "\$KC" >/dev/null 2>&1; then
  T="\$(mktemp -d)"
  printf '%s\n' '[req]' 'distinguished_name=dn' 'prompt=no' '[dn]' "CN=\$CN" \
    '[v3]' 'keyUsage=critical,digitalSignature' 'extendedKeyUsage=critical,codeSigning' \
    'basicConstraints=critical,CA:FALSE' > "\$T/o.cnf"
  openssl req -x509 -newkey rsa:2048 -nodes -days 36500 -keyout "\$T/k.pem" -out "\$T/c.pem" -config "\$T/o.cnf" -extensions v3 >/dev/null 2>&1
  openssl pkcs12 -export -macalg sha1 -keypbe PBE-SHA1-3DES -certpbe PBE-SHA1-3DES -out "\$T/b.p12" -inkey "\$T/k.pem" -in "\$T/c.pem" -name "\$CN" -passout pass:p12 >/dev/null 2>&1
  security import "\$T/b.p12" -k "\$KC" -P p12 -T /usr/bin/codesign -T /usr/bin/security >/dev/null 2>&1
  rm -rf "\$T"
fi
security set-key-partition-list -S apple-tool:,apple: -s -k "\$PASS" "\$KC" >/dev/null 2>&1 || true
CUR="\$(security list-keychains -d user | sed -e 's/\"//g' -e 's/^ *//')"
case "\$CUR" in *slab-signing*) : ;; *) security list-keychains -d user -s \$CUR "\$KC" ;; esac
codesign --force --deep --sign "\$CN" --identifier computer.slab.menubar "\$APP"
echo "rc=\$?"
EOS
    cat > "${plist}" <<EOP
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict><key>Label</key><string>${label}</string>
<key>ProgramArguments</key><array><string>/bin/bash</string><string>${script}</string></array>
<key>RunAtLoad</key><true/></dict></plist>
EOP
    launchctl bootout "gui/${uid}/${label}" 2>/dev/null || true
    if launchctl bootstrap "gui/${uid}" "${plist}" 2>/dev/null; then
        # Gate on the helper's own completion marker (the rc= line it echoes
        # last) rather than a fixed clock, so a job whose *start* launchd
        # throttles still gets waited out instead of us racing it to an ad-hoc
        # fallback. ~60s ceiling.
        for i in $(seq 1 240); do
            grep -q '^rc=' "${log}" 2>/dev/null && break
            sleep 0.25
        done
        launchctl bootout "gui/${uid}/${label}" 2>/dev/null || true
    fi
    rm -f "${script}" "${plist}"
    if codesign -dvv "${app}" 2>&1 | grep -q "Authority=${SIGN_CN}"; then
        rm -f "${log}"
        return 0
    fi
    warn "stable-sign helper did not produce a '${SIGN_CN}' signature; last output:"
    tail -4 "${log}" 2>/dev/null | sed 's/^/    /' >&2 || true
    rm -f "${log}"
    return 1
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

# Disable Terminal.app's "Do you want to terminate the running processes?" modal
# on every profile, so Slab can close provisioned windows without a popover.
# The per-profile GUI control is Settings → Profiles → Shell → "Ask before
# closing"; its plist key is `warnOnShellCloseAction` (0 = default/"if there
# are processes other than the login shell…", 1 = always, 2 = never). We set
# every Window Setting (the default profile + any Slab-* sets) to 2 so freshly
# copied Slab-* sets inherit "never warn". The menubar can't set this via
# AppleScript (Terminal only exposes `clean commands`), so it's done here.
# Terminal caches prefs in memory and rewrites the plist on quit, so this is
# only durable while it's NOT running.
provision_terminal_close_warning() {
    local pl="${HOME}/Library/Preferences/com.apple.Terminal.plist"
    [[ -e "${pl}" ]] || { warn "Terminal prefs not found — skipping close-warning provisioning"; return 0; }
    if pgrep -x "Terminal" >/dev/null 2>&1; then
        warn "Terminal.app is running — skipping close-warning provisioning (quit Terminal and re-run to set 'Ask before closing → Never')"
        return 0
    fi
    local pb=/usr/libexec/PlistBuddy n=0
    # Enumerate profile names under "Window Settings": PlistBuddy prints each
    # top-level profile as `    <name> = Dict {` at exactly 4 leading spaces.
    # The plist embeds binary color/font archives, so force a byte-safe locale
    # for sed and strip non-printable bytes before matching.
    local names
    names="$(${pb} -c "Print :'Window Settings'" "${pl}" 2>/dev/null \
        | LC_ALL=C tr -cd '\11\12\15\40-\176' \
        | LC_ALL=C sed -n 's/^    \([^ ].*\) = Dict {$/\1/p')"
    local IFS=$'\n'
    for name in ${names}; do
        ${pb} -c "Set :'Window Settings':'${name}':warnOnShellCloseAction 2" "${pl}" 2>/dev/null \
            || ${pb} -c "Add :'Window Settings':'${name}':warnOnShellCloseAction integer 2" "${pl}" 2>/dev/null || true
        n=$((n+1))
    done
    ok "provisioned ${n} Terminal profile(s): Ask before closing → Never"
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
provision_terminal_close_warning

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
if codesign_app "${APP_DIR}"; then
    SIGN_OK=1
    ok "signed with '${SIGN_CN}' (stable identity — TCC grants persist across rebuilds)"
else
    warn "stable signing unavailable; using ad-hoc (TCC grants reset each rebuild)"
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
