#!/usr/bin/env bash
# install-launcher.sh — build, sign, and install JUST the ⌘⌘ launcher.
#
# Why this exists separately from install.sh:
#
#   ⌘⌘ (same-side double-tap of either Command key) has two halves. When Menu
#   Band is already running, an in-process event tap handles it. When Menu Band
#   is NOT running, nothing in the app can hear the keystroke — a tiny
#   always-resident helper, MenuBandLauncher, does, and spawns the app.
#
#   install.sh embeds that helper in the SwiftPM direct-download bundle. But
#   the Xcode target (project.yml → the App Store / notarized DMG build,
#   bundle id computer.aesthetic.menuband) deliberately OMITS it — App Sandbox
#   forbids CGEventTap. So a Mac running only the Xcode/App-Store Menu Band has
#   no launcher at all, and ⌘⌘ does nothing until the app is opened by hand.
#
#   This installs the launcher standalone, as ~/Applications/MenuBandLauncher.app
#   plus a launch agent. It finds whichever Menu Band is installed at runtime
#   (see resolveMenuBandBundle in Sources/MenuBandLauncher/main.swift), so it
#   works with either build — or pin one with MENUBAND_APP=/path/to/App.app.
#
# Idempotent. Safe to re-run after edits. Cheap: the launcher target is ~200
# lines with no dependencies, so this is seconds, not a full Menu Band build.

set -euo pipefail

CYAN=$'\033[1;36m'
GREEN=$'\033[1;32m'
YELLOW=$'\033[1;33m'
RESET=$'\033[0m'

say() { printf "%s• %s%s\n" "$CYAN" "$1" "$RESET"; }
ok()  { printf "%s✓ %s%s\n" "$GREEN" "$1" "$RESET"; }
warn(){ printf "%s! %s%s\n" "$YELLOW" "$1" "$RESET"; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LAUNCH_AGENTS="${HOME}/Library/LaunchAgents"
PLIST_LABEL="computer.aestheticcomputer.menubandlauncher"
PLIST_PATH="${LAUNCH_AGENTS}/${PLIST_LABEL}.plist"
PLIST_TMPL="${SCRIPT_DIR}/${PLIST_LABEL}.plist.tmpl"
INSTALLED_APP="${HOME}/Applications/MenuBandLauncher.app"
LAUNCHER_BIN="${INSTALLED_APP}/Contents/MacOS/MenuBandLauncher"

# Same rationale as install.sh: stage and sign away from the live bundle, then
# rename. Replacing files inside a running signed app SIGKILLs it when macOS
# faults in a page whose hash no longer matches.
mkdir -p "${HOME}/Applications"
STAGE_ROOT="$(mktemp -d "${HOME}/Applications/.menubandlauncher-install.XXXXXX")"
trap 'rm -rf "${STAGE_ROOT}"' EXIT
APP_DIR="${STAGE_ROOT}/MenuBandLauncher.app"

command -v swiftc >/dev/null 2>&1 || {
    echo "swiftc not found — install Xcode Command Line Tools first:"
    echo "    xcode-select --install"
    exit 1
}

# Compile with swiftc directly rather than `swift build --target
# MenuBandLauncher`. The launcher is one file importing only AppKit /
# ApplicationServices / Foundation, so it needs nothing from the package graph —
# and going through SwiftPM would make this script hostage to every OTHER
# dependency in Package.swift resolving cleanly, which is exactly the failure
# that motivated writing it this way (a sibling package's invalid library
# product aborted graph loading before a single file compiled).
#
# Universal, matching install.sh — a launcher that only runs on one
# architecture is a launcher that silently doesn't run on the other Mac.
SRC="${SCRIPT_DIR}/Sources/MenuBandLauncher/main.swift"
BUILD_DIR="${STAGE_ROOT}/build"
mkdir -p "${BUILD_DIR}"
for arch in arm64 x86_64; do
    say "building MenuBandLauncher ${arch} slice"
    swiftc -O -target "${arch}-apple-macos11.0" \
        -o "${BUILD_DIR}/MenuBandLauncher-${arch}" "${SRC}"
done
ARM_BIN="${BUILD_DIR}/MenuBandLauncher-arm64"
X86_BIN="${BUILD_DIR}/MenuBandLauncher-x86_64"

say "assembling ${APP_DIR}"
mkdir -p "${APP_DIR}/Contents/MacOS"
lipo -create -output "${APP_DIR}/Contents/MacOS/MenuBandLauncher" "${ARM_BIN}" "${X86_BIN}"
chmod +x "${APP_DIR}/Contents/MacOS/MenuBandLauncher"
strip -S "${APP_DIR}/Contents/MacOS/MenuBandLauncher"
cp "${SCRIPT_DIR}/Info-Launcher.plist" "${APP_DIR}/Contents/Info.plist"
xattr -cr "${APP_DIR}" 2>/dev/null || true

# The identifier must stay EXACTLY computer.aestheticcomputer.menubandlauncher
# across rebuilds, and the signing cert must stay a stable Apple-issued one.
# TCC keys the Accessibility grant on identifier + signing authority; ad-hoc
# signing changes the designated requirement every build and macOS silently
# revokes the grant, at which point CGEvent.tapCreate returns nil and ⌘⌘ dies
# with no visible symptom. Prefer Developer ID; refuse to install ad-hoc.
SIGN_ID="${SIGN_IDENTITY:-$(security find-identity -v -p codesigning \
    "${HOME}/Library/Keychains/login.keychain-db" 2>/dev/null \
    | awk -F\" '/Developer ID Application/ {print $2; exit}')}"
if [[ -z "${SIGN_ID}" ]]; then
    warn "no Developer ID Application cert found."
    warn "Set SIGN_IDENTITY=<identity> to override, but note that an ad-hoc or"
    warn "self-signed launcher loses its Accessibility grant on every rebuild."
    exit 1
fi

say "signing with: ${SIGN_ID}"
codesign --force --sign "${SIGN_ID}" \
    --identifier "${PLIST_LABEL}" \
    --options runtime \
    --timestamp \
    "${APP_DIR}/Contents/MacOS/MenuBandLauncher"
codesign --force --sign "${SIGN_ID}" \
    --identifier "${PLIST_LABEL}" \
    --options runtime \
    --timestamp \
    "${APP_DIR}"
codesign --verify --deep --strict "${APP_DIR}"
ok "signed and verified"

say "writing launch agent → ${PLIST_PATH}"
mkdir -p "${LAUNCH_AGENTS}"
sed -e "s|@HOME@|${HOME}|g" \
    -e "s|@LAUNCHER_BIN@|${LAUNCHER_BIN}|g" \
    "${PLIST_TMPL}" > "${PLIST_PATH}"

# Stop the old copy BEFORE swapping the bundle out from under it.
launchctl bootout "gui/$(id -u)/${PLIST_LABEL}" 2>/dev/null || true

say "installing → ${INSTALLED_APP}"
rm -rf "${INSTALLED_APP}"
mv "${APP_DIR}" "${INSTALLED_APP}"

say "bootstrapping launch agent"
launchctl bootstrap "gui/$(id -u)" "${PLIST_PATH}"
launchctl kickstart -k "gui/$(id -u)/${PLIST_LABEL}"

sleep 1
if launchctl print "gui/$(id -u)/${PLIST_LABEL}" >/dev/null 2>&1; then
    ok "launch agent running"
else
    warn "launch agent did not come up — check /tmp/menubandlauncher.err"
fi

echo
# The permission that matters is Input Monitoring, and its absence is SILENT:
# the tap installs fine and just never receives an event. Read the log rather
# than trusting "launch agent running".
if tail -20 /tmp/menubandlauncher.err 2>/dev/null | grep -q "Input Monitoring granted"; then
    ok "Input Monitoring granted, event tap installed"
    ok "double-tap either ⌘ to summon Menu Band"
else
    warn "GRANT INPUT MONITORING — until then ⌘⌘ does nothing (silently):"
    warn "  System Settings > Privacy & Security > Input Monitoring"
    warn "  enable 'Menu Band Launcher', or + → ${INSTALLED_APP}"
    warn "If it isn't offered, TCC is holding a denial — clear it with:"
    warn "  tccutil reset ListenEvent ${PLIST_LABEL}"
    warn "Then: launchctl kickstart -k gui/\$(id -u)/${PLIST_LABEL}"
    warn "Log: /tmp/menubandlauncher.err"
fi
