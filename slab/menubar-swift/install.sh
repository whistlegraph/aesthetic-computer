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
BIN_DIR="${REPO_HOME}/.local/bin"
BIN_PATH="${BIN_DIR}/slab-menubar"

say() { printf "%s• %s%s\n" "$CYAN" "$1" "$RESET"; }
ok()  { printf "%s✓ %s%s\n" "$GREEN" "$1" "$RESET"; }
warn(){ printf "%s! %s%s\n" "$YELLOW" "$1" "$RESET"; }

command -v swift >/dev/null 2>&1 || {
    echo "swift not found — install Xcode Command Line Tools first:"
    echo "    xcode-select --install"
    exit 1
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

say "unloading any existing menubar launch agent"
if launchctl list | grep -q computer.slab.menubar; then
    launchctl unload "${PLIST_PATH}" 2>/dev/null || true
    ok "unloaded computer.slab.menubar"
else
    warn "no running menubar to unload — skipping"
fi

say "installing binary → ${BIN_PATH}"
mkdir -p "${BIN_DIR}"
cp "${BUILT}" "${BIN_PATH}"
chmod +x "${BIN_PATH}"
ok "binary installed"

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
echo "  binary:       ${BIN_PATH}"
echo "  plist:        ${PLIST_PATH}"
echo "  stdout:       /tmp/slab-menubar.out"
echo "  stderr:       /tmp/slab-menubar.err"
echo
echo "  rebuild+reload after edits:  $(basename "$0")"
echo "  tail logs:                   tail -f /tmp/slab-menubar.err"
