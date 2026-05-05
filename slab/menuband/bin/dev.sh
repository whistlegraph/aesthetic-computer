#!/usr/bin/env bash
# dev.sh — fast debug build + run loop for Menu Band.
#
# This is NOT in-place hot-reload (see SCORE.md → "Why no hot-reload"),
# but it's a much faster iteration loop than install.sh: skips signing,
# skips launchd, skips bundle assembly, runs the unsigned debug binary
# directly. Edit code → Ctrl-C → re-run this script. Subsequent runs
# rebuild incrementally so the cycle is usually 2-5 seconds.
#
# For an automated rebuild-on-save loop with state-preserving restart,
# use bin/watch-reload.sh instead.

set -euo pipefail

CYAN=$'\033[1;36m'
YELLOW=$'\033[1;33m'
DIM=$'\033[2m'
RESET=$'\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Stop the launchd-managed production daemon so we don't end up with
# two menubar items fighting over the same status item slot.
PLIST="${HOME}/Library/LaunchAgents/computer.aestheticcomputer.menuband.plist"
if [[ -f "${PLIST}" ]] && launchctl list | grep -q computer.aestheticcomputer.menuband; then
    printf "%s• stopping launchd Menu Band%s\n" "$CYAN" "$RESET"
    launchctl unload "${PLIST}" 2>/dev/null || true
fi
pkill -f "/MenuBand$" 2>/dev/null || true
sleep 0.3

cd "${PROJECT_DIR}"

printf "%s• building + launching debug Menu Band…%s\n" "$CYAN" "$RESET"
printf "%s  Ctrl-C to quit, then ./install.sh to restore the signed daemon%s\n\n" "$DIM" "$RESET"

# `--scratch-path` keeps the debug build dir separate from the release
# tree install.sh writes into, so debug + release don't trip each other.
exec swift run -c debug \
    --scratch-path "${PROJECT_DIR}/.build-debug" \
    MenuBand
