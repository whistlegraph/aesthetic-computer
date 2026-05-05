#!/usr/bin/env bash
# watch-reload.sh — fswatch Sources/, rebuild + relaunch on change, reopen
# the popover so iteration on liquid-glass UI feels close to live-reload.
#
# Usage:
#   ./bin/watch-reload.sh           # watch all Sources/
#   ./bin/watch-reload.sh popover   # only refire on MenuBandPopover.swift
#
# Requires: fswatch (`brew install fswatch`).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

CYAN=$'\033[1;36m'
GREEN=$'\033[1;32m'
RED=$'\033[1;31m'
DIM=$'\033[2m'
RESET=$'\033[0m'

if ! command -v fswatch >/dev/null 2>&1; then
    printf "%sfswatch not installed%s — run: %sbrew install fswatch%s\n" \
        "$RED" "$RESET" "$CYAN" "$RESET"
    exit 1
fi

# Filter the watched paths. Default = whole Sources tree. With "popover"
# arg, narrow to the popover file so heavy edits elsewhere don't trip the
# rebuild loop while you're iterating on chrome.
WATCH_PATHS=("${PROJECT_DIR}/Sources")
if [[ "${1:-}" == "popover" ]]; then
    WATCH_PATHS=(
        "${PROJECT_DIR}/Sources/MenuBand/MenuBandPopover.swift"
        "${PROJECT_DIR}/Sources/MenuBand/Localization.swift"
    )
fi

post_show_popover() {
    # Distributed notification name registered in AppDelegate.swift —
    # `handleShowPopoverNotification` re-opens the popover only if it
    # isn't already shown, so repeated triggers don't flicker it shut.
    /usr/bin/swift -e '
import Foundation
DistributedNotificationCenter.default().post(
    name: NSNotification.Name("computer.aestheticcomputer.menuband.showPopover"),
    object: nil)
RunLoop.main.run(until: Date(timeIntervalSinceNow: 0.1))
' >/dev/null 2>&1 || true
}

reload() {
    local started_at
    started_at=$(date +%H:%M:%S)
    printf "\n%s[watch %s] rebuild…%s\n" "$CYAN" "$started_at" "$RESET"
    if ! ( cd "$PROJECT_DIR" && bash install.sh >/tmp/menuband-watch.log 2>&1 ); then
        printf "%s[watch] install.sh failed — see /tmp/menuband-watch.log%s\n" \
            "$RED" "$RESET"
        tail -15 /tmp/menuband-watch.log
        return 1
    fi
    # Give the new MenuBand instance a beat to register its observer
    # before posting the show-popover notification.
    sleep 0.6
    post_show_popover
    printf "%s[watch] reloaded → popover reopened%s\n" "$GREEN" "$RESET"
}

printf "%swatching:%s\n" "$CYAN" "$RESET"
for p in "${WATCH_PATHS[@]}"; do printf "  %s%s%s\n" "$DIM" "$p" "$RESET"; done

# Initial build so the first save isn't a no-op restart of stale state.
reload || true

# `-or` recurses + outputs once per batch; `--latency 0.4` debounces
# rapid saves (editor write-then-rename, format-on-save) into one rebuild.
fswatch -or --latency 0.4 -e ".*/\.build/.*" -e ".*/\.swiftpm/.*" \
    "${WATCH_PATHS[@]}" | while read -r _; do
    reload || true
done
