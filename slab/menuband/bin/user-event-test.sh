#!/usr/bin/env bash
# Exercise Menu Band's Command gesture through the real macOS event pipeline.

set -euo pipefail

LOG=/tmp/menuband-debug.log
MARKER="quiet focus gesture qualified"
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
DRIVER=/tmp/menuband-user-event-driver

if ! pgrep -x MenuBand >/dev/null; then
    echo "MenuBand is not running; launch bin/dev.sh first." >&2
    exit 1
fi

xcrun swiftc "$SCRIPT_DIR/user-event-driver.swift" -o "$DRIVER"

marker_count() {
    if [[ -f "$LOG" ]]; then
        grep -c "$MARKER" "$LOG" || true
    else
        echo 0
    fi
}

# A normal shortcut followed by Command must not be mistaken for two bare taps.
before=$(marker_count)
"$DRIVER" command-c
sleep 0.10
"$DRIVER" command-tap
sleep 0.25
after=$(marker_count)
if [[ "$after" != "$before" ]]; then
    echo "FAIL: Command-C then Command armed focus" >&2
    exit 1
fi
echo "PASS: Command-C then Command did not arm focus"

# Two genuinely bare taps must still qualify.
"$DRIVER" command-tap
sleep 0.10
"$DRIVER" command-tap
sleep 0.25
after=$(marker_count)
if [[ "$after" -ne $((before + 1)) ]]; then
    echo "FAIL: bare Command double-tap did not arm focus" >&2
    exit 1
fi
echo "PASS: bare Command double-tap armed focus"

# Leave the user's keyboard in its ordinary, unfocused state.
osascript -e 'tell application "System Events" to key code 53'
