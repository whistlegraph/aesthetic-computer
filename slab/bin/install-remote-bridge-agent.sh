#!/usr/bin/env bash
# install-remote-bridge-agent.sh — run claude-remote-bridge as a LaunchAgent so
# the remote-session mirror is always live (survives reboots / logout), not just
# while a `jasellite` window is open. Idempotent.
set -euo pipefail

LABEL="computer.slab.remote-bridge"
PLIST="$HOME/Library/LaunchAgents/$LABEL.plist"
BRIDGE="$HOME/aesthetic-computer/slab/bin/claude-remote-bridge.mjs"
LOG="/tmp/$LABEL.log"

# fnm's per-shell node paths are volatile; the `default` alias is stable.
NODE="$HOME/.local/share/fnm/aliases/default/bin/node"
[ -x "$NODE" ] || NODE="$(command -v node)"
[ -x "$NODE" ] || { echo "no node found"; exit 1; }

# Retire any hand-started bridge so launchd becomes the sole owner (its single-
# instance guard would otherwise make the launchd copy exit and KeepAlive-spin).
pkill -f 'claude-remote-bridge' 2>/dev/null || true
rm -f "$HOME/.local/share/slab/state/remote-bridge.pid"

mkdir -p "$HOME/Library/LaunchAgents"
cat > "$PLIST" <<PLISTEOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key><string>$LABEL</string>
    <key>ProgramArguments</key>
    <array>
        <string>$NODE</string>
        <string>$BRIDGE</string>
    </array>
    <key>RunAtLoad</key><true/>
    <key>KeepAlive</key><true/>
    <key>ThrottleInterval</key><integer>5</integer>
    <key>StandardOutPath</key><string>$LOG</string>
    <key>StandardErrorPath</key><string>$LOG</string>
</dict>
</plist>
PLISTEOF

launchctl unload "$PLIST" 2>/dev/null || true
launchctl load "$PLIST"
sleep 2
echo "node:  $NODE"
echo "plist: $PLIST"
echo -n "status: "
launchctl list | grep "$LABEL" || echo "(not listed)"
echo -n "running: "
pgrep -f claude-remote-bridge | head -1 || echo "(none)"
