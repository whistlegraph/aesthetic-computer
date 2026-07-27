#!/usr/bin/env bash
# Keep a tiny launchd retry until the currently-offline PoorSlice wakes.
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
LABEL="computer.aesthetic.fleet-worker-poorslice-install"
PLIST="$HOME/Library/LaunchAgents/$LABEL.plist"
STATE="$HOME/.local/share/ac-fleet-worker"
RUNNER="$STATE/install-poorslice-on-wake.sh"
MARKER="$STATE/poorslice-installed"
mkdir -p "$HOME/Library/LaunchAgents" "$STATE"

cat > "$RUNNER" <<RUNNER
#!/bin/bash
if [ -f '$MARKER' ]; then exit 0; fi
cd '$REPO' || exit 1
if toolchain/fleet/deploy-worker.sh poorslice poorslice heavy /Users/aesthetic/aesthetic-computer && \
   slab/menubar-swift/deploy-host.sh poorslice; then
  touch '$MARKER'
  /usr/bin/osascript -e 'display notification "PoorSlice joined the render fleet with persistent stats." with title "AC fleet"' >/dev/null 2>&1 || true
fi
RUNNER
chmod 700 "$RUNNER"

cat > "$PLIST" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>Label</key><string>$LABEL</string>
  <key>ProgramArguments</key><array><string>/bin/bash</string><string>$RUNNER</string></array>
  <key>RunAtLoad</key><true/>
  <key>StartInterval</key><integer>120</integer>
  <key>ProcessType</key><string>Background</string>
  <key>LowPriorityIO</key><true/>
  <key>StandardOutPath</key><string>$STATE/poorslice-install.out</string>
  <key>StandardErrorPath</key><string>$STATE/poorslice-install.err</string>
</dict></plist>
PLIST
plutil -lint "$PLIST" >/dev/null
launchctl bootout "gui/$(id -u)/$LABEL" 2>/dev/null || true
attempt=0
while ! launchctl bootstrap "gui/$(id -u)" "$PLIST" 2>/dev/null; do
  attempt=$((attempt + 1)); (( attempt < 20 )) || { launchctl bootstrap "gui/$(id -u)" "$PLIST"; break; }
  sleep 0.1
done
echo "queued PoorSlice deployment retry every 120 seconds"
