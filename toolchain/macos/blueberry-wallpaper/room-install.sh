#!/usr/bin/env bash
# Give a Mac whose wallpaper feeder carries no room (the Fuser minis) its own
# room probe: ac-local-probe.mjs --watch as a LaunchAgent, writing room.json
# beside the feed for the wallpaper to read. No checkout needed there.
#
#   ./room-install.sh panda chicken        # also keeps their radio off (no-radio)
set -euo pipefail
cd "$(dirname "$0")"
SRC=../../analytics
for host in "$@"; do
  ssh -o BatchMode=yes "$host" 'mkdir -p ~/.local/share/ac-room ~/.config/blueberry-wallpaper ~/Library/LaunchAgents && touch ~/.config/blueberry-wallpaper/no-radio'
  rsync -a "$SRC/ac-local-probe.mjs" "$SRC/ac-sites-probe.mjs" "$host:.local/share/ac-room/"
  ssh -o BatchMode=yes "$host" bash -s <<'REMOTE'
set -e
node=$(command -v node || ls /opt/homebrew/bin/node /usr/local/bin/node 2>/dev/null | head -1)
agent=~/Library/LaunchAgents/computer.aesthetic.ac-room.plist
cat > "$agent" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>computer.aesthetic.ac-room</string>
  <key>ProgramArguments</key>
  <array><string>$node</string><string>$HOME/.local/share/ac-room/ac-local-probe.mjs</string><string>--watch</string></array>
  <key>RunAtLoad</key><true/>
  <key>KeepAlive</key><true/>
  <key>ProcessType</key><string>Background</string>
  <key>Nice</key><integer>10</integer>
  <key>StandardOutPath</key><string>$HOME/.local/share/ac-room/room.log</string>
  <key>StandardErrorPath</key><string>$HOME/.local/share/ac-room/room.log</string>
</dict>
</plist>
PLIST
plutil -lint "$agent" >/dev/null
launchctl bootout gui/$(id -u)/computer.aesthetic.ac-room 2>/dev/null || true
launchctl bootstrap gui/$(id -u) "$agent"
echo "room probe running on $(hostname -s) with $node"
REMOTE
done
