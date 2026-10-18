#!/usr/bin/env bash
# install-lane-watch.sh — run iris-lane-watch.mjs as a LaunchAgent on watchers.
#
#   macpal/bin/install-lane-watch.sh                    this Mac
#   macpal/bin/install-lane-watch.sh neo blueberry      over ssh (aliases)
#   LANE_HOSTS="panda chicken" … (default)              where the board is read
#
# Copies the script to ~/.local/bin, replaces the older flattening watcher's
# agent (same label), and keeps one process looping every 10s. Node resolves
# to a stable path (Homebrew, then fnm's default alias), never a shell's
# transient fnm multishell.
set -euo pipefail
cd "$(dirname "$0")"
LANE_HOSTS="${LANE_HOSTS:-panda chicken}"
LABEL=studio.fuser.iris-lane-watch

remote_setup() {   # runs on the target: $1 = lane hosts
  set -e
  node=""
  for n in /opt/homebrew/bin/node /usr/local/bin/node "$HOME/.local/share/fnm/aliases/default/bin/node"; do
    [ -x "$n" ] && { node=$n; break; }
  done
  [ -n "$node" ] || { echo "no stable node found" >&2; exit 1; }
  mkdir -p "$HOME/.local/share/desktop-badge" "$HOME/Library/Logs"
  args=""
  for h in $1; do args="$args<string>$h</string>"; done
  plist="$HOME/Library/LaunchAgents/$LABEL.plist"
  cat > "$plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>Label</key><string>$LABEL</string>
<key>ProgramArguments</key><array><string>$node</string><string>$HOME/.local/bin/iris-lane-watch.mjs</string><string>--every</string><string>10</string>$args</array>
<key>RunAtLoad</key><true/>
<key>KeepAlive</key><true/>
<key>ThrottleInterval</key><integer>15</integer>
<key>StandardErrorPath</key><string>$HOME/Library/Logs/iris-lane-watch.log</string>
</dict></plist>
PLIST
  launchctl bootout "gui/$(id -u)/$LABEL" 2>/dev/null || true
  launchctl bootstrap "gui/$(id -u)" "$plist"
  echo "$(hostname -s): $LABEL → $node, reading $1"
}

if [ $# -eq 0 ]; then
  mkdir -p "$HOME/.local/bin"
  install -m 755 iris-lane-watch.mjs "$HOME/.local/bin/iris-lane-watch.mjs"
  LABEL=$LABEL remote_setup "$LANE_HOSTS"
  exit
fi
for host in "$@"; do
  ssh "$host" 'mkdir -p ~/.local/bin && cat > ~/.local/bin/iris-lane-watch.mjs && chmod 755 ~/.local/bin/iris-lane-watch.mjs' < iris-lane-watch.mjs
  ssh "$host" "LABEL=$LABEL bash -s -- '$LANE_HOSTS'" <<< "$(declare -f remote_setup); remote_setup \"\$1\""
done
