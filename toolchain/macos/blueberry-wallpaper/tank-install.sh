#!/usr/bin/env bash
# Join screens on several Macs into one fishery tank, left to right.
#
#   ./tank-install.sh panda chicken        # panda leads (left), chicken follows
#   ./tank-install.sh --remove panda chicken
#
# Writes ~/.config/blueberry-wallpaper/tank.json on each machine and, on the
# leader, a LaunchAgent holding a reverse SSH tunnel per follower: the
# leader's loopback port appears on each follower's loopback, so neither
# firewall has to admit the app. The leader must be able to `ssh <follower>`.
# Then push the app (./push-minis.sh) or kickstart it to pick the file up.
set -euo pipefail
cd "$(dirname "$0")"

REMOVE=0
[ "${1:-}" = "--remove" ] && { REMOVE=1; shift; }
[ $# -ge 2 ] || { echo "usage: $0 [--remove] leader follower..." >&2; exit 2; }
PORT=47390
SCREEN=2560   # points per screen; every pane must match
LEADER="$1"; shift
FOLLOWERS=("$@")
WIDTH=$(( SCREEN * (1 + ${#FOLLOWERS[@]}) ))
SSH=(ssh -o ConnectTimeout=30 -o BatchMode=yes)
LABEL=computer.aesthetic.blueberry-tank-tunnel

restart='launchctl kickstart -k gui/$(id -u)/computer.aesthetic.blueberry-wallpaper'

if [ $REMOVE -eq 1 ]; then
  for h in "$LEADER" "${FOLLOWERS[@]}"; do
    "${SSH[@]}" "$h" "rm -f ~/.config/blueberry-wallpaper/tank.json; $restart"
  done
  "${SSH[@]}" "$LEADER" "launchctl bootout gui/\$(id -u)/$LABEL 2>/dev/null; rm -f ~/Library/LaunchAgents/$LABEL.plist"
  echo "tank removed"; exit 0
fi

conf() { printf '{ "role": "%s", "width": %d, "offset": %d, "port": %d }\n' "$1" "$WIDTH" "$2" "$PORT"; }
"${SSH[@]}" "$LEADER" "mkdir -p ~/.config/blueberry-wallpaper && cat > ~/.config/blueberry-wallpaper/tank.json" <<<"$(conf leader 0)"
i=1
for h in "${FOLLOWERS[@]}"; do
  "${SSH[@]}" "$h" "mkdir -p ~/.config/blueberry-wallpaper && cat > ~/.config/blueberry-wallpaper/tank.json" <<<"$(conf follower $(( SCREEN * i )))"
  i=$(( i + 1 ))
done

# One ssh per follower, each kept alive by its own shell loop.
forwards=""
for h in "${FOLLOWERS[@]}"; do
  forwards+="(while true; do /usr/bin/ssh -N -o BatchMode=yes -o ServerAliveInterval=5 -o ServerAliveCountMax=2 -o ExitOnForwardFailure=yes -R 127.0.0.1:$PORT:127.0.0.1:$PORT $h; sleep 3; done) & "
done
"${SSH[@]}" "$LEADER" bash -s -- "$LABEL" "$forwards wait" <<'REMOTE'
set -e
label="$1"; script="$2"
plist=~/Library/LaunchAgents/$label.plist
launchctl bootout gui/$(id -u)/$label 2>/dev/null || true
rm -f "$plist"
cat > "$plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>Label</key><string>$label</string>
  <key>ProgramArguments</key><array><string>/bin/bash</string><string>-c</string><string>$script</string></array>
  <key>RunAtLoad</key><true/>
  <key>KeepAlive</key><true/>
  <key>StandardErrorPath</key><string>$HOME/.local/share/blueberry-wallpaper/tank-tunnel.log</string>
</dict></plist>
PLIST
plutil -lint "$plist" >/dev/null
launchctl enable gui/$(id -u)/$label
launchctl bootstrap gui/$(id -u) "$plist"
echo "tunnel agent up"
REMOTE

for h in "$LEADER" "${FOLLOWERS[@]}"; do "${SSH[@]}" "$h" "$restart"; done
echo "tank: $LEADER + ${FOLLOWERS[*]} · ${WIDTH}pt wide"
