#!/bin/sh
# juke-sync — keep finished pop tracks (pop/**/out/*.mp3) converged across
# the fleet through the shelf-sync Space, shared prefix s3://shelf-sync/juke/.
#
#   pop/bin/juke-sync.sh up       push local finals newer than the Space
#   pop/bin/juke-sync.sh down     fetch finals the Space has and we lack
#   pop/bin/juke-sync.sh both     up then down (default) — run on each
#                                 machine and the fleet converges, newest wins
#   pop/bin/juke-sync.sh light    up, catalog only, then bound verified downloads
#   pop/bin/juke-sync.sh install light  install on-demand mode on this machine
#   pop/bin/juke-sync.sh install  write + load a launchd agent that runs
#                                 `both` every 20 minutes on this machine
#
# Scope is deliberately mp3 finals only: stems, wavs, and raw renders stay on
# the machine that made them (pop/.gitignore keeps them out of git for size
# and sample-licensing reasons; neo runs tight on disk). aws s3 sync is
# incremental — unchanged tracks cost nothing.

set -e
set -f # no globbing: $FILTERS carries literal aws --include/--exclude patterns

POP_DIR="$(cd "$(dirname "$0")/.." && pwd)"
BUCKET="s3://shelf-sync/juke"
AWS_ARGS="--endpoint-url https://sfo3.digitaloceanspaces.com --region sfo3"
FILTERS='--exclude * --include */out/*.mp3'

up() {
  # shellcheck disable=SC2086
  aws s3 sync "$POP_DIR" "$BUCKET" $AWS_ARGS $FILTERS
}

down() {
  # shellcheck disable=SC2086
  aws s3 sync "$BUCKET" "$POP_DIR" $AWS_ARGS $FILTERS
}

install_agent() {
  mode="${1:-both}"
  case "$mode" in light|both|up) ;; *) echo "invalid sync mode" >&2; exit 2;; esac
  if [ "$mode" = light ]; then
    test -x "$HOME/.local/bin/ac-juke-cache" || { echo "Install ac-juke-cache first" >&2; exit 1; }
  fi
  plist="$HOME/Library/LaunchAgents/computer.aesthetic.juke-sync.plist"
  cat > "$plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>computer.aesthetic.juke-sync</string>
  <key>ProgramArguments</key>
  <array>
    <string>/bin/sh</string>
    <string>$POP_DIR/bin/juke-sync.sh</string>
    <string>$mode</string>
  </array>
  <key>StartInterval</key><integer>1200</integer>
  <key>StandardOutPath</key><string>/tmp/juke-sync.log</string>
  <key>StandardErrorPath</key><string>/tmp/juke-sync.log</string>
  <key>EnvironmentVariables</key>
  <dict><key>PATH</key><string>$HOME/.local/bin:$HOME/.local/share/fnm/aliases/default/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin</string></dict>
</dict>
</plist>
EOF
  launchctl unload "$plist" 2>/dev/null || true
  launchctl load "$plist"
  echo "juke-sync agent loaded ($mode, every 20 min): $plist"
}

case "${1:-both}" in
  up) up ;;
  down) down ;;
  both) up; down ;;
  light)
    up
    "$HOME/.local/bin/ac-juke-cache" catalog
    node "$POP_DIR/bin/pop-library.mjs" --no-probe
    "$HOME/.local/bin/ac-juke-cache" prune
    ;;
  install) install_agent "${2:-both}" ;;
  *) echo "usage: juke-sync.sh [up|down|both|light|install [light|both|up]]" >&2; exit 2 ;;
esac
