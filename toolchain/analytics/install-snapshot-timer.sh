#!/usr/bin/env bash
# Install the network-report snapshot timer. Run this on jasellite.
#
# Every ten minutes it builds the default network report and keeps it at
# ~/.local/share/ac/analytics/network-report.json, where the analytics MCP on
# any Mac reads it in one ssh hop instead of building the report itself.
#
# The checkout needs the vault beside it (the lith ssh key, and the GPG key for
# the App Store and Google credentials). Without them the run fails and no
# snapshot is written, so Macs keep building the report live.
#
#   toolchain/analytics/install-snapshot-timer.sh            install and start
#   toolchain/analytics/install-snapshot-timer.sh --remove   stop and remove
set -euo pipefail

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
UNITS="$HOME/.config/systemd/user"
NAME=network-snapshot

if [ "${1:-}" = "--remove" ]; then
  systemctl --user disable --now "$NAME.timer" 2>/dev/null || true
  rm -f "$UNITS/$NAME.service" "$UNITS/$NAME.timer"
  systemctl --user daemon-reload
  echo "removed $NAME"
  exit 0
fi

mkdir -p "$UNITS"
cat > "$UNITS/$NAME.service" <<UNIT
[Unit]
Description=Aesthetic network report snapshot

[Service]
Type=oneshot
WorkingDirectory=$REPO
Environment=AC_ANALYTICS_SNAPSHOT_HOST=
ExecStart=$(command -v node) toolchain/mcp/analytics-mcp.mjs --snapshot
TimeoutStartSec=120
UNIT
cat > "$UNITS/$NAME.timer" <<UNIT
[Unit]
Description=Aesthetic network report snapshot, every ten minutes

[Timer]
OnCalendar=*:0/10
RandomizedDelaySec=30
Persistent=true

[Install]
WantedBy=timers.target
UNIT

systemctl --user daemon-reload
systemctl --user enable --now "$NAME.timer"
systemctl --user start "$NAME.service" && echo "first snapshot written" || echo "first run failed: journalctl --user -u $NAME.service -n 20"
systemctl --user list-timers "$NAME.timer" --no-pager
