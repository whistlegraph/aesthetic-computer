#!/bin/bash
# Cheap free-space warning; cleanup remains the separate Cleaner job.
set -euo pipefail

STATE="$HOME/.local/share/slab/disk-space"
LABEL=computer.aesthetic.disk-space-watch
FLOOR_KB=20971520 # 20 GiB reserved for swap, updates, and interactive work.

case "${1:---check}" in
  --install)
    mkdir -p "$HOME/.local/bin" "$HOME/Library/LaunchAgents" "$STATE"
    target="$HOME/.local/bin/disk-space-watch"
    if [[ "$0" != "$target" ]]; then install -m 0755 "$0" "$target"; fi
    plist="$HOME/Library/LaunchAgents/$LABEL.plist"
    cat > "$plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>Label</key><string>$LABEL</string>
  <key>ProgramArguments</key><array><string>$target</string><string>--check</string></array>
  <key>RunAtLoad</key><true/>
  <key>StartInterval</key><integer>3600</integer>
  <key>ProcessType</key><string>Background</string>
  <key>LowPriorityIO</key><true/>
</dict></plist>
EOF
    plutil -lint "$plist"
    launchctl bootout "gui/$(id -u)/$LABEL" >/dev/null 2>&1 || true
    launchctl bootstrap "gui/$(id -u)" "$plist"
    echo "Installed hourly disk-space warning (20 GiB floor)."
    exit 0
    ;;
  --check) ;;
  *) echo "usage: disk-space-watch.sh [--check | --install]" >&2; exit 2 ;;
esac

volume=/System/Volumes/Data
[[ -d "$volume" ]] || volume=/
available=$(df -Pk "$volume" | awk 'NR == 2 {print $4}')
case "$available" in ''|*[!0-9]*) echo "Cannot read free disk space" >&2; exit 1 ;; esac
mkdir -p "$STATE"
now=$(date +%s)
printf '{"checkedAt":%s,"availableKiB":%s,"floorKiB":%s}\n' \
  "$now" "$available" "$FLOOR_KB" > "$STATE/latest.json"

if (( available >= FLOOR_KB )); then
  # Keep the alert timestamp across recovery so a brief dip cannot notify twice.
  rm -f "$STATE/low-space"
  exit 0
fi
: > "$STATE/low-space"
last=$(cat "$STATE/last-alert" 2>/dev/null || echo 0)
case "$last" in ''|*[!0-9]*) last=0 ;; esac
if (( now - last >= 86400 )); then
  free_gib=$(awk -v kb="$available" 'BEGIN {printf "%.1f", kb / 1048576}')
  /usr/bin/osascript -e "display notification \"${free_gib} GiB free; keep 20 GiB available. Use Cleaner and keep large jobs on compute hosts.\" with title \"Low disk space\"" >/dev/null 2>&1
  printf '%s\n' "$now" > "$STATE/last-alert"
fi
