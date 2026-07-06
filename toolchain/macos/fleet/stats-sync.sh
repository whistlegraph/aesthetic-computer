#!/bin/bash
# stats-sync.sh — push the canonical Stats (exelban) menu-bar config to fleet Macs,
# register Stats as a login item, and launch it. Idempotent.
#
#   bash stats-sync.sh <host> [<host> ...]   # e.g. neo chicken panda
#   bash stats-sync.sh all                   # neo chicken panda (blueberry: see blueberry-join.sh)
#
# The canonical config lives beside this script as stats-shared.plist — it mirrors
# neo's menu bar: CPU/RAM/GPU/Disk (mini) + Network (speed); Sensors/Battery off.
# Machine-specific keys (remote_id, window/status-item positions) are intentionally
# excluded so each Mac lays out naturally.
#
# Login-at-startup: Stats 3.x uses SMAppService (not reliably settable from CLI), so
# we register a classic LaunchServices login item pointing at /Applications/Stats.app,
# which launches it at boot just the same. Single-instance app, so no double-launch.
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
PLIST="$DIR/stats-shared.plist"
[ -f "$PLIST" ] || { echo "missing $PLIST"; exit 1; }

hosts=("$@")
[ "${hosts[0]:-}" = "all" ] && hosts=(neo chicken panda)
[ ${#hosts[@]} -eq 0 ] && { echo "usage: bash stats-sync.sh <host> [host...] | all"; exit 1; }

for host in "${hosts[@]}"; do
  echo "=== $host ==="
  scp -q -o ConnectTimeout=8 "$PLIST" "$host:/tmp/stats-shared.plist"
  ssh -o ConnectTimeout=8 "$host" 'sh -s' <<'REMOTE'
    if [ ! -d /Applications/Stats.app ]; then echo "Stats not installed on $(hostname -s)"; exit 1; fi
    pkill -x Stats 2>/dev/null || true; sleep 1
    python3 - <<'PY'
import plistlib, subprocess
p = plistlib.load(open("/tmp/stats-shared.plist","rb"))
for k,v in p.items():
    if isinstance(v,bool): t,vv="-bool",("true" if v else "false")
    elif isinstance(v,int): t,vv="-int",str(v)
    else: t,vv="-string",str(v)
    subprocess.run(["defaults","write","eu.exelban.Stats",k,t,vv],check=True)
print("imported",len(p),"Stats keys")
PY
    rm -f /tmp/stats-shared.plist
    osascript -e 'tell application "System Events" to delete (every login item whose name is "Stats")' >/dev/null 2>&1 || true
    osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Stats.app", hidden:true}' >/dev/null 2>&1 || true
    open -a Stats; sleep 2
    pgrep -x Stats >/dev/null && echo "Stats RUNNING (login item registered)" || echo "Stats did not launch"
REMOTE
  echo
done
