#!/usr/bin/env bash
# fleet-refresh.sh — bring the shared MCP hub fresh on the fleet's Macs.
#
# The hub (slab/bin/mcp-hub.mjs) serves every server in hub-servers.json —
# analytics, crypto, x, illy, dmgify, oskiewar, jukewizard, roblox, sweep —
# from each machine's checkout. "Fresh" means three things on each host:
#   1. the checkout carries the knot's main (fast-forward only: this script
#      never merges, rebases or stashes a working tree it does not own),
#   2. the hub daemon was restarted from that code and Claude/Codex point at
#      it (toolchain/mcp/install-daemons.sh hub),
#   3. /mcp/analytics answers an `initialize` and `claude mcp list` shows it.
#
#   toolchain/mcp/fleet-refresh.sh                 blueberry and frisbee
#   toolchain/mcp/fleet-refresh.sh frisbee         one host
#   toolchain/mcp/fleet-refresh.sh --self          this machine too
#   toolchain/mcp/fleet-refresh.sh --all-daemons   every daemon, not only the hub
#
# Hosts are ssh config aliases. frisbee's login shell is fish, so the remote
# side runs under `bash -lc`. A refused fast-forward is reported, not forced:
# the hub then restarts on whatever that checkout holds, and the line says so.
set -u
HOSTS=()
SELF=0
TARGET=hub
for arg in "$@"; do
  case "$arg" in
    --self) SELF=1 ;;
    --all-daemons) TARGET="" ;;
    -h|--help) sed -n '2,22p' "$0"; exit 0 ;;
    *) HOSTS+=("$arg") ;;
  esac
done
[ ${#HOSTS[@]} -gt 0 ] || HOSTS=(blueberry frisbee)

read -r -d '' REMOTE <<'SCRIPT'
set -u
cd ~/aesthetic-computer 2>/dev/null || { echo "RESULT host=$(hostname -s) error=no-checkout"; exit 0; }
before=$(git rev-parse --short HEAD)
remote=origin
git fetch -q origin main 2>/dev/null || { remote=github; git fetch -q github main 2>/dev/null || remote=none; }
tip=$(git rev-parse --short FETCH_HEAD 2>/dev/null || echo "?")
if [ "$remote" = none ]; then ff=no-fetch
elif git merge --ff-only -q FETCH_HEAD >/dev/null 2>&1; then ff=ok
else ff=refused; fi
after=$(git rev-parse --short HEAD)
log=/tmp/mcp-fleet-refresh.log
if toolchain/mcp/install-daemons.sh $TARGET >"$log" 2>&1; then install=ok; else install=FAIL; fi
health=$(curl -s -m 5 http://127.0.0.1:7788/health 2>/dev/null | grep -c '"servers"')
init=$(curl -s -m 90 -X POST http://127.0.0.1:7788/mcp/analytics \
  -H 'Content-Type: application/json' -H 'Accept: application/json, text/event-stream' \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"fleet-refresh","version":"1"}}}' \
  2>/dev/null | grep -c '"serverInfo"')
registered=$(claude mcp list 2>/dev/null | grep -c 'analytics: http://127.0.0.1:7788/mcp/analytics')
echo "RESULT host=$(hostname -s) head=$before->$after tip=$tip($remote) ff=$ff install=$install hub=$([ "$health" -gt 0 ] && echo up || echo DOWN) analytics=$([ "$init" -gt 0 ] && echo answers || echo SILENT) claude=$([ "$registered" -gt 0 ] && echo registered || echo UNREGISTERED) log=$log"
[ "$install" = ok ] || tail -5 "$log" | sed 's/^/  | /'
SCRIPT

run_host() {
  local host="$1"
  if [ "$host" = self ]; then
    TARGET="$TARGET" bash -lc "$REMOTE" 2>&1
  else
    ssh -o ConnectTimeout=10 -o BatchMode=yes "$host" "TARGET='$TARGET' bash -lc '$(printf %s "$REMOTE" | sed "s/'/'\\\\''/g")'" 2>&1 \
      || echo "RESULT host=$host error=ssh"
  fi
}

fail=0
[ "$SELF" = 1 ] && HOSTS+=(self)
for host in "${HOSTS[@]}"; do
  echo "== $host"
  out=$(run_host "$host")
  echo "$out"
  echo "$out" | grep -q 'hub=up analytics=answers claude=registered' || fail=1
done
exit $fail
