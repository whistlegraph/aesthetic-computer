#!/bin/bash
# deploy-relays.sh — ship every standalone oskiewar relay in relays.json and
# prove each one is serving the commit that was shipped. The New Jersey
# monolith carries the same lane and ships with session-server/deploy.fish;
# this checks it answers the relay routes too. Exit 1 if any relay is wrong.
#   session-server/relay/deploy-relays.sh            deploy + verify all
#   session-server/relay/deploy-relays.sh --verify   verify only
# 26.10.14
set -uo pipefail
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
COMMIT=$(git -C "$ROOT" rev-parse --short=10 HEAD)
VERIFY_ONLY=${1:-}
fail=0
relays=$(node -e 'for (const r of require(process.argv[1]).relays) console.log([r.region, r.host, r.kind, r.ip || ""].join(" "))' "$HERE/relays.json")
while read -r region host kind ip; do
  if [ "$kind" = "relay" ] && [ "$VERIFY_ONLY" != "--verify" ]; then
    RELAY_COMMIT=$COMMIT "$HERE/deploy-relay.sh" "$ip" "$host" "$region" >/dev/null || { echo "✗ $region: deploy failed"; fail=1; continue; }
  fi
  health=$(curl -s -m 8 "https://$host/oskiewar-room?match=ow-probe1") || health=""
  if ! echo "$health" | grep -q '"live"'; then echo "✗ $region ($host): /oskiewar-room did not answer: ${health:0:80}"; fail=1; continue; fi
  if [ "$kind" = "relay" ]; then
    served=$(curl -s -m 8 "https://$host/health" | node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>{try{console.log(JSON.parse(s).commit||"")}catch{console.log("")}})')
    if [ "$served" != "$COMMIT" ]; then echo "✗ $region ($host): serving ${served:-unknown}, expected $COMMIT"; fail=1; continue; fi
    echo "✓ $region ($host) serving $COMMIT"
  else
    echo "✓ $region ($host) answers the relay routes (monolith; its commit is session-server/deploy.fish's to verify)"
  fi
done <<< "$relays"
exit $fail
