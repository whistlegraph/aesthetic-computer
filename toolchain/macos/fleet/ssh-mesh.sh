#!/bin/bash
# ssh-mesh.sh — merge the fleet public keys (fleet-keys.pub) into a host's
# ~/.ssh/authorized_keys so every fleet Mac can reach every other. Dedups by key
# blob, never removes existing keys. Idempotent.
#
#   bash ssh-mesh.sh <host> [<host> ...]
#   bash ssh-mesh.sh all                  # neo chicken panda
#
# Note: these are all Macs. Tailscale SSH's *server* is Linux-only, so it can't be
# the mesh mechanism here — classic authorized_keys distribution is the durable path.
# Blueberry is not SSH-reachable until it has run blueberry-join.sh once; see that file.
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
KEYS="$DIR/fleet-keys.pub"
[ -f "$KEYS" ] || { echo "missing $KEYS"; exit 1; }

hosts=("$@")
[ "${hosts[0]:-}" = "all" ] && hosts=(neo chicken panda)
[ ${#hosts[@]} -eq 0 ] && { echo "usage: bash ssh-mesh.sh <host> [host...] | all"; exit 1; }

for host in "${hosts[@]}"; do
  echo "=== $host ==="
  scp -q -o ConnectTimeout=8 "$KEYS" "$host:/tmp/fleet-keys.pub"
  ssh -o ConnectTimeout=8 "$host" 'sh -s' <<'REMOTE'
    mkdir -p ~/.ssh; touch ~/.ssh/authorized_keys; chmod 700 ~/.ssh; chmod 600 ~/.ssh/authorized_keys
    before=$(grep -c . ~/.ssh/authorized_keys || echo 0)
    while read -r line; do
      [ -z "$line" ] && continue
      blob=$(echo "$line" | awk '{print $2}')
      awk '{print $2}' ~/.ssh/authorized_keys | grep -qxF "$blob" || echo "$line" >> ~/.ssh/authorized_keys
    done < /tmp/fleet-keys.pub
    rm -f /tmp/fleet-keys.pub
    echo "authorized_keys: $before -> $(grep -c . ~/.ssh/authorized_keys) keys"
REMOTE
  echo
done
