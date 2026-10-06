#!/usr/bin/env bash
# Deploy only the personal relay. Existing help/aa routes stay on port 3004.
set -euo pipefail
repo=$(cd "$(dirname "$0")/../.." && pwd)
host=${HELP_HOST:-root@help.aesthetic.computer}
key=${HELP_SSH_KEY:-$repo/aesthetic-computer-vault/home/.ssh/id_rsa}
ssh -i "$key" "$host" 'node --input-type=module' <<'JS'
const response=await fetch("http://127.0.0.1:3006/health").catch(()=>null);
if(response?.ok && (await response.json()).busy) throw Error("Aesel is working; wait for the turn before deploying");
JS
ssh -i "$key" "$host" 'mkdir -p /opt/aesel-relay/app/help/relay /opt/aesel-relay/app/aesel'
rsync -az -e "ssh -i $key" "$repo/help/relay/" "$host:/opt/aesel-relay/app/help/relay/"
rsync -az -e "ssh -i $key" "$repo/aesel/src" "$repo/aesel/context" "$repo/aesel/package.json" "$host:/opt/aesel-relay/app/aesel/"
ssh -i "$key" "$host" 'cp /opt/aesel-relay/app/help/relay/aesel-relay.service /etc/systemd/system/ && systemctl daemon-reload && systemctl enable --now aesel-relay && systemctl restart aesel-relay && curl --retry 5 --retry-connrefused --retry-delay 1 --fail -s http://127.0.0.1:3006/health'
