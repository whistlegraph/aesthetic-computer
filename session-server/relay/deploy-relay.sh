#!/bin/bash
# deploy-relay.sh — stand up or update one oskiewar relay box.
#   session-server/relay/deploy-relay.sh <host-or-ip> <dns-name> <region>
#   session-server/relay/deploy-relay.sh 164.92.117.115 relay-sfo.aesthetic.computer sfo
# A fresh Ubuntu droplet becomes: Node 22, the relay (systemd, :8890 on
# loopback), and Caddy terminating TLS for <dns-name> with an automatic
# certificate. Re-running only re-copies the relay's four files and restarts
# it. The relay needs nothing else from the repo. 26.10.14
set -euo pipefail
HOST=$1; NAME=$2; REGION=$3
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
SSH="ssh -o StrictHostKeyChecking=accept-new root@$HOST"

$SSH 'bash -s' <<REMOTE
set -e
if ! command -v node >/dev/null; then
  export DEBIAN_FRONTEND=noninteractive
  apt-get update -qq
  apt-get install -y -qq ca-certificates curl gnupg debian-keyring debian-archive-keyring apt-transport-https >/dev/null
  curl -fsSL https://deb.nodesource.com/setup_22.x | bash - >/dev/null
  apt-get install -y -qq nodejs >/dev/null
  curl -1sLf https://dl.cloudsmith.io/public/caddy/stable/gpg.key | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
  curl -1sLf https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt > /etc/apt/sources.list.d/caddy-stable.list
  apt-get update -qq && apt-get install -y -qq caddy >/dev/null
  ufw allow 22/tcp >/dev/null; ufw allow 80/tcp >/dev/null; ufw allow 443/tcp >/dev/null; ufw --force enable >/dev/null
fi
mkdir -p /opt/oskiewar-relay/session-server /opt/oskiewar-relay/shared /opt/oskiewar-relay/system/public/aesthetic.computer/lib
REMOTE

scp -q "$ROOT/session-server/oskiewar-relay.mjs" "$ROOT/session-server/oskiewar-live-manager.mjs" root@$HOST:/opt/oskiewar-relay/session-server/
scp -q "$ROOT/shared/posthog-event-capture.mjs" root@$HOST:/opt/oskiewar-relay/shared/
scp -q "$ROOT/system/public/aesthetic.computer/lib/oskiewar-analytics.mjs" root@$HOST:/opt/oskiewar-relay/system/public/aesthetic.computer/lib/

$SSH 'bash -s' <<REMOTE
set -e
cd /opt/oskiewar-relay/session-server
[ -f package.json ] || echo '{"type":"module","private":true}' > package.json
[ -d node_modules/ws ] || npm install --silent ws@8 >/dev/null
cat > /etc/systemd/system/oskiewar-relay.service <<UNIT
[Unit]
Description=oskiewar relay ($REGION)
After=network.target
[Service]
Environment=PORT=8890 RELAY_REGION=$REGION
WorkingDirectory=/opt/oskiewar-relay/session-server
ExecStart=/usr/bin/node oskiewar-relay.mjs
Restart=always
RestartSec=2
User=nobody
[Install]
WantedBy=multi-user.target
UNIT
cat > /etc/caddy/Caddyfile <<CADDY
$NAME {
  reverse_proxy 127.0.0.1:8890
}
CADDY
systemctl daemon-reload
systemctl enable --now oskiewar-relay >/dev/null 2>&1
systemctl restart oskiewar-relay
systemctl reload caddy || systemctl restart caddy
sleep 1
curl -s localhost:8890/health; echo
REMOTE
echo "deployed $NAME ($REGION) → $HOST"
