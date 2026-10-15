#!/bin/bash
# deploy-relay.sh — stand up or update one oskiewar relay box.
#   session-server/relay/deploy-relay.sh <host-or-ip> <dns-name> <region>
#   session-server/relay/deploy-relay.sh 164.92.117.115 relay-sfo.aesthetic.computer sfo
# A fresh Ubuntu droplet becomes: Node 22, the relay (systemd, :8890 on
# loopback), and Caddy terminating TLS for <dns-name> with an automatic
# certificate. Re-running only re-copies the relay's four files and restarts
# it. The relay needs nothing else from the repo. 26.10.14
# The datagram lane (26.10.15): UDP 8891 for the Mac app's raw datagrams and
# 20000-20199 for geckos.io's WebRTC channels, both opened in ufw on every run
# so an existing box picks them up too.
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
ufw allow 8891/udp >/dev/null
ufw allow 20000:20199/udp >/dev/null
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
[ -d node_modules/@geckos.io/server ] || npm install --silent @geckos.io/server@3 >/dev/null
cat > /etc/systemd/system/oskiewar-relay.service <<UNIT
[Unit]
Description=oskiewar relay ($REGION)
After=network.target
[Service]
Environment=PORT=8890 RELAY_REGION=$REGION RELAY_COMMIT=${RELAY_COMMIT:-} UDP_PORT=8891 GECKOS_PORTS=20000-20199
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
# The fill bots (BOT_FILL=1, set from relays.json "bots": true): a supervisor
# that watches the one room on every relay and sends a bot in when a person
# has been alone a few seconds (xbox/tools/oskiewar-bot-fill.mjs). Bots play
# the released game straight from oskiewar.com, so they always match the
# players' build; only these four small files ship. 26.10.15
if [ "${BOT_FILL:-}" = "1" ]; then
  B=/opt/oskiewar-bots
  $SSH "mkdir -p $B/xbox/tools $B/xbox/live/tests/two-laptop /var/lib/oskiewar-bots && chown nobody:nogroup /var/lib/oskiewar-bots"
  scp -q "$ROOT/xbox/tools/oskiewar-bot-seat.mjs" "$ROOT/xbox/tools/oskiewar-bot-fill.mjs" root@$HOST:$B/xbox/tools/
  scp -q "$ROOT/xbox/live/round-room.mjs" root@$HOST:$B/xbox/live/
  scp -q "$ROOT/xbox/live/tests/two-laptop/bot-brain.mjs" root@$HOST:$B/xbox/live/tests/two-laptop/
  $SSH 'bash -s' <<REMOTE
set -e
echo '{"type":"module","private":true}' > /opt/oskiewar-bots/package.json
cat > /etc/systemd/system/oskiewar-bot-fill.service <<UNIT
[Unit]
Description=oskiewar fill bots
After=network.target
[Service]
WorkingDirectory=/opt/oskiewar-bots
ExecStart=/usr/bin/node xbox/tools/oskiewar-bot-fill.mjs --room dojo1 --after 8 --state /var/lib/oskiewar-bots --game https://oskiewar.com/oskiewar.js
Restart=always
RestartSec=5
User=nobody
[Install]
WantedBy=multi-user.target
UNIT
systemctl daemon-reload
systemctl enable oskiewar-bot-fill >/dev/null 2>&1
systemctl restart oskiewar-bot-fill
sleep 2; journalctl -u oskiewar-bot-fill -n 2 --no-pager | tail -1
REMOTE
fi
echo "deployed $NAME ($REGION) → $HOST"
