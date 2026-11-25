#!/bin/bash
# deploy.sh — set up or update a rax satellite on a box nearer some players.
#   rax/satellite/deploy.sh <ssh-host-or-ip> <dns-name> [rooms]
#   rax/satellite/deploy.sh session-server.aesthetic.computer rax-nj.aesthetic.computer east
# The box gets Node 22 (if it has none), the satellite bundled into one file
# (rax/satellite.mjs: esbuild of rax/server.mjs and what it reads) under
# /opt/rax-satellite, a systemd unit on 127.0.0.1:8786, and a Caddy site for
# <dns-name> with an automatic certificate. The site is its own file,
# imported from the box's Caddyfile: a box already serving other names (the
# New Jersey session-server is one) keeps them. The DNS name must already
# point at the box. Re-running ships a fresh bundle and restarts it. 26.11.25
set -euo pipefail
HOST=$1; NAME=$2; ROOMS=${3:-east}
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
OUT=$(mktemp -d)
# The box's key: SSH_KEY, else the session-server's own (session-server/deploy.fish), else ssh's default.
KEY=${SSH_KEY:-$HOME/.ssh/session_server}
KEYARG=""; [ -f "$KEY" ] && KEYARG="-i $KEY"
SSH="ssh $KEYARG -o StrictHostKeyChecking=accept-new root@$HOST"
SCP="scp -q $KEYARG"

# One file: rax and everything it imports from the repo; ws and obscenity are installed on the box.
"$ROOT/system/node_modules/.bin/esbuild" "$ROOT/rax/satellite.mjs" --bundle --platform=node --format=esm \
  --target=node22 --packages=external --outfile="$OUT/satellite.mjs" --log-level=warning
COMMIT=$(git -C "$ROOT" rev-parse --short HEAD)

$SSH 'bash -s' <<REMOTE
set -e
if ss -ltnp 2>/dev/null | grep ':443 ' | grep -vq caddy; then
  echo "✕ port 443 on this box belongs to something other than Caddy; not touching it" >&2; exit 1
fi
# The box's own Node if it has one (the session-server keeps it under fnm); else Node 22 from apt.
NODEBIN=\$(command -v node || true)
[ -n "\$NODEBIN" ] || { [ -x /root/.local/share/fnm/aliases/default/bin/node ] && NODEBIN=/root/.local/share/fnm/aliases/default/bin/node; }
if [ -z "\$NODEBIN" ] || [ "\$("\$NODEBIN" -p 'process.versions.node.split(".")[0]')" -lt 22 ]; then
  export DEBIAN_FRONTEND=noninteractive
  curl -fsSL https://deb.nodesource.com/setup_22.x | bash - >/dev/null
  apt-get install -y -qq nodejs >/dev/null
  NODEBIN=/usr/bin/node
fi
export PATH="\$(dirname "\$NODEBIN"):\$PATH"
if ! command -v caddy >/dev/null; then
  export DEBIAN_FRONTEND=noninteractive
  apt-get install -y -qq debian-keyring debian-archive-keyring apt-transport-https curl gnupg >/dev/null
  curl -1sLf https://dl.cloudsmith.io/public/caddy/stable/gpg.key | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
  curl -1sLf https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt > /etc/apt/sources.list.d/caddy-stable.list
  apt-get update -qq && apt-get install -y -qq caddy >/dev/null
  : > /etc/caddy/Caddyfile
fi
mkdir -p /opt/rax-satellite/rax /opt/rax-satellite/lith /opt/rax-satellite/bin /var/lib/rax-satellite
# (A copy of the one binary: the service runs as nobody, who cannot reach into /root.)
install -m 755 "\$NODEBIN" /opt/rax-satellite/bin/node
chown nobody:nogroup /var/lib/rax-satellite
cd /opt/rax-satellite
[ -f package.json ] || echo '{"type":"module","private":true}' > package.json
[ -f lith/package.json ] || echo '{"type":"module","private":true}' > lith/package.json
[ -d node_modules/ws ] || npm install --silent ws@8 >/dev/null
[ -d node_modules/obscenity ] || npm install --silent obscenity@0.4 >/dev/null
REMOTE

$SCP "$OUT/satellite.mjs" root@$HOST:/opt/rax-satellite/rax/satellite.mjs
rm -rf "$OUT"

$SSH 'bash -s' <<REMOTE
set -e
cat > /etc/systemd/system/rax-satellite.service <<UNIT
[Unit]
Description=rax satellite ($ROOMS) $COMMIT
After=network.target
[Service]
Environment=RAX_PORT=8786 RAX_ONLY_ROOMS=$ROOMS RAX_STATE=/var/lib/rax-satellite RAX_ORIGINS=aesthetic.computer,www.aesthetic.computer,prompt.ac
WorkingDirectory=/opt/rax-satellite
ExecStart=/opt/rax-satellite/bin/node rax/satellite.mjs
Restart=always
RestartSec=2
User=nobody
[Install]
WantedBy=multi-user.target
UNIT
cat > /etc/caddy/rax-satellite.caddy <<CADDY
$NAME {
  reverse_proxy /rax/* 127.0.0.1:8786
}
CADDY
grep -q 'import /etc/caddy/rax-satellite.caddy' /etc/caddy/Caddyfile || {
  cp /etc/caddy/Caddyfile /etc/caddy/Caddyfile.before-rax-satellite
  printf '\nimport /etc/caddy/rax-satellite.caddy\n' >> /etc/caddy/Caddyfile
}
caddy validate --config /etc/caddy/Caddyfile --adapter caddyfile >/dev/null
systemctl daemon-reload
systemctl enable --now rax-satellite >/dev/null 2>&1
systemctl restart rax-satellite
systemctl reload caddy || systemctl restart caddy
sleep 2
systemctl is-active rax-satellite
journalctl -u rax-satellite -n 3 --no-pager -o cat
REMOTE
echo "→ https://$NAME/rax/health"
curl -s -m 10 "https://$NAME/rax/health" | head -c 300; echo
