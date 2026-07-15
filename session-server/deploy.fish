#!/usr/bin/env fish
# Redeploy the session server — the single DigitalOcean droplet that runs chat +
# the shared realtime room (session-server.aesthetic.computer). Replaces the old
# script, which was wrong in three ways this one fixes:
#
#   1. It skipped `npm install` when node_modules already existed, so dependency
#      changes never actually deployed.
#   2. It restarted with `pkill + nohup node session.mjs`, fighting the systemd
#      unit (session-server.service) that actually manages the process.
#   3. It assumed `node` was on PATH; it isn't in a non-interactive SSH shell —
#      node lives under fnm and must be addressed by absolute path.
#
# And the reason it exists at all: the server takes ~45s to boot (it loads every
# chat instance's history from Mongo before it binds :8889), so a deploy that
# checks health too early reads a healthy-but-slow boot as a failure. This one
# waits, and — the important part — AUTO-ROLLS-BACK to the previous commit if the
# server does not come up healthy. (Learned the hard way: a redis 4->6 bump did
# not come up, and without a gate the deploy would have left chat down.)
#
# Usage:  fish session-server/deploy.fish
#         fish session-server/deploy.fish <git-ref>   # deploy a specific ref

set -l HOST root@157.245.134.225
set -l KEY $HOME/.ssh/session_server
set -l NODE_BIN /root/.local/share/fnm/node-versions/v20.17.0/installation/bin
set -l REMOTE /home/aesthetic-computer
set -l HEALTH_URL https://session-server.aesthetic.computer/
set -l BOOT_BUDGET 150   # seconds to wait for :8889 (slow history load)
set -l REF origin/main
test -n "$argv[1]"; and set REF $argv[1]

if not test -f $KEY
    echo "❌ SSH key missing at $KEY."
    echo "   From the vault:  install -m 600 aesthetic-computer-vault/session-server/session_server $KEY"
    exit 1
end

echo "🚀 Deploying session server → $HOST  (ref: $REF)"

# The whole remote deploy runs as one bash script so PRE (the rollback point) and
# the health gate share state. Prints RESULT=... on its last line.
ssh -i $KEY -o ConnectTimeout=15 $HOST "bash -s" <<REMOTE_EOF
set -uo pipefail
export PATH=$NODE_BIN:\$PATH
cd $REMOTE || { echo 'RESULT=fail:cd'; exit 1; }

PRE=\$(git rev-parse HEAD)
echo "rollback point: \$(git rev-parse --short HEAD)"

echo "→ fetch + checkout $REF"
git fetch origin --quiet || { echo 'RESULT=fail:fetch'; exit 1; }
git reset --hard $REF --quiet || { echo 'RESULT=fail:reset'; exit 1; }
echo "  now at \$(git rev-parse --short HEAD) (\$(git log -1 --format=%s | head -c 55))"

cd session-server || { echo 'RESULT=fail:cd-ss'; exit 1; }

deploy_and_check() {
  echo "→ npm ci --omit=dev"
  npm ci --omit=dev >/tmp/ss-deploy-npm.log 2>&1 || { echo '  npm ci FAILED'; tail -3 /tmp/ss-deploy-npm.log; return 1; }
  echo "→ systemctl restart session-server"
  systemctl restart session-server || return 1
  # Health gate: wait for :8889 to bind (slow boot loads chat history first).
  for i in \$(seq 1 $BOOT_BUDGET); do
    if ss -ltn 2>/dev/null | grep -q :8889; then
      echo "  ✓ bound :8889 after \${i}s"
      return 0
    fi
    if [ "\$(systemctl show session-server -p NRestarts --value)" -gt 3 ]; then
      echo "  ✗ crash-looping (NRestarts>3)"; return 1
    fi
    sleep 1
  done
  echo "  ✗ did not bind :8889 within ${BOOT_BUDGET}s"
  return 1
}

if deploy_and_check; then
  echo "RESULT=ok:\$(git rev-parse --short HEAD)"
  exit 0
fi

echo "→ ⏮  ROLLING BACK to \$(git rev-parse --short \$PRE)"
cd $REMOTE && git reset --hard \$PRE --quiet && cd session-server
if deploy_and_check; then
  echo "RESULT=rolledback:\$(git rev-parse --short \$PRE)"
else
  echo "RESULT=DOWN:rollback-also-failed"
fi
exit 1
REMOTE_EOF

set -l ssh_status $status

echo ""
echo "→ external health check: $HEALTH_URL"
set -l code (curl -sk -m 15 -o /dev/null -w "%{http_code}" $HEALTH_URL)
echo "   HTTP $code"

if test $ssh_status -eq 0; and test "$code" = "200"
    echo "✅ deployed and healthy."
else
    echo "⚠️  deploy did not end healthy (ssh=$ssh_status, http=$code) — see the RESULT line above."
    echo "   Logs:  ssh -i $KEY $HOST 'tail -40 /tmp/session-server.log'"
    exit 1
end
