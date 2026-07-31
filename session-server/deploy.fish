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
set -l NODE_BIN /root/.local/share/fnm/aliases/default/bin
set -l REMOTE /home/aesthetic-computer
set -l SCRIPT_DIR (path dirname (status --current-filename))
set -l REMOTE_SCRIPT $SCRIPT_DIR/deploy-remote.sh
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

# The whole remote deploy runs as one Bash script so PRE (the rollback point)
# and the health gate share state. Fish cannot parse Bash heredocs, so keep the
# remote program in its own syntax-checked file and stream it over SSH.
set -l node_bin_q (string escape -- $NODE_BIN)
set -l remote_q (string escape -- $REMOTE)
set -l ref_q (string escape -- $REF)
set -l boot_budget_q (string escape -- $BOOT_BUDGET)
ssh -i $KEY -o ConnectTimeout=15 $HOST \
    "env NODE_BIN=$node_bin_q REMOTE=$remote_q REF=$ref_q BOOT_BUDGET=$boot_budget_q bash -s" \
    < $REMOTE_SCRIPT

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
