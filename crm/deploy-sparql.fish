#!/usr/bin/env fish
# Provision the Oxigraph SPARQL store on lith for data.aesthetic.computer/sparql.
# Idempotent — safe to re-run. Run `fish lith/deploy.fish` FIRST: it ships the
# crm/ ETL, the jsonld dependency, and the Caddyfile /sparql route.
#
#   fish crm/deploy-sparql.fish

set OXIVER 0.5.9
set ASSET "oxigraph_v{$OXIVER}_x86_64_linux_gnu"
set DL "https://github.com/oxigraph/oxigraph/releases/download/v$OXIVER/$ASSET"

set SCRIPT_DIR (dirname (status --current-filename))
set REPO_ROOT (realpath "$SCRIPT_DIR/..")
set SSH_KEY "$REPO_ROOT/../aesthetic-computer-vault/home/.ssh/id_rsa"
set HOST lith.aesthetic.computer
set LUSER root

if not test -f $SSH_KEY
    echo "x SSH key not found: $SSH_KEY"
    exit 1
end

function rmt
    ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=15 $LUSER@$HOST $argv
end

echo "-> Installing Oxigraph v$OXIVER binary…"
rmt "set -e; mkdir -p /opt/oxigraph/data; \
  if ! test -x /opt/oxigraph/oxigraph || ! /opt/oxigraph/oxigraph --version | grep -q '$OXIVER'; then \
    curl -fsSL -o /opt/oxigraph/oxigraph '$DL'; chmod +x /opt/oxigraph/oxigraph; fi; \
  /opt/oxigraph/oxigraph --version"

echo "-> Installing systemd units…"
scp -i $SSH_KEY \
  $REPO_ROOT/crm/oxigraph.service \
  $REPO_ROOT/crm/oxigraph-sync.service \
  $REPO_ROOT/crm/oxigraph-sync.timer \
  $LUSER@$HOST:/etc/systemd/system/

echo "-> Starting Oxigraph…"
rmt "systemctl daemon-reload; systemctl enable --now oxigraph.service; sleep 2; \
  systemctl is-active oxigraph.service; ss -ltn | grep 7878"

echo "-> Initial graph build (this can take a couple minutes)…"
rmt "systemctl start oxigraph-sync.service; journalctl -u oxigraph-sync.service -n 25 --no-pager"

echo "-> Enabling the 30-minute refresh timer…"
rmt "systemctl enable --now oxigraph-sync.timer; systemctl list-timers oxigraph-sync.timer --no-pager"

echo "-> Verifying triple count…"
rmt "curl -s http://127.0.0.1:7878/query --data-urlencode 'query=SELECT (COUNT(*) AS ?n) WHERE { ?s ?p ?o }' -H 'Accept: application/sparql-results+json'"

echo ""
echo "-> Done. Public endpoint: https://data.aesthetic.computer/sparql"
