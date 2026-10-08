#!/usr/bin/env bash
# install.sh — put link-email-identity live in the `aesthetic` tenant without
# the dashboard's Action editor (which hung twice on 2026-10-08).
#
#   bash system/backend/auth0-actions/install.sh
#
# Uses the Auth0 CLI (installed with Homebrew if missing). `auth0 login` opens
# a browser for you to approve; nothing is typed here. The three secrets are
# read straight from aesthetic-computer-vault/lith/.env — the copy production
# uses — and are never printed. Then: update the Action, deploy it, and add it
# to the post-login flow, keeping any bindings already there.

set -euo pipefail
# The CLI turns on a prompt-free "agent mode" when it thinks an AI is driving;
# the browser login below needs prompts.
export AUTH0_AGENT_MODE=false
cd "$(git rev-parse --show-toplevel)"

ACTION_NAME="link-email-identity"
CODE="system/backend/auth0-actions/link-email-identity.js"
ENV_FILE="aesthetic-computer-vault/lith/.env"
TENANT="aesthetic.us.auth0.com"
AUTH0_SDK="auth0=4.37.1" # the code calls the v4 API (usersByEmail.getByEmail, users.link)

command -v auth0 >/dev/null || brew install auth0/auth0-cli/auth0
auth0 tenants use "$TENANT" >/dev/null 2>&1 || auth0 login --domain "$TENANT"
auth0 tenants use "$TENANT"

value() { grep -E "^$1=" "$ENV_FILE" | head -1 | cut -d= -f2- | sed -e 's/^"//' -e 's/"$//'; }
CLIENT_ID="$(value AUTH0_M2M_CLIENT_ID)"
CLIENT_SECRET="$(value AUTH0_M2M_SECRET)"
[ -n "$CLIENT_ID" ] && [ -n "$CLIENT_SECRET" ] || { echo "✗ M2M credentials missing from $ENV_FILE"; exit 1; }

ID="$(auth0 actions list --json | node -e '
  let s = ""; process.stdin.on("data", d => s += d).on("end", () => {
    const list = JSON.parse(s); const hit = (Array.isArray(list) ? list : list.actions || []).find(a => a.name === process.argv[1]);
    process.stdout.write(hit?.id || "");
  });' "$ACTION_NAME")"

common=(--code "$(cat "$CODE")" --dependency "$AUTH0_SDK"
  --secret "AUTH0_DOMAIN=$TENANT"
  --secret "AUTH0_M2M_CLIENT_ID=$CLIENT_ID"
  --secret "AUTH0_M2M_SECRET=$CLIENT_SECRET")

if [ -n "$ID" ]; then
  echo "→ updating $ACTION_NAME ($ID)"
  auth0 actions update "$ID" "${common[@]}" --force --no-input >/dev/null
else
  echo "→ creating $ACTION_NAME"
  ID="$(auth0 actions create --name "$ACTION_NAME" --trigger post-login "${common[@]}" --no-input --json | node -e '
    let s = ""; process.stdin.on("data", d => s += d).on("end", () => process.stdout.write(JSON.parse(s).id));')"
fi

echo "→ waiting for the build"
for _ in $(seq 1 30); do
  STATUS="$(auth0 actions show "$ID" --json | node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>process.stdout.write(JSON.parse(s).status||""))')"
  [ "$STATUS" = "built" ] && break
  [ "$STATUS" = "failed" ] && { echo "✗ build failed"; exit 1; }
  sleep 2
done

echo "→ deploying"
auth0 actions deploy "$ID" --no-input >/dev/null

echo "→ binding to post-login (keeping existing bindings)"
BINDINGS="$(auth0 api get "actions/triggers/post-login/bindings" | node -e '
  let s = ""; process.stdin.on("data", d => s += d).on("end", () => {
    const id = process.argv[1], name = process.argv[2];
    const current = (JSON.parse(s).bindings || []).map(b => ({ ref: { type: "action_id", value: b.action.id }, display_name: b.display_name }));
    if (!current.some(b => b.ref.value === id)) current.push({ ref: { type: "action_id", value: id }, display_name: name });
    process.stdout.write(JSON.stringify({ bindings: current }));
  });' "$ID" "$ACTION_NAME")"
auth0 api patch "actions/triggers/post-login/bindings" --data "$BINDINGS" >/dev/null

echo "→ post-login flow now:"
auth0 api get "actions/triggers/post-login/bindings" | node -e '
  let s = ""; process.stdin.on("data", d => s += d).on("end", () => {
    for (const b of JSON.parse(s).bindings || []) console.log("   •", b.display_name, "→", b.action?.id);
  });'
echo "✓ $ACTION_NAME deployed and bound"
