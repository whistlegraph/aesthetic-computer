#!/usr/bin/env bash
# social.sh — switch on "Continue with Google / Apple" for aesthetic.computer.
#
#   bash system/backend/auth0-actions/social.sh
#
# Reads provider credentials from aesthetic-computer-vault/auth0/social.env
# (never printed), creates or updates the `google-oauth2` and `apple` Auth0
# connections, and enables them for the aesthetic SPA. Providers whose keys
# are missing from the file are skipped, so Google can go live before Apple.
#
# social.env:
#   GOOGLE_CLIENT_ID=…apps.googleusercontent.com
#   GOOGLE_CLIENT_SECRET=…
#   APPLE_SERVICES_ID=computer.aesthetic.signin   # the Services ID, used as client_id
#   APPLE_TEAM_ID=…
#   APPLE_KEY_ID=…                                # the Sign in with Apple key's id
#   APPLE_KEY_FILE=aesthetic-computer-vault/apple/AuthKey_<KEY_ID>.p8
#
# Provider-side settings (both providers):
#   Google  · Authorized JavaScript origin  https://hi.aesthetic.computer
#           · Authorized redirect URI       https://hi.aesthetic.computer/login/callback
#   Apple   · Services ID domain            hi.aesthetic.computer
#           · Return URL                    https://hi.aesthetic.computer/login/callback
#
# After it runs, drop the `ac:social` preview gate in boot.mjs so the buttons
# show for everyone. Linking to existing accounts is already handled by the
# link-email-identity Action (LINKABLE_CONNECTIONS).

set -euo pipefail
export AUTH0_AGENT_MODE=false
cd "$(git rev-parse --show-toplevel)"

TENANT="aesthetic.us.auth0.com"
SPA_CLIENT="LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt"
ENV_FILE="aesthetic-computer-vault/auth0/social.env"
[ -f "$ENV_FILE" ] || { echo "✗ $ENV_FILE not found (see the header of this script)"; exit 1; }

command -v auth0 >/dev/null || brew install auth0/auth0-cli/auth0
auth0 tenants use "$TENANT" >/dev/null 2>&1 || auth0 login --domain "$TENANT"
auth0 tenants use "$TENANT" >/dev/null

value() { grep -E "^$1=" "$ENV_FILE" | head -1 | cut -d= -f2- | sed -e 's/^"//' -e 's/"$//'; }

connection_id() {
  auth0 api get "connections" --query "name=$1" --query "fields=id" --no-input 2>/dev/null |
    node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>{try{process.stdout.write(JSON.parse(s)[0]?.id||"")}catch{}})'
}

# Create the connection or update its options, then enable it for the SPA.
upsert() { # name strategy options-json
  local name="$1" strategy="$2" options="$3" id
  id="$(connection_id "$name")"
  if [ -z "$id" ]; then
    echo "→ creating $name"
    id="$(node -e 'process.stdout.write(JSON.stringify({ name: process.argv[1], strategy: process.argv[2], options: JSON.parse(process.argv[3]) }))' "$name" "$strategy" "$options" |
      auth0 api post "connections" --no-input 2>/dev/null |
      node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>process.stdout.write(JSON.parse(s).id))')"
  else
    echo "→ updating $name ($id)"
    node -e 'process.stdout.write(JSON.stringify({ options: JSON.parse(process.argv[1]) }))' "$options" |
      auth0 api patch "connections/$id" --no-input >/dev/null 2>&1
  fi
  auth0 api patch "connections/$id/clients" --data "[{\"client_id\":\"$SPA_CLIENT\",\"status\":true}]" --no-input >/dev/null 2>&1
  echo "  ✓ $name enabled for the aesthetic app"
}

GOOGLE_ID="$(value GOOGLE_CLIENT_ID)"; GOOGLE_SECRET="$(value GOOGLE_CLIENT_SECRET)"
if [ -n "$GOOGLE_ID" ] && [ -n "$GOOGLE_SECRET" ]; then
  upsert google-oauth2 google-oauth2 "$(node -e 'process.stdout.write(JSON.stringify({ client_id: process.argv[1], client_secret: process.argv[2], email: true, profile: true, scope: ["email", "profile"] }))' "$GOOGLE_ID" "$GOOGLE_SECRET")"
else
  echo "· google skipped (no GOOGLE_CLIENT_ID / GOOGLE_CLIENT_SECRET)"
fi

APPLE_ID="$(value APPLE_SERVICES_ID)"; APPLE_TEAM="$(value APPLE_TEAM_ID)"; APPLE_KID="$(value APPLE_KEY_ID)"; APPLE_KEY="$(value APPLE_KEY_FILE)"
if [ -n "$APPLE_ID" ] && [ -n "$APPLE_TEAM" ] && [ -n "$APPLE_KID" ] && [ -f "$APPLE_KEY" ]; then
  upsert apple apple "$(node -e 'const fs=require("fs");process.stdout.write(JSON.stringify({ client_id: process.argv[1], team_id: process.argv[2], kid: process.argv[3], app_secret: fs.readFileSync(process.argv[4], "utf8"), name: true, email: true, scope: "name email" }))' "$APPLE_ID" "$APPLE_TEAM" "$APPLE_KID" "$APPLE_KEY")"
else
  echo "· apple skipped (needs APPLE_SERVICES_ID, APPLE_TEAM_ID, APPLE_KEY_ID and APPLE_KEY_FILE)"
fi
