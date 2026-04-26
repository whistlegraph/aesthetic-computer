#!/usr/bin/env fish
# cf-purge.fish — Purge specific URLs from the Cloudflare edge cache.
#
# Usage:
#   fish lith/scripts/cf-purge.fish <url> [<url>...]
#   fish lith/scripts/cf-purge.fish --everything    # full-zone purge (sledgehammer)
#
# Credentials are read in this order:
#   1. $CLOUDFLARE_PURGE_TOKEN + $CLOUDFLARE_ZONE_ID environment variables
#   2. aesthetic-computer-vault/lith/.env (after GPG-decryption to .env)
#   3. aesthetic-computer-vault/.devcontainer/envs/devcontainer.env (global key fallback)
#
# This script is the workstation-side companion to lith/webhook.sh's
# auto-purge — same API, different trigger. Use it when a build runs
# locally, or to clear a stale negative-cache entry without redeploying.

set SCRIPT_DIR (dirname (status --current-filename))
set REPO_ROOT (realpath "$SCRIPT_DIR/../..")
set VAULT_DIR "$REPO_ROOT/aesthetic-computer-vault"

set CF_TOKEN ""
set CF_ZONE ""
set CF_EMAIL ""
set CF_GLOBAL_KEY ""

# 1. Environment.
if set -q CLOUDFLARE_PURGE_TOKEN
    set CF_TOKEN $CLOUDFLARE_PURGE_TOKEN
end
if set -q CLOUDFLARE_ZONE_ID
    set CF_ZONE $CLOUDFLARE_ZONE_ID
end

# 2. lith vault env (decrypted on demand if needed).
if test -z "$CF_TOKEN"; and test -f "$VAULT_DIR/lith/.env"
    set token_line (rg -m1 '^CLOUDFLARE_PURGE_TOKEN=' "$VAULT_DIR/lith/.env" 2>/dev/null)
    if test -n "$token_line"
        set CF_TOKEN (string replace -r '^CLOUDFLARE_PURGE_TOKEN=' '' -- $token_line)
    end
    set zone_line (rg -m1 '^CLOUDFLARE_ZONE_ID=' "$VAULT_DIR/lith/.env" 2>/dev/null)
    if test -n "$zone_line"
        set CF_ZONE (string replace -r '^CLOUDFLARE_ZONE_ID=' '' -- $zone_line)
    end
end

# 3. Global key fallback (last resort).
if test -z "$CF_TOKEN"; and test -f "$VAULT_DIR/.devcontainer/envs/devcontainer.env"
    set key_line (rg -m1 '^CLOUDFLARE_API_KEY=' "$VAULT_DIR/.devcontainer/envs/devcontainer.env" 2>/dev/null)
    if test -n "$key_line"
        set CF_GLOBAL_KEY (string replace -r '^CLOUDFLARE_API_KEY=' '' -- $key_line)
    end
    set email_line (rg -m1 '^CLOUDFLARE_EMAIL=' "$VAULT_DIR/.devcontainer/envs/devcontainer.env" 2>/dev/null)
    if test -n "$email_line"
        set CF_EMAIL (string replace -r '^CLOUDFLARE_EMAIL=' '' -- $email_line)
    end
end

# Look up zone ID if missing but we have credentials.
if test -z "$CF_ZONE"
    if test -n "$CF_TOKEN"
        set CF_ZONE (curl -sS "https://api.cloudflare.com/client/v4/zones?name=aesthetic.computer" \
            -H "Authorization: Bearer $CF_TOKEN" | python3 -c 'import sys,json; d=json.load(sys.stdin); print(d.get("result",[{}])[0].get("id",""))')
    else if test -n "$CF_GLOBAL_KEY"; and test -n "$CF_EMAIL"
        set CF_ZONE (curl -sS "https://api.cloudflare.com/client/v4/zones?name=aesthetic.computer" \
            -H "X-Auth-Email: $CF_EMAIL" -H "X-Auth-Key: $CF_GLOBAL_KEY" | python3 -c 'import sys,json; d=json.load(sys.stdin); print(d.get("result",[{}])[0].get("id",""))')
    end
end

if test -z "$CF_ZONE"
    echo "✗ no zone ID resolvable for aesthetic.computer"
    exit 1
end

if test (count $argv) -eq 0
    echo "usage: fish lith/scripts/cf-purge.fish <url> [<url>...]"
    echo "       fish lith/scripts/cf-purge.fish --everything"
    exit 1
end

# Build curl auth flags.
set AUTH_FLAGS
if test -n "$CF_TOKEN"
    set AUTH_FLAGS -H "Authorization: Bearer $CF_TOKEN"
else if test -n "$CF_GLOBAL_KEY"; and test -n "$CF_EMAIL"
    set AUTH_FLAGS -H "X-Auth-Email: $CF_EMAIL" -H "X-Auth-Key: $CF_GLOBAL_KEY"
else
    echo "✗ no Cloudflare credentials found"
    echo "  expected one of:"
    echo "    \$CLOUDFLARE_PURGE_TOKEN  (preferred — scoped Zone.Cache Purge token)"
    echo "    aesthetic-computer-vault/lith/.env: CLOUDFLARE_PURGE_TOKEN=..."
    echo "    aesthetic-computer-vault/.devcontainer/envs/devcontainer.env: CLOUDFLARE_API_KEY + CLOUDFLARE_EMAIL"
    exit 1
end

# Build payload.
if test "$argv[1]" = "--everything"
    set PAYLOAD '{"purge_everything":true}'
    echo "→ purging EVERYTHING on zone $CF_ZONE"
else
    set PAYLOAD (printf '%s\n' $argv | python3 -c 'import sys, json; print(json.dumps({"files": [l.strip() for l in sys.stdin if l.strip()]}))')
    echo "→ purging "(count $argv)" URL(s) on zone $CF_ZONE"
    for url in $argv
        echo "  $url"
    end
end

set CF_RESPONSE (curl -sS -X POST \
    "https://api.cloudflare.com/client/v4/zones/$CF_ZONE/purge_cache" \
    $AUTH_FLAGS \
    -H "Content-Type: application/json" \
    --data "$PAYLOAD" \
    --max-time 20)

if echo "$CF_RESPONSE" | grep -q '"success":true'
    echo "✓ purged"
else
    echo "✗ purge failed: $CF_RESPONSE"
    exit 1
end
