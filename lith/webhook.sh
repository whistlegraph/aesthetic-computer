#!/usr/bin/env bash
# webhook.sh — Smart auto-deploy for lith
# Called by POST /lith/deploy (from GitHub push webhook)
#
# Strategy:
#   1. git pull
#   2. Check which files changed
#   3. If only static frontend files changed → done (Caddy serves from disk)
#   4. If backend functions or lith/ changed → npm install if needed, restart lith
#   5. If Caddyfile changed → reload caddy
#
# This means most frontend pushes deploy instantly with zero downtime.

set -euo pipefail

REMOTE_DIR="/opt/ac"
LOG_TAG="[lith-deploy]"
DEPLOY_BRANCH="${DEPLOY_BRANCH:-main}"

log() { echo "$LOG_TAG $*"; }

if ! [[ "$DEPLOY_BRANCH" =~ ^[A-Za-z0-9._/-]+$ ]]; then
  log "invalid DEPLOY_BRANCH: $DEPLOY_BRANCH"
  exit 2
fi

cd "$REMOTE_DIR"

# Record HEAD before pull
OLD_HEAD=$(git rev-parse HEAD)
OLD_BRANCH=$(git branch --show-current)

# Pull latest
log "pulling branch $DEPLOY_BRANCH..."
git fetch origin "$DEPLOY_BRANCH" --quiet

# Authenticated publishers pin a deployment to the exact SHA they just pushed.
# Re-check after fetch so a racing push can never make us deploy a different
# commit than the one the maintainer reviewed.
REMOTE_HEAD=$(git rev-parse "origin/$DEPLOY_BRANCH")
if [ -n "${EXPECTED_COMMIT:-}" ] && [ "$REMOTE_HEAD" != "$EXPECTED_COMMIT" ]; then
  log "refusing deploy: origin/$DEPLOY_BRANCH is $REMOTE_HEAD, expected $EXPECTED_COMMIT"
  exit 3
fi

if git show-ref --verify --quiet "refs/heads/$DEPLOY_BRANCH"; then
  git checkout "$DEPLOY_BRANCH" --quiet
else
  git checkout -B "$DEPLOY_BRANCH" "origin/$DEPLOY_BRANCH" --quiet
fi

git reset --hard "origin/$DEPLOY_BRANCH" --quiet

NEW_HEAD=$(git rev-parse HEAD)
NEW_BRANCH=$(git branch --show-current)

if [ "$OLD_HEAD" = "$NEW_HEAD" ]; then
  log "already up to date on $NEW_BRANCH ($NEW_HEAD)"
  exit 0
fi

# Write commit ref for version endpoint
echo "$NEW_HEAD" > system/public/.commit-ref

# Get list of changed files
CHANGED=$(git diff --name-only "$OLD_HEAD" "$NEW_HEAD")
log "updated $OLD_BRANCH/$OLD_HEAD -> $NEW_BRANCH/$NEW_HEAD"
log "changed files:"
echo "$CHANGED" | sed 's/^/  /'

NEED_RESTART=false
NEED_CADDY_RELOAD=false
NEED_NPM_INSTALL=false
NEED_DP1_FEED_RESTART=false
PURGE_URLS=()

# Map a system/public/<host>/<rel> path to the public URL(s) it serves.
# papers.aesthetic.computer is also reachable as papers.prompt.ac, so emit both.
emit_urls_for() {
  local file="$1"
  case "$file" in
    system/public/papers.aesthetic.computer/*)
      local rel="${file#system/public/papers.aesthetic.computer/}"
      PURGE_URLS+=("https://papers.aesthetic.computer/${rel}")
      PURGE_URLS+=("https://papers.prompt.ac/${rel}")
      ;;
    system/public/aesthetic.computer/*)
      local rel="${file#system/public/aesthetic.computer/}"
      PURGE_URLS+=("https://aesthetic.computer/${rel}")
      ;;
    system/public/*)
      # Other subdomains — strip the host segment and emit one URL.
      local stripped="${file#system/public/}"
      local host="${stripped%%/*}"
      local rel="${stripped#*/}"
      PURGE_URLS+=("https://${host}/${rel}")
      ;;
  esac
}

while IFS= read -r file; do
  case "$file" in
    lith/server.mjs|lith/package.json)
      NEED_RESTART=true
      ;;
    lith/Caddyfile)
      NEED_CADDY_RELOAD=true
      ;;
    lith/package-lock.json)
      NEED_NPM_INSTALL=true
      NEED_RESTART=true
      ;;
    system/netlify/functions/*)
      NEED_RESTART=true
      ;;
    system/package.json|system/package-lock.json)
      NEED_NPM_INSTALL=true
      NEED_RESTART=true
      ;;
    shared/*)
      NEED_RESTART=true
      ;;
    lith/dp1-feed-config.yaml|lith/dp1-feed.service)
      NEED_DP1_FEED_RESTART=true
      ;;
    system/public/*)
      emit_urls_for "$file"
      ;;
    *)
      # Other files (docs, tests, etc.) — no action needed
      ;;
  esac
done <<< "$CHANGED"

if $NEED_NPM_INSTALL; then
  log "installing dependencies..."
  cd "$REMOTE_DIR/system" && npm install --omit=dev --quiet 2>&1 | tail -1
  cd "$REMOTE_DIR/lith" && npm install --omit=dev --quiet 2>&1 | tail -1
fi

if $NEED_CADDY_RELOAD; then
  # Caddy reads /etc/caddy/Caddyfile, not /opt/ac/lith/Caddyfile, so the
  # checked-out copy has to be installed before the reload — otherwise
  # `systemctl reload caddy` re-reads the same stale config and the change
  # silently no-ops. (deploy.fish does the same cp; webhook missed it.)
  # See: 2026-04-29 jeffrey-platter Caddy try_files fix that didn't apply.
  log "installing Caddyfile + reloading caddy..."
  cp "$REMOTE_DIR/lith/Caddyfile" /etc/caddy/Caddyfile
  systemctl reload caddy
fi

if $NEED_DP1_FEED_RESTART; then
  if systemctl is-active dp1-feed &>/dev/null; then
    log "updating dp1-feed config..."
    cp "$REMOTE_DIR/lith/dp1-feed-config.yaml" /opt/dp1-feed/config.yaml
    cp "$REMOTE_DIR/lith/dp1-feed.service" /etc/systemd/system/dp1-feed.service
    systemctl daemon-reload
    systemctl restart dp1-feed
  fi
fi

if $NEED_RESTART; then
  log "restarting lith (backend changes detected)..."
  systemctl restart lith
else
  log "static-only deploy — no restart needed"
fi

if [ ${#PURGE_URLS[@]} -gt 0 ]; then
  if [ -n "${CLOUDFLARE_PURGE_TOKEN:-}" ] && [ -n "${CLOUDFLARE_ZONE_ID:-}" ]; then
    log "purging ${#PURGE_URLS[@]} Cloudflare URL(s) on zone $CLOUDFLARE_ZONE_ID..."
    # Cloudflare's purge_cache takes up to 30 URLs per request. Chunk the list.
    chunk=()
    purge_chunk() {
      local files_json
      files_json=$(printf '%s\n' "${chunk[@]}" | python3 -c 'import sys, json; print(json.dumps({"files": [l.strip() for l in sys.stdin if l.strip()]}))')
      CF_RESPONSE=$(curl -sS -X POST \
        "https://api.cloudflare.com/client/v4/zones/${CLOUDFLARE_ZONE_ID}/purge_cache" \
        -H "Authorization: Bearer ${CLOUDFLARE_PURGE_TOKEN}" \
        -H "Content-Type: application/json" \
        --data "$files_json" \
        --max-time 20 || echo '{"success":false,"errors":[{"message":"curl failed"}]}')
      if echo "$CF_RESPONSE" | grep -q '"success":true'; then
        log "  purged ${#chunk[@]} URL(s)"
      else
        log "  WARN: purge failed: $CF_RESPONSE"
      fi
    }
    for url in "${PURGE_URLS[@]}"; do
      chunk+=("$url")
      if [ ${#chunk[@]} -ge 30 ]; then
        purge_chunk
        chunk=()
      fi
    done
    if [ ${#chunk[@]} -gt 0 ]; then
      purge_chunk
    fi
  else
    log "skipping CF purge of ${#PURGE_URLS[@]} URL(s) — CLOUDFLARE_PURGE_TOKEN / CLOUDFLARE_ZONE_ID not set"
    log "  set them in aesthetic-computer-vault/lith/.env (uploaded to /opt/ac/system/.env on deploy)"
  fi
fi

log "done"
