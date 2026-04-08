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
      # Static files — Caddy serves directly from disk, no action needed
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
  log "reloading caddy..."
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

log "done"
