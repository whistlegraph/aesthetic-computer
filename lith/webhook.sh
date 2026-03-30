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

log() { echo "$LOG_TAG $*"; }

cd "$REMOTE_DIR"

# Record HEAD before pull
OLD_HEAD=$(git rev-parse HEAD)

# Pull latest
log "pulling..."
git fetch origin main --quiet
git reset --hard origin/main --quiet

NEW_HEAD=$(git rev-parse HEAD)

if [ "$OLD_HEAD" = "$NEW_HEAD" ]; then
  log "already up to date ($NEW_HEAD)"
  exit 0
fi

# Write commit ref for version endpoint
echo "$NEW_HEAD" > system/public/.commit-ref

# Get list of changed files
CHANGED=$(git diff --name-only "$OLD_HEAD" "$NEW_HEAD")
log "updated $OLD_HEAD -> $NEW_HEAD"
log "changed files:"
echo "$CHANGED" | sed 's/^/  /'

NEED_RESTART=false
NEED_CADDY_RELOAD=false
NEED_NPM_INSTALL=false

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

if $NEED_RESTART; then
  log "restarting lith (backend changes detected)..."
  systemctl restart lith
else
  log "static-only deploy — no restart needed"
fi

log "done"
