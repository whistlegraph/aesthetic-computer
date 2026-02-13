#!/bin/bash
# Sync ac-source files to oven and prewarm the bundle cache.
# Called automatically after git push (via .git/hooks/post-push)
# or manually: ./oven/sync-source.sh

set -e

OVEN_HOST="137.184.237.166"
SSH_KEY="${SSH_KEY:-$(dirname "$0")/../aesthetic-computer-vault/oven/ssh/oven-deploy-key}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
AC_SOURCE="$SCRIPT_DIR/../system/public/aesthetic.computer"

# Only sync if SSH key exists (skip in CI or environments without vault)
if [ ! -f "$SSH_KEY" ]; then
  echo "⏭️  Skipping oven sync (no SSH key at $SSH_KEY)"
  exit 0
fi

echo "📦 Syncing ac-source to oven..."
rsync -az --delete \
  --include='*/' \
  --include='*.mjs' \
  --include='*.js' \
  --include='*.json' \
  --include='*.lisp' \
  --exclude='*' \
  -e "ssh -i $SSH_KEY -o StrictHostKeyChecking=no" \
  "$AC_SOURCE/" \
  "root@$OVEN_HOST:/opt/oven/ac-source/"

echo "🔥 Prewarming bundle cache..."
ssh -i "$SSH_KEY" -o StrictHostKeyChecking=no "root@$OVEN_HOST" \
  "curl -s -X POST http://localhost:3002/bundle-prewarm --max-time 120" 2>/dev/null || true

echo "✅ Oven bundle cache updated."
