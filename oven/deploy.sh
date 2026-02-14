#!/bin/bash
# Fast oven deploy with verbose output
# Usage: ./deploy.sh [--no-restart]

set -e

OVEN_HOST="137.184.237.166"
SSH_KEY="${SSH_KEY:-$(dirname "$0")/../aesthetic-computer-vault/oven/ssh/oven-deploy-key}"
REMOTE_DIR="/opt/oven"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
AC_SOURCE="$SCRIPT_DIR/../system/public/aesthetic.computer"

echo "🚀 Deploying oven..."
echo "   Host: $OVEN_HOST"
echo "   Key: $SSH_KEY"

# Get current git version for OVEN_VERSION env var
GIT_VERSION=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
echo "   Version: $GIT_VERSION"

# Time the rsync
START_TIME=$(date +%s%3N)

echo ""
echo "📦 Syncing oven files..."
rsync -avz --progress --delete \
  --exclude='node_modules' \
  --exclude='.git' \
  --exclude='*.log' \
  --exclude='ac-source' \
  -e "ssh -i $SSH_KEY -o StrictHostKeyChecking=no" \
  "$SCRIPT_DIR/" \
  "root@$OVEN_HOST:$REMOTE_DIR/"

END_SYNC=$(date +%s%3N)
SYNC_TIME=$((END_SYNC - START_TIME))
echo ""
echo "✅ Oven sync complete in ${SYNC_TIME}ms"

# Sync aesthetic.computer source files needed for bundle generation
echo ""
echo "📦 Syncing ac-source files for bundler..."
rsync -avz --progress --delete \
  --include='*/' \
  --include='*.mjs' \
  --include='*.js' \
  --include='*.json' \
  --include='*.lisp' \
  --exclude='*' \
  -e "ssh -i $SSH_KEY -o StrictHostKeyChecking=no" \
  "$AC_SOURCE/" \
  "root@$OVEN_HOST:$REMOTE_DIR/ac-source/"

END_AC_SYNC=$(date +%s%3N)
AC_SYNC_TIME=$((END_AC_SYNC - END_SYNC))
echo ""
echo "✅ ac-source sync complete in ${AC_SYNC_TIME}ms"

# Restart unless --no-restart flag
if [ "$1" != "--no-restart" ]; then
  echo ""
  echo "🔄 Restarting oven service..."

  # Update OVEN_VERSION in both .env and systemd override, then restart
  ssh -i "$SSH_KEY" -o StrictHostKeyChecking=no "root@$OVEN_HOST" "
cd $REMOTE_DIR
# Update version in .env
if grep -q '^OVEN_VERSION=' .env 2>/dev/null; then
  sed -i 's/^OVEN_VERSION=.*/OVEN_VERSION=$GIT_VERSION/' .env
else
  echo 'OVEN_VERSION=$GIT_VERSION' >> .env
fi
# Update systemd override (takes precedence over .env)
sed -i 's/^Environment=OVEN_VERSION=.*/Environment=OVEN_VERSION=$GIT_VERSION/' /etc/systemd/system/oven.service.d/override.conf
systemctl daemon-reload
systemctl restart oven
sleep 2
systemctl status oven --no-pager | head -5
"

  END_RESTART=$(date +%s%3N)
  RESTART_TIME=$((END_RESTART - END_AC_SYNC))

  echo ""
  echo "✅ Restart complete in ${RESTART_TIME}ms"

  # Prewarm the bundle cache after restart
  echo ""
  echo "🔥 Prewarming bundle cache..."
  PREWARM_RESULT=$(ssh -i "$SSH_KEY" -o StrictHostKeyChecking=no "root@$OVEN_HOST" \
    "curl -s -X POST http://localhost:3002/bundle-prewarm --max-time 120" 2>/dev/null || echo '{"error":"prewarm timeout"}')
  echo "   $PREWARM_RESULT"

  END_PREWARM=$(date +%s%3N)
  PREWARM_TIME=$((END_PREWARM - END_RESTART))
  TOTAL_TIME=$((END_PREWARM - START_TIME))

  echo ""
  echo "✅ Prewarm complete in ${PREWARM_TIME}ms"
  echo "🏁 Total deploy time: ${TOTAL_TIME}ms"
else
  echo ""
  echo "⏭️  Skipped restart (--no-restart)"
fi

echo ""
echo "🔥 Done! https://oven.aesthetic.computer"
