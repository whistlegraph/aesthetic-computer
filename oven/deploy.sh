#!/bin/bash
# Fast oven deploy with verbose output
# Usage: ./deploy.sh [--no-restart]

set -e

OVEN_HOST="137.184.237.166"
SSH_KEY="${SSH_KEY:-$(dirname "$0")/../aesthetic-computer-vault/oven/ssh/oven-deploy-key}"
REMOTE_DIR="/opt/oven"

echo "🚀 Deploying oven..."
echo "   Host: $OVEN_HOST"
echo "   Key: $SSH_KEY"

# Get current git version for OVEN_VERSION env var
GIT_VERSION=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
echo "   Version: $GIT_VERSION"

# Time the rsync
START_TIME=$(date +%s%3N)

echo ""
echo "📦 Syncing files..."
rsync -avz --progress --delete \
  --exclude='node_modules' \
  --exclude='.git' \
  --exclude='*.log' \
  -e "ssh -i $SSH_KEY -o StrictHostKeyChecking=no" \
  "$(dirname "$0")/" \
  "root@$OVEN_HOST:$REMOTE_DIR/"

END_SYNC=$(date +%s%3N)
SYNC_TIME=$((END_SYNC - START_TIME))
echo ""
echo "✅ Sync complete in ${SYNC_TIME}ms"

# Restart unless --no-restart flag
if [ "$1" != "--no-restart" ]; then
  echo ""
  echo "🔄 Restarting oven service..."
  
  # Update OVEN_VERSION in env and restart
  ssh -i "$SSH_KEY" -o StrictHostKeyChecking=no "root@$OVEN_HOST" "
cd $REMOTE_DIR
# Update version in .env
if grep -q '^OVEN_VERSION=' .env 2>/dev/null; then
  sed -i 's/^OVEN_VERSION=.*/OVEN_VERSION=$GIT_VERSION/' .env
else
  echo 'OVEN_VERSION=$GIT_VERSION' >> .env
fi
systemctl restart oven
sleep 1
systemctl status oven --no-pager | head -5
"
  
  END_RESTART=$(date +%s%3N)
  RESTART_TIME=$((END_RESTART - END_SYNC))
  TOTAL_TIME=$((END_RESTART - START_TIME))
  
  echo ""
  echo "✅ Restart complete in ${RESTART_TIME}ms"
  echo "🏁 Total deploy time: ${TOTAL_TIME}ms"
else
  echo ""
  echo "⏭️  Skipped restart (--no-restart)"
fi

echo ""
echo "🔥 Done! https://oven.aesthetic.computer"