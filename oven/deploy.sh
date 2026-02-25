#!/bin/bash
# Fast oven deploy with verbose output
# Usage: ./deploy.sh [--no-restart]

set -e

OVEN_HOST="137.184.237.166"
SSH_KEY="${SSH_KEY:-$(dirname "$0")/../aesthetic-computer-vault/oven/ssh/oven-deploy-key}"
REMOTE_DIR="/opt/oven"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
AC_SOURCE="$SCRIPT_DIR/../system/public/aesthetic.computer"
FEDAC_SOURCE="$SCRIPT_DIR/../fedac"
VAULT_OS_KEY="$SCRIPT_DIR/../aesthetic-computer-vault/oven/os-build-admin-key.txt"

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

# Sync fedac scripts/overlays used by background OS base-image build jobs
echo ""
echo "📦 Syncing fedac OS build pipeline..."
rsync -avz --progress --delete \
  --exclude='.git' \
  --exclude='*.img' \
  --exclude='*.iso' \
  --exclude='*.qcow2' \
  --exclude='*.log' \
  -e "ssh -i $SSH_KEY -o StrictHostKeyChecking=no" \
  "$FEDAC_SOURCE/" \
  "root@$OVEN_HOST:$REMOTE_DIR/fedac/"

END_FEDAC_SYNC=$(date +%s%3N)
FEDAC_SYNC_TIME=$((END_FEDAC_SYNC - END_AC_SYNC))
echo ""
echo "✅ fedac sync complete in ${FEDAC_SYNC_TIME}ms"

# Optional vault-managed admin key for /os-base-build endpoints
echo ""
if [ -f "$VAULT_OS_KEY" ]; then
  echo "🔐 Syncing OS build admin key from vault..."
  ssh -i "$SSH_KEY" -o StrictHostKeyChecking=no "root@$OVEN_HOST" "mkdir -p $REMOTE_DIR/secrets && chmod 700 $REMOTE_DIR/secrets"
  rsync -avz --progress \
    -e "ssh -i $SSH_KEY -o StrictHostKeyChecking=no" \
    "$VAULT_OS_KEY" \
    "root@$OVEN_HOST:$REMOTE_DIR/secrets/os-build-admin-key.txt"
  ssh -i "$SSH_KEY" -o StrictHostKeyChecking=no "root@$OVEN_HOST" "
chmod 600 $REMOTE_DIR/secrets/os-build-admin-key.txt
if grep -q '^OS_BUILD_ADMIN_KEY_FILE=' $REMOTE_DIR/.env 2>/dev/null; then
  sed -i 's|^OS_BUILD_ADMIN_KEY_FILE=.*|OS_BUILD_ADMIN_KEY_FILE=$REMOTE_DIR/secrets/os-build-admin-key.txt|' $REMOTE_DIR/.env
else
  echo 'OS_BUILD_ADMIN_KEY_FILE=$REMOTE_DIR/secrets/os-build-admin-key.txt' >> $REMOTE_DIR/.env
fi
"
  echo "✅ OS build admin key synced"
else
  echo "⚠️  No vault key at $VAULT_OS_KEY (skipping OS_BUILD_ADMIN_KEY_FILE provisioning)"
fi

END_SECRET_SYNC=$(date +%s%3N)
SECRET_SYNC_TIME=$((END_SECRET_SYNC - END_FEDAC_SYNC))
echo "✅ Secret sync stage complete in ${SECRET_SYNC_TIME}ms"

# Sync BDF font files + glyph caches for bundle font embedding
echo ""
echo "📦 Syncing font assets (BDF + glyph caches)..."
rsync -avz --progress \
  --include='*/' \
  --include='*.bdf' \
  --include='*.bdf.gz' \
  --include='*.json' \
  --exclude='*' \
  -e "ssh -i $SSH_KEY -o StrictHostKeyChecking=no" \
  "$SCRIPT_DIR/../system/public/assets/type/" \
  "root@$OVEN_HOST:$REMOTE_DIR/assets-type/"

END_FONT_SYNC=$(date +%s%3N)
FONT_SYNC_TIME=$((END_FONT_SYNC - END_SECRET_SYNC))
echo ""
echo "✅ Font glyph sync complete in ${FONT_SYNC_TIME}ms"

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
  RESTART_TIME=$((END_RESTART - END_FONT_SYNC))

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
