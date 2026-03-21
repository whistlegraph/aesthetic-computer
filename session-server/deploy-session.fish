#!/usr/bin/env fish
# Deploy session server to production VPS

set SERVER "root@157.245.134.225"
set DEPLOY_PATH "/root/aesthetic-computer"

echo "🚀 Deploying session server to production..."
echo ""

# Step 1: Update code on server
echo "📥 Step 1/4: Pulling latest code..."
ssh $SERVER "cd $DEPLOY_PATH && git pull origin main"

if test $status -ne 0
    echo "❌ Failed to pull code from GitHub"
    exit 1
end

echo ""
echo "📦 Step 2/4: Installing dependencies..."
ssh $SERVER "cd $DEPLOY_PATH/session-server && npm install"

if test $status -ne 0
    echo "❌ Failed to install dependencies"
    exit 1
end

echo ""
echo "🔄 Step 3/4: Restarting session server with pm2..."
ssh $SERVER "cd $DEPLOY_PATH && pm2 restart session-server || pm2 start session-server/session.mjs --name session-server --env production"

if test $status -ne 0
    echo "❌ Failed to restart session server"
    exit 1
end

echo ""
echo "🔍 Step 4/4: Checking server status..."
sleep 2
ssh $SERVER "pm2 list && pm2 logs session-server --lines 10 --nostream"

echo ""
echo "✅ Deployment complete!"
echo ""
echo "📊 Monitor logs with: ssh $SERVER 'pm2 logs session-server'"
echo "🔄 Restart with: ssh $SERVER 'pm2 restart session-server'"
echo "📈 Check status: ssh $SERVER 'pm2 status session-server'"
