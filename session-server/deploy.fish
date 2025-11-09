#!/usr/bin/env fish
# Deploy session server to production
# This script pulls latest code, installs dependencies, and restarts the service

set SERVER "root@157.245.134.225"
set SSH_KEY "$HOME/.ssh/session_server"
set REPO_PATH "/root/aesthetic-computer"

echo "🚀 Deploying session server to production..."
echo ""

# Step 1: Pull latest code
echo "📥 Step 1/4: Pulling latest code from GitHub..."
ssh -i $SSH_KEY $SERVER "cd $REPO_PATH && git pull origin main"

if test $status -ne 0
    echo ""
    echo "❌ Failed to pull code from GitHub"
    exit 1
end

# Step 2: Install dependencies
echo ""
echo "📦 Step 2/4: Installing npm dependencies..."
ssh -i $SSH_KEY $SERVER "cd $REPO_PATH/session-server && npm install"

if test $status -ne 0
    echo ""
    echo "❌ Failed to install dependencies"
    exit 1
end

# Step 3: Restart service
echo ""
echo "🔄 Step 3/4: Restarting session server with pm2..."
ssh -i $SSH_KEY $SERVER "pm2 restart session-server"

if test $status -ne 0
    echo ""
    echo "❌ Failed to restart session server"
    exit 1
end

# Step 4: Show logs
echo ""
echo "📊 Step 4/4: Checking server status..."
sleep 2
ssh -i $SSH_KEY $SERVER "pm2 list && echo '' && pm2 logs session-server --lines 10 --nostream"

echo ""
echo "✅ Deployment complete!"
echo ""
echo "📡 Test the endpoints:"
echo "   curl -k https://session-server.aesthetic.computer/build-stream \\"
echo "     --header 'Content-Type: application/json' \\"
echo "     --data '{\"line\": \"Test message\"}'"
