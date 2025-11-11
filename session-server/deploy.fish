#!/usr/bin/env fish
# Deploy session server to production
# This script pulls latest code, installs dependencies, and restarts the service

set SERVER "root@157.245.134.225"
set SSH_KEY "$HOME/.ssh/session_server"
set REPO_PATH "/home/aesthetic-computer"

echo "🚀 Deploying session server to production..."
echo ""

# Step 1: Pull latest code
echo "📥 Step 1/4: Pulling latest code from GitHub..."
ssh -i $SSH_KEY $SERVER "cd $REPO_PATH && git fetch origin && git reset --hard origin/main"

if test $status -ne 0
    echo ""
    echo "❌ Failed to pull code from GitHub"
    exit 1
end

# Step 2: Install dependencies (skip if node_modules exists)
echo ""
echo "📦 Step 2/4: Checking dependencies..."
ssh -i $SSH_KEY $SERVER "cd $REPO_PATH/session-server && if [ ! -d node_modules ]; then echo 'Installing...'; npm install; else echo 'Dependencies already installed'; fi"

if test $status -ne 0
    echo ""
    echo "❌ Failed to install dependencies"
    exit 1
end

# Step 3: Restart service
echo ""
echo "🔄 Step 3/4: Restarting session server..."
ssh -i $SSH_KEY $SERVER "pkill -f 'node.*session.mjs' && sleep 2 && cd $REPO_PATH/session-server && nohup node session.mjs > /tmp/session-server.log 2>&1 &"

if test $status -ne 0
    echo ""
    echo "❌ Failed to restart session server"
    exit 1
end

# Step 4: Show logs
echo ""
echo "📊 Step 4/4: Checking server status..."
sleep 3
ssh -i $SSH_KEY $SERVER "ps aux | grep 'session.mjs' | grep -v grep && echo '' && tail -20 /tmp/session-server.log"

echo ""
echo "✅ Deployment complete!"
echo ""
echo "📡 Test the endpoints:"
echo "   curl -k https://session-server.aesthetic.computer/build-stream \\"
echo "     --header 'Content-Type: application/json' \\"
echo "     --data '{\"line\": \"Test message\"}'"
