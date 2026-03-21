#!/usr/bin/env fish
# Quick redeploy script for judge service updates
# Updates only the changed files without full reprovisioning

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m' # No Color

set SSH_KEY "$HOME/.ssh/judge-deploy-key"
set DROPLET_IP "64.227.102.108"
set SCRIPT_DIR (dirname (status --current-filename))

echo -e "$GREEN🚀 Redeploying judge service...$NC"

# Upload updated files
echo -e "$YELLOW📤 Uploading files...$NC"
scp -i $SSH_KEY $SCRIPT_DIR/api-server.mjs root@$DROPLET_IP:/opt/judge/
scp -i $SSH_KEY $SCRIPT_DIR/index.html root@$DROPLET_IP:/opt/judge/

# Restart service
echo -e "$YELLOW🔄 Restarting service...$NC"
ssh -i $SSH_KEY root@$DROPLET_IP 'systemctl restart judge'

# Wait and check status
sleep 3
echo -e "$GREEN✅ Checking service status...$NC"
ssh -i $SSH_KEY root@$DROPLET_IP 'systemctl status judge --no-pager | head -10'

echo ""
echo -e "$GREEN✅ Redeploy complete!$NC"
echo -e "$YELLOW🌐 Dashboard: https://judge.aesthetic.computer/$NC"
echo -e "$YELLOW🔧 API: https://judge.aesthetic.computer/api/filter$NC"
