#!/usr/bin/env fish
# lith Deployment Script
# Deploys the AC monolith (frontend + API) to DigitalOcean droplet

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set SERVICE_ENV "$VAULT_DIR/lith/.env"
set LITH_HOST "lith.aesthetic.computer"
set LITH_USER "root"
set REMOTE_DIR "/opt/ac"

# Check for required files
if not test -f $SSH_KEY
    echo -e "$RED x SSH key not found: $SSH_KEY$NC"
    exit 1
end

if not test -f $SERVICE_ENV
    echo -e "$RED x Service env not found: $SERVICE_ENV$NC"
    exit 1
end

# Test SSH connection
echo -e "$GREEN-> Testing SSH connection to $LITH_HOST...$NC"
if not ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=10 $LITH_USER@$LITH_HOST "echo ok" &>/dev/null
    echo -e "$RED x Cannot connect to $LITH_HOST$NC"
    exit 1
end

echo -e "$GREEN-> Connected.$NC"

# Sync repo (git pull on remote)
echo -e "$GREEN-> Pulling latest code...$NC"
ssh -i $SSH_KEY $LITH_USER@$LITH_HOST "cd $REMOTE_DIR && git pull origin main"

# Upload env
echo -e "$GREEN-> Uploading environment...$NC"
scp -i $SSH_KEY $SERVICE_ENV $LITH_USER@$LITH_HOST:$REMOTE_DIR/system/.env

# Install deps
echo -e "$GREEN-> Installing dependencies...$NC"
ssh -i $SSH_KEY $LITH_USER@$LITH_HOST "cd $REMOTE_DIR/lith && npm install && cd $REMOTE_DIR/system && npm install"

# Upload Caddyfile
echo -e "$GREEN-> Updating Caddy config...$NC"
scp -i $SSH_KEY $SCRIPT_DIR/Caddyfile $LITH_USER@$LITH_HOST:/etc/caddy/Caddyfile
ssh -i $SSH_KEY $LITH_USER@$LITH_HOST "systemctl reload caddy"

# Restart lith service
echo -e "$GREEN-> Restarting lith...$NC"
ssh -i $SSH_KEY $LITH_USER@$LITH_HOST "systemctl restart lith"

echo -e "$GREEN-> Done. lith deployed to $LITH_HOST$NC"
