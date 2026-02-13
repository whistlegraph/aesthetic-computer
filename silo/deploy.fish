#!/usr/bin/env fish
# Silo Deployment Script
# Deploys the Silo data dashboard to DigitalOcean droplet

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set SERVICE_ENV "$VAULT_DIR/silo/.env"
set SILO_HOST "silo.aesthetic.computer"
set SILO_USER "root"
set REMOTE_DIR "/opt/silo"

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
echo -e "$GREEN-> Testing SSH connection to $SILO_HOST...$NC"
if not ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=10 $SILO_USER@$SILO_HOST "echo ok" &>/dev/null
    echo -e "$RED x Cannot connect to $SILO_HOST$NC"
    exit 1
end
echo -e "$GREEN   Connected.$NC"

# Upload files
echo -e "$GREEN-> Uploading silo files...$NC"
scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $SCRIPT_DIR/server.mjs \
    $SCRIPT_DIR/package.json \
    $SCRIPT_DIR/package-lock.json \
    $SILO_USER@$SILO_HOST:$REMOTE_DIR/

# Upload production .env from vault
scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $SERVICE_ENV $SILO_USER@$SILO_HOST:$REMOTE_DIR/.env

# Fix ownership, install deps if needed, restart
echo -e "$GREEN-> Installing dependencies & restarting...$NC"
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
    chown -R silo:silo $REMOTE_DIR
    cd $REMOTE_DIR && npm install --production --silent 2>&1 | tail -1
    systemctl restart silo
    sleep 2
    systemctl is-active silo
"

set STATUS $status
if test $STATUS -eq 0
    echo -e "$GREEN   Silo is running.$NC"
else
    echo -e "$RED x Silo failed to start. Check logs:$NC"
    echo -e "$YELLOW   ssh -i $SSH_KEY $SILO_USER@$SILO_HOST journalctl -u silo -n 30$NC"
    exit 1
end

echo ""
echo -e "$GREEN Done. Silo deployed to https://$SILO_HOST$NC"
