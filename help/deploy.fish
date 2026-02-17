#!/usr/bin/env fish
# help - deploy to help.aesthetic.computer
#
# Usage:
#   fish deploy.fish          # Full deploy (server + page, restarts service)
#   fish deploy.fish --page   # Page-only (uploads HTML, SIGHUP reload, zero downtime)

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set SERVICE_ENV "$VAULT_DIR/help/.env"
set HELP_HOST "help.aesthetic.computer"
set HELP_USER "root"
set REMOTE_DIR "/opt/help"

set PAGE_ONLY false
if contains -- --page $argv
    set PAGE_ONLY true
end

# Check SSH key
if not test -f $SSH_KEY
    echo -e "$RED x SSH key not found: $SSH_KEY$NC"
    exit 1
end

# Test SSH
echo -e "$GREEN-> Testing SSH to $HELP_HOST...$NC"
if not ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=10 $HELP_USER@$HELP_HOST "echo ok" &>/dev/null
    echo -e "$RED x Cannot connect to $HELP_HOST$NC"
    exit 1
end
echo -e "$GREEN   Connected.$NC"

if test $PAGE_ONLY = true
    # Page-only deploy
    echo -e "$GREEN-> Uploading index.html (zero-downtime)...$NC"
    scp -i $SSH_KEY -o StrictHostKeyChecking=no \
        $SCRIPT_DIR/index.html \
        $HELP_USER@$HELP_HOST:$REMOTE_DIR/

    ssh -i $SSH_KEY $HELP_USER@$HELP_HOST "
        chown help:help $REMOTE_DIR/index.html
        kill -HUP \$(systemctl show -p MainPID --value help)
    "
    echo -e "$GREEN   Page reloaded.$NC"
else
    # Full deploy
    echo -e "$GREEN-> Uploading help files...$NC"
    scp -i $SSH_KEY -o StrictHostKeyChecking=no \
        $SCRIPT_DIR/server.mjs \
        $SCRIPT_DIR/index.html \
        $SCRIPT_DIR/package.json \
        $HELP_USER@$HELP_HOST:$REMOTE_DIR/

    # Upload .env from vault
    if test -f $SERVICE_ENV
        scp -i $SSH_KEY -o StrictHostKeyChecking=no \
            $SERVICE_ENV $HELP_USER@$HELP_HOST:$REMOTE_DIR/.env
    else
        echo -e "$YELLOW   No vault .env found — skipping$NC"
    end

    echo -e "$GREEN-> Installing dependencies & restarting...$NC"
    ssh -i $SSH_KEY $HELP_USER@$HELP_HOST "
        chown -R help:help $REMOTE_DIR 2>/dev/null || true
        cd $REMOTE_DIR && npm install --production --silent 2>&1 | tail -1
        systemctl restart help
        sleep 2
        systemctl is-active help
    "

    set STATUS $status
    if test $STATUS -eq 0
        echo -e "$GREEN   Help is running.$NC"
    else
        echo -e "$RED x Help failed to start. Check logs:$NC"
        echo -e "$YELLOW   ssh -i $SSH_KEY $HELP_USER@$HELP_HOST journalctl -u help -n 30$NC"
        exit 1
    end
end

echo ""
echo -e "$GREEN Done. https://$HELP_HOST$NC"
