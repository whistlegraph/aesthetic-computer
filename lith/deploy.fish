#!/usr/bin/env fish
# lith Deployment Script
# Deploys the AC monolith (frontend + API) to DigitalOcean droplet

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set REPO_ROOT (realpath "$SCRIPT_DIR/..")
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set SERVICE_ENV "$VAULT_DIR/lith/.env"
set LITH_USER "root"
set REMOTE_DIR "/opt/ac"
set DEFAULT_LITH_HOST "lith.aesthetic.computer"
set DEFAULT_LITH_DROPLET_NAME "ac-lith"
set TARGET_HOST $DEFAULT_LITH_HOST
set TARGET_DROPLET_NAME $DEFAULT_LITH_DROPLET_NAME

if set -q LITH_HOST
    set TARGET_HOST $LITH_HOST
end

if set -q LITH_DROPLET_NAME
    set TARGET_DROPLET_NAME $LITH_DROPLET_NAME
end

function ssh_ok --argument host
    ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=10 $LITH_USER@$host "echo ok" &>/dev/null
end

function get_do_token
    if set -q DIGITALOCEAN_ACCESS_TOKEN
        echo $DIGITALOCEAN_ACCESS_TOKEN
        return 0
    end

    if set -q DO_TOKEN
        echo $DO_TOKEN
        return 0
    end

    for token_file in \
        "$VAULT_DIR/help/deploy.env" \
        "$VAULT_DIR/judge/deploy.env" \
        "$VAULT_DIR/oven/deploy.env" \
        "$VAULT_DIR/at/deploy.env"
        if not test -f $token_file
            continue
        end

        set token_line (rg -m1 '^DO_TOKEN=' $token_file)
        if test -n "$token_line"
            string replace -r '^DO_TOKEN=' '' -- $token_line
            return 0
        end
    end

    return 1
end

function get_lith_host_from_do
    if not command -sq doctl
        return 1
    end

    set do_token (get_do_token)
    if test -z "$do_token"
        return 1
    end

    set row (env DIGITALOCEAN_ACCESS_TOKEN="$do_token" \
        doctl compute droplet list --format Name,PublicIPv4 --no-header 2>/dev/null | \
        rg "^$TARGET_DROPLET_NAME\\s")

    if test -z "$row"
        return 1
    end

    set compact_row (string replace -ra '\s+' ' ' -- (string trim -- $row))
    set fields (string split ' ' -- $compact_row)

    if test (count $fields) -lt 2
        return 1
    end

    echo $fields[2]
end

# Check for required files
if not test -f $SSH_KEY
    echo -e "$RED x SSH key not found: $SSH_KEY$NC"
    exit 1
end

if not test -f $SERVICE_ENV
    echo -e "$RED x Service env not found: $SERVICE_ENV$NC"
    exit 1
end

if not rg -q '^DEPLOY_SECRET=' $SERVICE_ENV
    echo -e "$RED x DEPLOY_SECRET missing from $SERVICE_ENV$NC"
    echo -e "$YELLOW   lith reads this file via /opt/ac/system/.env on the server.$NC"
    exit 1
end

# Test SSH connection
echo -e "$GREEN-> Testing SSH connection to $TARGET_HOST...$NC"
if not ssh_ok $TARGET_HOST
    set fallback_host (get_lith_host_from_do)

    if test -n "$fallback_host"; and test "$fallback_host" != "$TARGET_HOST"
        echo -e "$YELLOW   Falling back to DigitalOcean droplet $TARGET_DROPLET_NAME at $fallback_host.$NC"
        set TARGET_HOST $fallback_host
    end

    if not ssh_ok $TARGET_HOST
        echo -e "$RED x Cannot connect to $TARGET_HOST$NC"
        exit 1
    end
end

echo -e "$GREEN-> Connected to $TARGET_HOST.$NC"

# Sync repo (git pull on remote)
echo -e "$GREEN-> Pulling latest code...$NC"
ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR && git pull origin main"

# Overlay local working tree changes so deploys include uncommitted routing/frontend edits.
echo -e "$GREEN-> Syncing local lith/ and system/ working tree...$NC"
rsync -az --delete \
    --exclude node_modules \
    --exclude .env \
    --exclude .DS_Store \
    "$REPO_ROOT/lith/" \
    $LITH_USER@$TARGET_HOST:$REMOTE_DIR/lith/
rsync -az --delete \
    --exclude node_modules \
    --exclude .env \
    --exclude .DS_Store \
    --exclude .netlify \
    --exclude .commit-ref \
    "$REPO_ROOT/system/" \
    $LITH_USER@$TARGET_HOST:$REMOTE_DIR/system/

# Write .commit-ref AFTER rsync so it reflects the actual deployed state
echo -e "$GREEN-> Writing commit ref...$NC"
ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR && git rev-parse HEAD > system/public/.commit-ref"

# Upload env
echo -e "$GREEN-> Uploading environment...$NC"
# Note: lith.service reads EnvironmentFile=/opt/ac/system/.env, so the
# canonical vault source lives at aesthetic-computer-vault/lith/.env and is
# uploaded into system/.env on the remote host.
scp -i $SSH_KEY $SERVICE_ENV $LITH_USER@$TARGET_HOST:$REMOTE_DIR/system/.env

# Install deps
echo -e "$GREEN-> Installing dependencies...$NC"
ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR/lith && npm install && cd $REMOTE_DIR/system && npm install"

# Upload Caddyfile
echo -e "$GREEN-> Updating Caddy config...$NC"
scp -i $SSH_KEY $SCRIPT_DIR/Caddyfile $LITH_USER@$TARGET_HOST:/etc/caddy/Caddyfile
ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "systemctl reload caddy"

# Restart lith service
echo -e "$GREEN-> Restarting lith...$NC"
ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "systemctl restart lith"

echo -e "$GREEN-> Done. lith deployed to $TARGET_HOST$NC"
