#!/usr/bin/env fish
# Deploy dp1-feed (SQLite mode) to silo.aesthetic.computer
#
# Usage:
#   fish deploy-silo.fish            # Full deploy (build, upload, restart)
#   fish deploy-silo.fish --landing  # Landing page only (no restart)
#   fish deploy-silo.fish --setup    # First-time setup (create dirs, systemd service, env)

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set SILO_HOST "silo.aesthetic.computer"
set SILO_USER "root"
set REMOTE_DIR "/opt/dp1-feed"
set DP1_DIR "$SCRIPT_DIR/dp1-feed"

set SETUP false
set LANDING_ONLY false
if contains -- --setup $argv
    set SETUP true
end
if contains -- --landing $argv
    set LANDING_ONLY true
end

# Check for SSH key
if not test -f $SSH_KEY
    echo -e "$RED x SSH key not found: $SSH_KEY$NC"
    exit 1
end

# Test SSH connection
echo -e "$GREEN-> Testing SSH connection to $SILO_HOST...$NC"
if not ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=10 $SILO_USER@$SILO_HOST "echo ok" &>/dev/null
    echo -e "$RED x Cannot connect to $SILO_HOST$NC"
    exit 1
end
echo -e "$GREEN   Connected.$NC"

if test $LANDING_ONLY = true
    echo -e "$GREEN-> Uploading landing-page.html (no restart)...$NC"
    scp -i $SSH_KEY -o StrictHostKeyChecking=no \
        $SCRIPT_DIR/landing-page.html \
        $SILO_USER@$SILO_HOST:$REMOTE_DIR/
    ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "chown dp1feed:dp1feed $REMOTE_DIR/landing-page.html"
    echo -e "$GREEN   Landing page deployed.$NC"
    exit 0
end

if test $SETUP = true
    # First-time setup: create dirs, systemd service, generate keys
    echo -e "$GREEN-> First-time setup on $SILO_HOST...$NC"

    ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
        # Create app directory and user
        mkdir -p $REMOTE_DIR/data
        id -u dp1feed &>/dev/null || useradd -r -s /bin/false dp1feed
        chown -R dp1feed:dp1feed $REMOTE_DIR

        # Install Node.js 22 if not present
        if ! command -v node &>/dev/null || [ \$(node -v | cut -d. -f1 | tr -d v) -lt 22 ]; then
            curl -fsSL https://deb.nodesource.com/setup_22.x | bash -
            apt-get install -y nodejs
        fi
        echo 'Node:' \$(node -v)

        # Install build tools for better-sqlite3
        apt-get install -y python3 make g++ 2>/dev/null

        # Create systemd service
        cat > /etc/systemd/system/dp1-feed.service << 'UNIT'
[Unit]
Description=DP-1 Feed Operator API (SQLite)
After=network.target

[Service]
Type=simple
User=dp1feed
WorkingDirectory=/opt/dp1-feed
ExecStart=/usr/bin/node dist/server-sqlite.js
EnvironmentFile=/opt/dp1-feed/.env
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
UNIT

        systemctl daemon-reload
        systemctl enable dp1-feed
        echo 'Systemd service created and enabled.'
    "

    # Read API secret from vault (shared with silo dashboard)
    echo -e "$GREEN-> Generating .env...$NC"
    set VAULT_FEED_ENV "$VAULT_DIR/feed/.env"
    if test -f $VAULT_FEED_ENV
        set API_SECRET (grep '^FEED_API_SECRET=' $VAULT_FEED_ENV | sed 's/^FEED_API_SECRET=//')
    end
    if test -z "$API_SECRET"
        set API_SECRET (openssl rand -hex 32)
        echo -e "$YELLOW   No vault secret found, generated random API_SECRET$NC"
    end
    set ED25519_KEY (node -e "
        const { generateKeyPairSync } = require('crypto');
        const kp = generateKeyPairSync('ed25519');
        const der = kp.privateKey.export({ type: 'pkcs8', format: 'der' });
        console.log(Buffer.from(der).toString('hex'));
    " 2>/dev/null; or echo "302e020100300506032b6570042204205e42cad90e34efb36d84b8dbbcf15777ac33f4126a80c087cdedfb030138ac6f")

    ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "cat > $REMOTE_DIR/.env << EOF
API_SECRET=$API_SECRET
ED25519_PRIVATE_KEY=$ED25519_KEY
SQLITE_DB_PATH=/opt/dp1-feed/data/dp1-feed.db
ENVIRONMENT=sqlite
SELF_HOSTED_DOMAINS=silo.aesthetic.computer:8787,localhost:8787
PORT=8787
EOF
    chown dp1feed:dp1feed $REMOTE_DIR/.env
    chmod 600 $REMOTE_DIR/.env"

    echo -e "$GREEN   Setup complete. Run 'fish deploy-silo.fish' to deploy.$NC"
    exit 0
end

# Regular deploy: build locally, upload, restart
echo -e "$GREEN-> Building dp1-feed (SQLite)...$NC"
cd $DP1_DIR
npm run sqlite:build 2>&1 | tail -1

if test $status -ne 0
    echo -e "$RED x Build failed$NC"
    exit 1
end

echo -e "$GREEN-> Uploading to $SILO_HOST...$NC"
ssh -i $SSH_KEY -o StrictHostKeyChecking=no $SILO_USER@$SILO_HOST "mkdir -p $REMOTE_DIR/dist"
scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $DP1_DIR/dist/server-sqlite.js \
    $SILO_USER@$SILO_HOST:$REMOTE_DIR/dist/
scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $DP1_DIR/package.json \
    $DP1_DIR/package-lock.json \
    $SILO_USER@$SILO_HOST:$REMOTE_DIR/

# Upload landing page
echo -e "$GREEN-> Uploading landing page...$NC"
scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $SCRIPT_DIR/landing-page.html \
    $SILO_USER@$SILO_HOST:$REMOTE_DIR/

echo -e "$GREEN-> Installing dependencies & restarting...$NC"
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
    chown -R dp1feed:dp1feed $REMOTE_DIR
    cd $REMOTE_DIR && npm install --production --silent 2>&1 | tail -1
    systemctl restart dp1-feed
    sleep 2
    systemctl is-active dp1-feed
"

set STATUS $status
if test $STATUS -eq 0
    echo -e "$GREEN   dp1-feed is running.$NC"

    # Quick health check
    sleep 1
    ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "curl -s http://localhost:8787/api/v1/health"
    echo ""
else
    echo -e "$RED x dp1-feed failed to start. Check logs:$NC"
    echo -e "$YELLOW   ssh -i $SSH_KEY $SILO_USER@$SILO_HOST journalctl -u dp1-feed -n 30$NC"
    exit 1
end

echo ""
echo -e "$GREEN Done. dp1-feed deployed to $SILO_HOST:8787$NC"
