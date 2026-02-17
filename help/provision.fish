#!/usr/bin/env fish
# First-time provisioning for help.aesthetic.computer
# Run this once to set up the droplet after SSH access is established.
#
# Prerequisites:
#   - SSH access to 146.190.150.173 via vault key
#   - Vault .env file at aesthetic-computer-vault/help/.env

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set HELP_HOST "help.aesthetic.computer"
set HELP_USER "root"

if not test -f $SSH_KEY
    echo -e "$RED x SSH key not found: $SSH_KEY$NC"
    exit 1
end

echo -e "$GREEN-> Provisioning help.aesthetic.computer...$NC"

# Test SSH
if not ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=10 $HELP_USER@$HELP_HOST "echo ok" &>/dev/null
    echo -e "$RED x Cannot connect to $HELP_HOST$NC"
    exit 1
end

echo -e "$GREEN-> Installing Node.js 22...$NC"
ssh -i $SSH_KEY -o StrictHostKeyChecking=no $HELP_USER@$HELP_HOST '
    # Clean up any judge remnants
    systemctl stop judge 2>/dev/null || true
    systemctl disable judge 2>/dev/null || true
    systemctl stop ollama 2>/dev/null || true
    systemctl disable ollama 2>/dev/null || true
    rm -rf /opt/judge
    rm -f /etc/systemd/system/judge.service

    # Install Node.js 22
    if ! command -v node &>/dev/null || [[ "$(node --version)" != v22* ]]; then
        curl -fsSL https://deb.nodesource.com/setup_22.x | bash -
        apt-get install -y nodejs
    fi
    echo "node $(node --version)"

    # Create help user
    id help &>/dev/null || useradd --system --create-home help
    mkdir -p /opt/help
    chown help:help /opt/help
'

echo -e "$GREEN-> Installing Caddy...$NC"
ssh -i $SSH_KEY -o StrictHostKeyChecking=no $HELP_USER@$HELP_HOST '
    if ! command -v caddy &>/dev/null; then
        apt-get install -y debian-keyring debian-archive-keyring apt-transport-https curl
        curl -1sLf "https://dl.cloudsmith.io/public/caddy/stable/gpg.key" | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
        curl -1sLf "https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt" | tee /etc/apt/sources.list.d/caddy-stable.list
        apt-get update && apt-get install -y caddy
    fi
    echo "caddy $(caddy version)"

    # Caddy config
    cat > /etc/caddy/Caddyfile <<CADDY
help.aesthetic.computer {
    reverse_proxy localhost:3004
}
CADDY

    systemctl enable caddy
    systemctl restart caddy
'

echo -e "$GREEN-> Creating systemd service...$NC"
ssh -i $SSH_KEY -o StrictHostKeyChecking=no $HELP_USER@$HELP_HOST '
    cat > /etc/systemd/system/help.service <<SERVICE
[Unit]
Description=help.aesthetic.computer
After=network.target

[Service]
Type=simple
User=help
WorkingDirectory=/opt/help
ExecStart=/usr/bin/node server.mjs
Restart=always
RestartSec=5
Environment=NODE_ENV=production

[Install]
WantedBy=multi-user.target
SERVICE

    systemctl daemon-reload
    systemctl enable help
'

echo -e "$GREEN-> Provisioning complete.$NC"
echo -e "$YELLOW   Next: run deploy.fish to upload code$NC"
