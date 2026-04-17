#!/usr/bin/env bash
# Aesthetic Computer Knot Deployment Script
# Installs a Tangled knot server on the existing PDS droplet (at.aesthetic.computer)
# Target: knot.aesthetic.computer

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
VAULT_DIR="$PROJECT_ROOT/../aesthetic-computer-vault"
INFRA_DIR="$SCRIPT_DIR/../infra"

# Knot configuration
KNOT_DOMAIN="knot.aesthetic.computer"
KNOT_PUBLIC_PORT=5555
KNOT_INTERNAL_PORT=5444
APPVIEW_ENDPOINT="https://tangled.org"
# Tag from https://tangled.org/@tangled.org/core (empty = build master HEAD)
KNOT_VERSION="${KNOT_VERSION:-v1.13.0-alpha}"

# PDS droplet — knot co-hosts here
PDS_DEPLOY_DIR="$SCRIPT_DIR/../../pds/deployment/digitalocean"
DROPLET_IP_FILE="$PDS_DEPLOY_DIR/.droplet_ip"

info()    { echo -e "${BLUE}i${NC} $1"; }
success() { echo -e "${GREEN}✓${NC} $1"; }
warning() { echo -e "${YELLOW}!${NC} $1"; }
error()   { echo -e "${RED}x${NC} $1"; exit 1; }

get_droplet_ip() {
    if [ -f "$DROPLET_IP_FILE" ]; then
        DROPLET_IP=$(cat "$DROPLET_IP_FILE")
    elif [ -n "${DROPLET_IP:-}" ]; then
        : # already set via env
    else
        error "Droplet IP not found. Set DROPLET_IP or ensure $DROPLET_IP_FILE exists."
    fi
    info "Target: $DROPLET_IP ($KNOT_DOMAIN)"
}

get_ssh_key() {
    # Try PDS key first, then default
    if [ -f ~/.ssh/aesthetic_pds ]; then
        SSH_KEY=~/.ssh/aesthetic_pds
    elif [ -f ~/.ssh/id_ed25519 ]; then
        SSH_KEY=~/.ssh/id_ed25519
    else
        error "No SSH key found. Expected ~/.ssh/aesthetic_pds or ~/.ssh/id_ed25519"
    fi
    info "SSH key: $SSH_KEY"
}

get_owner_did() {
    if [ -n "${KNOT_OWNER_DID:-}" ]; then
        info "Owner DID: $KNOT_OWNER_DID"
        return
    fi

    # Try to load from vault
    if [ -f "$VAULT_DIR/at/knot.env" ]; then
        source "$VAULT_DIR/at/knot.env"
        if [ -n "${KNOT_OWNER_DID:-}" ]; then
            info "Owner DID (from vault): $KNOT_OWNER_DID"
            return
        fi
    fi

    # Default: @aesthetic.computer DID (jeffrey)
    KNOT_OWNER_DID="did:plc:k3k3wknzkcnekbnyde4dbatz"
    info "Owner DID (aesthetic.computer): $KNOT_OWNER_DID"
}

wait_for_ssh() {
    info "Waiting for SSH..."
    for i in {1..20}; do
        if ssh -o ConnectTimeout=5 -o StrictHostKeyChecking=no -i "$SSH_KEY" "root@$DROPLET_IP" "echo ok" &>/dev/null; then
            success "SSH ready"
            return
        fi
        sleep 3
    done
    error "SSH timeout"
}

remote() {
    ssh -i "$SSH_KEY" "root@$DROPLET_IP" "$@"
}

remote_script() {
    ssh -i "$SSH_KEY" "root@$DROPLET_IP" 'bash -s' < "$1"
}

install_go() {
    info "Checking Go on remote..."
    if remote "go version" &>/dev/null; then
        success "Go already installed"
        return
    fi

    info "Installing Go..."
    remote << 'ENDSSH'
set -euo pipefail
cd /tmp
curl -sLO https://go.dev/dl/go1.24.1.linux-amd64.tar.gz
rm -rf /usr/local/go
tar -C /usr/local -xzf go1.24.1.linux-amd64.tar.gz
rm go1.24.1.linux-amd64.tar.gz

# Add to PATH for all users
echo 'export PATH=$PATH:/usr/local/go/bin' > /etc/profile.d/go.sh
export PATH=$PATH:/usr/local/go/bin
go version
ENDSSH
    success "Go installed"
}

install_build_deps() {
    info "Installing build dependencies..."
    remote << 'ENDSSH'
set -euo pipefail
apt-get update -qq
apt-get install -y -qq gcc git make sqlite3 > /dev/null
ENDSSH
    success "Build dependencies installed"
}

create_git_user() {
    info "Creating git user..."
    if remote "id git" &>/dev/null; then
        success "git user already exists"
        return
    fi

    remote << 'ENDSSH'
set -euo pipefail
adduser --disabled-password --gecos "" git
mkdir -p /home/git/.ssh
chown -R git:git /home/git
ENDSSH
    success "git user created"
}

build_knot() {
    info "Building knot binary (version: ${KNOT_VERSION:-master})..."
    remote KNOT_VERSION="$KNOT_VERSION" bash -s << 'ENDSSH'
set -euo pipefail
export PATH=$PATH:/usr/local/go/bin

cd /tmp
rm -rf tangled-core

# Clone from Tangled's own hosting
git clone https://tangled.org/@tangled.org/core tangled-core
cd tangled-core

if [ -n "${KNOT_VERSION:-}" ]; then
    git fetch --tags
    git checkout "$KNOT_VERSION"
fi

CGO_ENABLED=1 go build -o knot ./cmd/knot

# Preserve previous binary for rollback
if [ -f /usr/local/bin/knot ]; then
    cp -a /usr/local/bin/knot "/usr/local/bin/knot.prev.$(date -u +%Y%m%d-%H%M%S)"
fi

install -o root -g root -m 0755 knot /usr/local/bin/knot

rm -rf /tmp/tangled-core

/usr/local/bin/knot --help || true
ENDSSH
    success "Knot binary built and installed"
}

configure_ssh_knot() {
    info "Configuring SSH for knot..."
    remote << 'ENDSSH'
set -euo pipefail

# Configure SSH AuthorizedKeysCommand for git user
cat > /etc/ssh/sshd_config.d/tangled-knot.conf << 'SSHEOF'
Match User git
  AuthorizedKeysCommand /usr/local/bin/knot keys -o authorized-keys
  AuthorizedKeysCommandUser nobody
SSHEOF

# Reload SSH
systemctl reload ssh || systemctl reload sshd

echo "SSH configured for knot"
ENDSSH
    success "SSH configured for git user"
}

deploy_knot_env() {
    info "Deploying knot environment..."
    remote "cat > /home/git/.knot.env" << EOF
KNOT_REPO_SCAN_PATH=/home/git/repositories
KNOT_SERVER_HOSTNAME=$KNOT_DOMAIN
APPVIEW_ENDPOINT=$APPVIEW_ENDPOINT
KNOT_SERVER_OWNER=$KNOT_OWNER_DID
KNOT_SERVER_INTERNAL_LISTEN_ADDR=127.0.0.1:$KNOT_INTERNAL_PORT
KNOT_SERVER_LISTEN_ADDR=127.0.0.1:$KNOT_PUBLIC_PORT
KNOT_SERVER_DB_PATH=/home/git/database/knotserver.db
EOF

    remote << 'ENDSSH'
set -euo pipefail
chown git:git /home/git/.knot.env
chmod 600 /home/git/.knot.env
mkdir -p /home/git/repositories /home/git/database /home/git/log
chown -R git:git /home/git/repositories /home/git/database /home/git/log

# Optional MOTD
printf "aesthetic computer knot\n" > /home/git/motd
chown git:git /home/git/motd
ENDSSH
    success "Knot environment deployed"
}

deploy_systemd_service() {
    info "Deploying systemd service..."
    cat "$INFRA_DIR/knotserver.service" | remote "cat > /etc/systemd/system/knotserver.service"
    remote << 'ENDSSH'
set -euo pipefail
systemctl daemon-reload
systemctl enable knotserver
systemctl start knotserver
sleep 2
systemctl status knotserver --no-pager || true
ENDSSH
    success "Knot service running"
}

configure_caddy() {
    info "Configuring Caddy reverse proxy..."

    # Check if Caddy is managing PDS
    if remote "systemctl is-active caddy" &>/dev/null; then
        info "Caddy already running (PDS), adding knot block..."

        # Append knot config to existing Caddyfile
        remote "cat >> /etc/caddy/Caddyfile" << EOF

# Tangled Knot Server
$KNOT_DOMAIN {
    reverse_proxy localhost:$KNOT_PUBLIC_PORT

    @websocket {
        header Connection *Upgrade*
        header Upgrade websocket
    }
    reverse_proxy @websocket localhost:$KNOT_PUBLIC_PORT
}
EOF
        remote "systemctl reload caddy"
    else
        # PDS might use its own TLS (common with bluesky PDS installer)
        # Install Caddy for knot only
        info "Installing Caddy..."
        remote << 'ENDSSH'
set -euo pipefail
apt-get install -y -qq debian-keyring debian-archive-keyring apt-transport-https curl > /dev/null
curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg 2>/dev/null
curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | tee /etc/apt/sources.list.d/caddy-stable.list > /dev/null
apt-get update -qq
apt-get install -y -qq caddy > /dev/null
ENDSSH

        cat "$INFRA_DIR/Caddyfile" | remote "cat > /etc/caddy/Caddyfile"
        remote "systemctl enable caddy && systemctl restart caddy"
    fi

    success "Caddy configured for $KNOT_DOMAIN"
}

configure_dns() {
    info "Configuring DNS via Cloudflare..."

    # Write droplet IP for the DNS script
    echo "$DROPLET_IP" > "$SCRIPT_DIR/.droplet_ip"

    node "$SCRIPT_DIR/configure-dns.mjs" || error "DNS configuration failed"

    success "DNS configured"
}

verify_deployment() {
    info "Verifying deployment..."
    echo ""

    # Check systemd
    if remote "systemctl is-active knotserver" &>/dev/null; then
        success "knotserver service: active"
    else
        warning "knotserver service: not running"
        remote "journalctl -u knotserver --no-pager -n 20" || true
    fi

    # Check local port
    if remote "curl -sf http://localhost:$KNOT_PUBLIC_PORT/ > /dev/null 2>&1"; then
        success "Knot responding on localhost:$KNOT_PUBLIC_PORT"
    else
        warning "Knot not responding on localhost:$KNOT_PUBLIC_PORT yet"
    fi

    echo ""
    info "Next steps:"
    echo "  1. Verify DNS:  dig $KNOT_DOMAIN"
    echo "  2. Check HTTPS: curl https://$KNOT_DOMAIN/"
    echo "  3. Register knot at https://tangled.org/settings/knots"
    echo "  4. Add SSH key at https://tangled.org/settings/keys"
    echo "  5. Push repos:  git remote add tangled git@$KNOT_DOMAIN:aesthetic.computer/core"
    echo ""
}

main() {
    echo ""
    echo "╔════════════════════════════════════════════════════════╗"
    echo "║   Aesthetic Computer Knot Deployment (Tangled)        ║"
    echo "╚════════════════════════════════════════════════════════╝"
    echo ""

    get_droplet_ip
    get_ssh_key
    get_owner_did
    echo ""

    if [[ "${AUTO_CONFIRM:-no}" != "yes" ]]; then
        read -p "Deploy knot to $DROPLET_IP as $KNOT_DOMAIN? (y/N) " -n 1 -r
        echo ""
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            error "Cancelled"
        fi
    fi

    echo ""
    wait_for_ssh
    install_build_deps
    install_go
    create_git_user
    build_knot
    configure_ssh_knot
    deploy_knot_env
    deploy_systemd_service
    configure_dns
    configure_caddy
    echo ""
    verify_deployment

    success "Knot deployment complete!"
}

if [ $# -gt 0 ]; then
    "$@"
else
    main
fi
