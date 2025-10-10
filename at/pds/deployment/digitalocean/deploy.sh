#!/usr/bin/env bash
# Aesthetic Computer PDS Deployment Script - DigitalOcean
# This script automates the deployment of a PDS server on DigitalOcean

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
VAULT_DIR="$PROJECT_ROOT/../../aesthetic-computer-vault"

# Default values
DROPLET_NAME="aesthetic-computer-at"
DROPLET_SIZE="s-1vcpu-1gb"  # $6/month
DROPLET_IMAGE="ubuntu-24-04-x64"
DROPLET_REGION="nyc3"
SSH_KEY_NAME="aesthetic-computer"
DOMAIN="at.aesthetic.computer"
SPACES_BUCKET="at-blobs-aesthetic-computer"

# Functions

info() {
    echo -e "${BLUE}ℹ${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

error() {
    echo -e "${RED}✗${NC} $1"
    exit 1
}

check_requirements() {
    info "Checking requirements..."
    
    # Check for doctl
    if ! command -v doctl &> /dev/null; then
        error "doctl not found. Install with: brew install doctl"
    fi
    
    # Check authentication
    if ! doctl account get &> /dev/null; then
        error "doctl not authenticated. Run: doctl auth init"
    fi
    
    # Check for DO token in vault
    if [ -f "$VAULT_DIR/.devcontainer/envs/devcontainer.env" ]; then
        source "$VAULT_DIR/.devcontainer/envs/devcontainer.env"
        if [ -z "${DO_TOKEN:-}" ]; then
            warning "DO_TOKEN not found in vault, will use doctl authentication"
        fi
    fi
    
    success "Requirements check passed"
}

create_spaces_bucket() {
    info "Creating DigitalOcean Spaces bucket..."
    
    # Configure s3cmd for DigitalOcean Spaces
    cat > /tmp/s3cfg << EOF
[default]
access_key = ${SPACES_KEY}
secret_key = ${SPACES_SECRET}
host_base = ${DROPLET_REGION}.digitaloceanspaces.com
host_bucket = %(bucket)s.${DROPLET_REGION}.digitaloceanspaces.com
use_https = True
EOF
    
    # Check if space already exists
    if s3cmd -c /tmp/s3cfg ls | grep -q "s3://$SPACES_BUCKET"; then
        warning "Space '$SPACES_BUCKET' already exists, skipping creation"
        rm /tmp/s3cfg
        return 0
    fi
    
    # Create space using s3cmd
    s3cmd -c /tmp/s3cfg mb "s3://$SPACES_BUCKET"
    
    if [ $? -eq 0 ]; then
        success "Created Spaces bucket: $SPACES_BUCKET"
    else
        error "Failed to create Spaces bucket"
    fi
    
    rm /tmp/s3cfg
}

create_ssh_key() {
    info "Setting up SSH key..."
    
    # Check if key already exists in DigitalOcean
    SSH_KEY_ID=$(doctl compute ssh-key list --format ID,Name --no-header | grep "$SSH_KEY_NAME" | awk '{print $1}')
    
    if [ -n "$SSH_KEY_ID" ]; then
        success "SSH key '$SSH_KEY_NAME' already exists (ID: $SSH_KEY_ID)"
        return 0
    fi
    
    # Generate key if doesn't exist locally
    if [ ! -f ~/.ssh/aesthetic_pds ]; then
        ssh-keygen -t ed25519 -f ~/.ssh/aesthetic_pds -N "" -C "pds@aesthetic.computer"
    fi
    
    # Add to DigitalOcean
    doctl compute ssh-key import "$SSH_KEY_NAME" --public-key-file ~/.ssh/aesthetic_pds.pub
    
    # Get the key ID
    SSH_KEY_ID=$(doctl compute ssh-key list --format ID,Name --no-header | grep "$SSH_KEY_NAME" | awk '{print $1}')
    
    success "SSH key added to DigitalOcean (ID: $SSH_KEY_ID)"
}

create_droplet() {
    info "Creating droplet..."
    
    # Check if droplet already exists
    if doctl compute droplet list | grep -q "$DROPLET_NAME"; then
        warning "Droplet '$DROPLET_NAME' already exists"
        
        # Check if running non-interactively (with AUTO_CONFIRM=yes)
        if [[ "$AUTO_CONFIRM" == "yes" ]]; then
            info "Auto-confirming droplet deletion (non-interactive mode)..."
            CONFIRM="y"
        else
            read -p "Delete existing droplet and recreate? (y/N) " -n 1 -r CONFIRM
            echo  # Move to new line
        fi
        
        if [[ $CONFIRM =~ ^[Yy]$ ]]; then
            info "Deleting existing droplet..."
            DROPLET_ID=$(doctl compute droplet list --format ID,Name --no-header | grep "$DROPLET_NAME" | awk '{print $1}')
            doctl compute droplet delete "$DROPLET_ID" --force || error "Failed to delete existing droplet"
            success "Droplet deleted"
            
            # Wait for deletion to complete
            sleep 5
        else
            error "Deployment cancelled. Please use a different droplet name or delete manually."
        fi
    fi
    
    # Create the droplet
    info "Creating new droplet..."
    DROPLET_OUTPUT=$(doctl compute droplet create "$DROPLET_NAME" \
        --size "$DROPLET_SIZE" \
        --image "$DROPLET_IMAGE" \
        --region "$DROPLET_REGION" \
        --ssh-keys "$SSH_KEY_ID" \
        --enable-monitoring \
        --wait \
        --format ID,Name,PublicIPv4 \
        --no-header) || error "Failed to create droplet"
    
    DROPLET_ID=$(echo "$DROPLET_OUTPUT" | awk '{print $1}')
    DROPLET_IP=$(echo "$DROPLET_OUTPUT" | awk '{print $3}')
    
    # Save droplet info
    echo "$DROPLET_ID" > "$SCRIPT_DIR/.droplet_id"
    echo "$DROPLET_IP" > "$SCRIPT_DIR/.droplet_ip"
    
    success "Droplet created: $DROPLET_NAME ($DROPLET_IP)"
}

configure_dns() {
    info "Configuring DNS via Cloudflare API..."
    
    if [ ! -f "$SCRIPT_DIR/.droplet_ip" ]; then
        error "Droplet IP not found. Create droplet first."
    fi
    
    DROPLET_IP=$(cat "$SCRIPT_DIR/.droplet_ip")
    
    # Run the Node.js DNS configuration script
    node "$SCRIPT_DIR/configure-dns.mjs" || error "Failed to configure DNS"
    
    # Verify DNS propagation
    info "Verifying DNS propagation..."
    echo ""
    
    MAX_ATTEMPTS=30
    ATTEMPT=0
    DNS_READY=false
    
    while [ $ATTEMPT -lt $MAX_ATTEMPTS ]; do
        if host "$DOMAIN" | grep -q "$DROPLET_IP"; then
            DNS_READY=true
            break
        fi
        ATTEMPT=$((ATTEMPT + 1))
        echo -n "."
        sleep 2
    done
    
    echo ""
    
    if [ "$DNS_READY" = true ]; then
        success "DNS propagated successfully"
    else
        warning "DNS not fully propagated yet, but continuing..."
        warning "The PDS installer will wait for DNS to be ready"
    fi
}

install_pds() {
    info "Installing PDS on droplet..."
    
    if [ ! -f "$SCRIPT_DIR/.droplet_ip" ]; then
        error "Droplet IP not found. Create droplet first."
    fi
    
    DROPLET_IP=$(cat "$SCRIPT_DIR/.droplet_ip")
    
    # Wait for SSH to be ready
    info "Waiting for SSH to be ready..."
    for i in {1..30}; do
        if ssh -o ConnectTimeout=5 -o StrictHostKeyChecking=no -i ~/.ssh/aesthetic_pds "root@$DROPLET_IP" "echo 'SSH ready'" &> /dev/null; then
            success "SSH connection established"
            break
        fi
        if [ $i -eq 30 ]; then
            error "SSH connection timeout"
        fi
        sleep 10
    done
    
    # Download and run PDS installer
    ssh -i ~/.ssh/aesthetic_pds "root@$DROPLET_IP" << 'ENDSSH'
set -euo pipefail

echo "Updating system..."
apt-get update
apt-get upgrade -y

echo "Downloading PDS installer..."
wget -O /root/installer.sh https://raw.githubusercontent.com/bluesky-social/pds/main/installer.sh

echo "Running PDS installer..."
bash /root/installer.sh
ENDSSH
    
    success "PDS installed on droplet"
}

configure_pds() {
    info "Configuring PDS..."
    
    if [ ! -f "$SCRIPT_DIR/.droplet_ip" ]; then
        error "Droplet IP not found. Create droplet first."
    fi
    
    DROPLET_IP=$(cat "$SCRIPT_DIR/.droplet_ip")
    
    warning "Configuration steps:"
    echo ""
    echo "1. SSH to server: ssh -i ~/.ssh/aesthetic_pds root@$DROPLET_IP"
    echo "2. Edit /pds/pds.env with your configuration"
    echo "3. Add Spaces credentials from vault"
    echo "4. Configure SMTP settings"
    echo "5. Restart PDS: systemctl restart pds"
    echo ""
    echo "See config/pds.env.example for template"
}

create_firewall() {
    info "Creating firewall rules..."
    
    FIREWALL_NAME="pds-aesthetic-computer"
    
    # Check if firewall exists
    if doctl compute firewall list | grep -q "$FIREWALL_NAME"; then
        warning "Firewall '$FIREWALL_NAME' already exists"
        return 0
    fi
    
    # Create firewall
    doctl compute firewall create \
        --name "$FIREWALL_NAME" \
        --inbound-rules "protocol:tcp,ports:80,address:0.0.0.0/0 protocol:tcp,ports:443,address:0.0.0.0/0 protocol:tcp,ports:22,address:0.0.0.0/0" \
        --outbound-rules "protocol:tcp,ports:all,address:0.0.0.0/0 protocol:udp,ports:all,address:0.0.0.0/0"
    
    # Get droplet ID
    DROPLET_ID=$(doctl compute droplet list --format ID,Name --no-header | grep "$DROPLET_NAME" | awk '{print $1}')
    
    # Apply to droplet
    doctl compute firewall add-droplets "$FIREWALL_NAME" --droplet-ids "$DROPLET_ID"
    
    success "Firewall configured"
}

main() {
    echo ""
    echo "╔════════════════════════════════════════════════════════╗"
    echo "║   Aesthetic Computer PDS Deployment - DigitalOcean    ║"
    echo "╚════════════════════════════════════════════════════════╝"
    echo ""
    
    check_requirements
    
    echo ""
    info "Deployment configuration:"
    echo "  Droplet: $DROPLET_NAME"
    echo "  Size: $DROPLET_SIZE"
    echo "  Region: $DROPLET_REGION"
    echo "  Domain: $DOMAIN"
    echo ""
    
    if [[ "$AUTO_CONFIRM" == "yes" ]]; then
        info "Auto-confirming deployment (non-interactive mode)..."
    else
        read -p "Continue with deployment? (y/N) " -n 1 -r
        echo ""
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            error "Deployment cancelled"
        fi
    fi
    
    echo ""
    create_ssh_key
    create_spaces_bucket
    create_droplet
    create_firewall
    configure_dns
    install_pds
    configure_pds
    
    echo ""
    success "Deployment complete!"
    echo ""
    info "Next steps:"
    echo "1. SSH to server: ssh -i ~/.ssh/aesthetic_pds root@$(cat "$SCRIPT_DIR/.droplet_ip")"
    echo "2. Configure /pds/pds.env (see config/pds.env.example)"
    echo "3. Restart PDS: systemctl restart pds"
    echo "4. Create account: pdsadmin account create"
    echo "5. Verify health: curl https://$DOMAIN/xrpc/_health"
    echo ""
}

# Run main or specific function
if [ $# -gt 0 ]; then
    # Call specific function if provided
    "$@"
else
    # Run full deployment
    main
fi
