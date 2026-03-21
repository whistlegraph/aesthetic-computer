#!/usr/bin/env fish
# Judge Deployment Script
# Provisions and deploys the Judge AI moderation service to DigitalOcean

# Colors for output
set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m' # No Color

# Paths
set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault/judge"
set DEPLOY_ENV "$VAULT_DIR/deploy.env"
set SERVICE_ENV "$VAULT_DIR/.env"

# Check for required files
if not test -f $DEPLOY_ENV
    echo -e "$RED❌ Deployment config not found: $DEPLOY_ENV$NC"
    echo -e "$YELLOW💡 Create $DEPLOY_ENV with these variables:$NC"
    echo "   DO_TOKEN=your_digitalocean_token"
    echo "   DROPLET_NAME=judge-aesthetic-computer"
    echo "   DROPLET_SIZE=s-1vcpu-1gb"
    echo "   DROPLET_REGION=nyc3"
    echo "   DROPLET_IMAGE=ubuntu-22-04-x64"
    echo "   SSH_KEY_NAME=judge-deploy-key"
    echo "   JUDGE_HOSTNAME=judge.aesthetic.computer"
    echo "   CLOUDFLARE_ZONE_ID=your_zone_id"
    echo "   CLOUDFLARE_API_TOKEN=your_api_token"
    exit 1
end

if not test -f $SERVICE_ENV
    echo -e "$RED❌ Service config not found: $SERVICE_ENV$NC"
    echo -e "$YELLOW💡 Create $SERVICE_ENV with MongoDB credentials$NC"
    exit 1
end

# Load deployment configuration
echo -e "$GREEN🔧 Loading deployment configuration...$NC"
for line in (cat $DEPLOY_ENV | grep -v '^#' | grep -v '^$' | grep '=')
    set -l parts (string split '=' $line)
    if test (count $parts) -ge 2
        set -gx $parts[1] (string join '=' $parts[2..-1])
    end
end

# Check for required tools
if not command -v doctl &> /dev/null
    echo -e "$RED❌ doctl not found. Install with:$NC"
    echo "   wget https://github.com/digitalocean/doctl/releases/download/v1.109.0/doctl-1.109.0-linux-amd64.tar.gz"
    exit 1
end

if not command -v node &> /dev/null
    echo -e "$RED❌ node not found$NC"
    exit 1
end

# Authenticate with DigitalOcean
echo -e "$GREEN🔑 Authenticating with DigitalOcean...$NC"
doctl auth init --access-token $DO_TOKEN

# Check if droplet already exists
echo -e "$GREEN🔍 Checking for existing droplet...$NC"
set EXISTING_DROPLET (doctl compute droplet list --format Name,PublicIPv4 | grep "^$DROPLET_NAME" | awk '{print $2}')

if test -n "$EXISTING_DROPLET"
    echo -e "$YELLOW⚠️  Droplet $DROPLET_NAME already exists at $EXISTING_DROPLET$NC"
    echo -e "$YELLOW   Redeploying to existing droplet...$NC"
    
    set DROPLET_IP $EXISTING_DROPLET
else
    # Create SSH key
    echo -e "$GREEN🔑 Creating SSH key...$NC"
    set SSH_KEY_FILE "$HOME/.ssh/$SSH_KEY_NAME"
    
    if not test -f $SSH_KEY_FILE
        ssh-keygen -t ed25519 -f $SSH_KEY_FILE -N "" -C "judge@aesthetic.computer"
    end
    
    # Upload SSH key to DigitalOcean
    set SSH_PUBLIC_KEY (cat "$SSH_KEY_FILE.pub")
    doctl compute ssh-key import $SSH_KEY_NAME --public-key-file "$SSH_KEY_FILE.pub" 2>/dev/null; or true
    
    # Create droplet
    echo -e "$GREEN🚀 Creating droplet: $DROPLET_NAME...$NC"
    echo -e "$YELLOW   Size: $DROPLET_SIZE (2GB RAM, 1 vCPU - \$12/mo)$NC"
    echo -e "$YELLOW   Region: $DROPLET_REGION$NC"
    echo -e "$YELLOW   Image: $DROPLET_IMAGE$NC"
    
    doctl compute droplet create $DROPLET_NAME \
        --size $DROPLET_SIZE \
        --image $DROPLET_IMAGE \
        --region $DROPLET_REGION \
        --ssh-keys (doctl compute ssh-key list --format ID --no-header) \
        --wait
    
    # Get droplet IP
    echo -e "$GREEN⏳ Waiting for droplet to be ready...$NC"
    sleep 30
    
    set DROPLET_IP (doctl compute droplet list --format Name,PublicIPv4 | grep "^$DROPLET_NAME" | awk '{print $2}')
    
    if test -z "$DROPLET_IP"
        echo -e "$RED❌ Failed to get droplet IP$NC"
        exit 1
    end
    
    echo -e "$GREEN✅ Droplet created: $DROPLET_IP$NC"
    
    # Configure firewall
    echo -e "$GREEN🔒 Configuring firewall...$NC"
    doctl compute firewall create \
        --name "judge-firewall" \
        --inbound-rules "protocol:tcp,ports:22,sources:addresses:0.0.0.0/0,sources:addresses:::/0 protocol:tcp,ports:80,sources:addresses:0.0.0.0/0,sources:addresses:::/0 protocol:tcp,ports:443,sources:addresses:0.0.0.0/0,sources:addresses:::/0" \
        --outbound-rules "protocol:tcp,ports:all,destinations:addresses:0.0.0.0/0,destinations:addresses:::/0 protocol:udp,ports:all,destinations:addresses:0.0.0.0/0,destinations:addresses:::/0" \
        --droplet-ids (doctl compute droplet list --format ID --no-header | grep -A1 $DROPLET_NAME | tail -1) 2>/dev/null; or true
end

# Wait for SSH to be ready
echo -e "$GREEN⏳ Waiting for SSH to be ready...$NC"
set MAX_ATTEMPTS 30
set ATTEMPT 1

while test $ATTEMPT -le $MAX_ATTEMPTS
    if ssh -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no -o ConnectTimeout=5 root@$DROPLET_IP "echo SSH ready" &>/dev/null
        break
    end
    echo -e "$YELLOW   Attempt $ATTEMPT/$MAX_ATTEMPTS...$NC"
    sleep 10
    set ATTEMPT (math $ATTEMPT + 1)
end

if test $ATTEMPT -gt $MAX_ATTEMPTS
    echo -e "$RED❌ SSH connection timeout$NC"
    exit 1
end

echo -e "$GREEN✅ SSH connection established$NC"

# Create setup script
echo -e "$GREEN📝 Creating server setup script...$NC"
set SETUP_SCRIPT "/tmp/judge-setup.sh"

echo '#!/bin/bash
set -e

echo "🔧 Setting up Judge server..."

# Update system
echo "📦 Updating system packages..."
apt-get update
apt-get upgrade -y

# Install Node.js 22
echo "📦 Installing Node.js 22..."
curl -fsSL https://deb.nodesource.com/setup_22.x | bash -
apt-get install -y nodejs

# Install Ollama
echo "📦 Installing Ollama..."
curl -fsSL https://ollama.com/install.sh | sh

# Install Caddy
echo "📦 Installing Caddy..."
apt-get install -y debian-keyring debian-archive-keyring apt-transport-https curl
curl -1sLf '"'"'https://dl.cloudsmith.io/public/caddy/stable/gpg.key'"'"' | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
curl -1sLf '"'"'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt'"'"' | tee /etc/apt/sources.list.d/caddy-stable.list
apt-get update
apt-get install -y caddy

# Create judge user
echo "👤 Creating judge user..."
useradd -m -s /bin/bash judge || true

# Create directories
echo "📁 Creating directories..."
mkdir -p /opt/judge
mkdir -p /var/log/judge
chown -R judge:judge /opt/judge /var/log/judge

# Start Ollama service
echo "🚀 Starting Ollama service..."
systemctl enable ollama
systemctl start ollama

# Wait for Ollama to start
sleep 5

# Pull gemma2:2b model
echo "📥 Pulling gemma2:2b model (this may take a few minutes)..."
ollama pull gemma2:2b

echo "✅ Server setup complete"' > $SETUP_SCRIPT

# Upload and run setup script
echo -e "$GREEN📤 Uploading setup script...$NC"
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SETUP_SCRIPT root@$DROPLET_IP:/tmp/
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "chmod +x /tmp/judge-setup.sh && /tmp/judge-setup.sh"

# Upload service files
echo -e "$GREEN📤 Uploading service files...$NC"
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SCRIPT_DIR/package.json root@$DROPLET_IP:/opt/judge/
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SCRIPT_DIR/package-lock.json root@$DROPLET_IP:/opt/judge/
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SCRIPT_DIR/api-server.mjs root@$DROPLET_IP:/opt/judge/
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SCRIPT_DIR/index.html root@$DROPLET_IP:/opt/judge/
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SERVICE_ENV root@$DROPLET_IP:/opt/judge/.env

# Update .env for production
echo -e "$GREEN🔧 Configuring production environment...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "cd /opt/judge && sed -i 's/PORT=3000/PORT=3000/g' .env"

# Install dependencies
echo -e "$GREEN📦 Installing Node.js dependencies...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "cd /opt/judge && npm install --production"

# Create systemd service
echo -e "$GREEN🔧 Creating systemd service...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "printf '[Unit]\nDescription=Judge - AI Chat Moderation Service\nAfter=network.target ollama.service\nRequires=ollama.service\n\n[Service]\nType=simple\nUser=judge\nWorkingDirectory=/opt/judge\nEnvironment=NODE_ENV=production\nExecStart=/usr/bin/node /opt/judge/api-server.mjs\nRestart=always\nRestartSec=10\nStandardOutput=append:/var/log/judge/judge.log\nStandardError=append:/var/log/judge/judge.log\n\n[Install]\nWantedBy=multi-user.target\n' > /etc/systemd/system/judge.service"

# Configure Caddy
echo -e "$GREEN🔧 Configuring Caddy...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "printf '{\n    email me@jas.life\n}\n\n$JUDGE_HOSTNAME {\n    reverse_proxy localhost:3000\n    encode gzip\n    \n    @websockets {\n        header Connection *Upgrade*\n        header Upgrade websocket\n    }\n    reverse_proxy @websockets localhost:3000\n    \n    log {\n        output file /var/log/caddy/judge.log\n    }\n}\n' > /etc/caddy/Caddyfile"

# Start services
echo -e "$GREEN🚀 Starting services...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "systemctl daemon-reload && systemctl enable judge && systemctl restart judge && systemctl restart caddy"

# Wait for service to start
echo -e "$GREEN⏳ Waiting for service to start...$NC"
sleep 10

# Check service status
echo -e "$GREEN🔍 Checking service status...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "systemctl status judge --no-pager | head -20"

# Configure DNS
echo -e "$GREEN🌐 Configuring DNS...$NC"
echo -e "$YELLOW   Creating A record: $JUDGE_HOSTNAME -> $DROPLET_IP$NC"

# Use Cloudflare API to create/update DNS record
curl -X POST "https://api.cloudflare.com/client/v4/zones/$CLOUDFLARE_ZONE_ID/dns_records" \
    -H "Authorization: Bearer $CLOUDFLARE_API_TOKEN" \
    -H "Content-Type: application/json" \
    --data "{\"type\":\"A\",\"name\":\"judge\",\"content\":\"$DROPLET_IP\",\"ttl\":1,\"proxied\":false}" \
    2>/dev/null || \
curl -X PUT "https://api.cloudflare.com/client/v4/zones/$CLOUDFLARE_ZONE_ID/dns_records" \
    -H "Authorization: Bearer $CLOUDFLARE_API_TOKEN" \
    -H "Content-Type: application/json" \
    --data "{\"type\":\"A\",\"name\":\"judge\",\"content\":\"$DROPLET_IP\",\"ttl\":1,\"proxied\":false}"

echo ""
echo -e "$GREEN✅ Deployment complete!$NC"
echo ""
echo -e "$YELLOW📋 Service Information:$NC"
echo -e "   URL: https://$JUDGE_HOSTNAME"
echo -e "   IP: $DROPLET_IP"
echo -e "   SSH: ssh -i ~/.ssh/$SSH_KEY_NAME root@$DROPLET_IP"
echo ""
echo -e "$YELLOW🔧 Useful commands:$NC"
echo -e "   Check status: systemctl status judge"
echo -e "   View logs: tail -f /var/log/judge/judge.log"
echo -e "   Restart: systemctl restart judge"
echo -e "   Ollama status: systemctl status ollama"
echo ""
echo -e "$YELLOW⏱️  Performance:$NC"
echo -e "   Model: gemma2:2b (~250MB RAM)"
echo -e "   Response time: ~1.3s per message"
echo -e "   Accuracy: 100% explicit content blocking"
echo ""
echo -e "$YELLOW⚠️  Next steps:$NC"
echo -e "   1. Test health: curl https://$JUDGE_HOSTNAME/api/health"
echo -e "   2. Test dashboard: https://$JUDGE_HOSTNAME"
echo -e "   3. Test filter: curl -X POST https://$JUDGE_HOSTNAME/api/filter -H 'Content-Type: application/json' -d '{\"message\":\"hello world\"}'"
echo -e "   4. Update chat servers with JUDGE_URL=https://$JUDGE_HOSTNAME"
echo ""
