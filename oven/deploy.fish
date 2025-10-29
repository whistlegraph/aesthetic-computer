#!/usr/bin/env fish
# Oven Deployment Script
#!/usr/bin/env fish
# Oven Deployment Script
# Provisions and deploys the Oven video processing service to DigitalOcean

# Colors for output
set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m' # No Color

# Paths
set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault/oven"
set DEPLOY_ENV "$VAULT_DIR/deploy.env"
set SERVICE_ENV "$VAULT_DIR/.env"

# Check for required files
if not test -f $DEPLOY_ENV
    echo -e "$RED❌ Deployment config not found: $DEPLOY_ENV$NC"
    exit 1
end

if not test -f $SERVICE_ENV
    echo -e "$RED❌ Service config not found: $SERVICE_ENV$NC"
    exit 1
end

# Load deployment configuration (parse bash-style env file)
echo -e "$GREEN🔧 Loading deployment configuration...$NC"
for line in (cat $DEPLOY_ENV | grep -v '^#' | grep -v '^$' | grep '=')
    set -l parts (string split '=' $line)
    if test (count $parts) -ge 2
        set -gx $parts[1] (string join '=' $parts[2..-1])
    end
end

set -e

# Colors for output
set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m' # No Color

# Paths
set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault/oven"
set DEPLOY_ENV "$VAULT_DIR/deploy.env"
set SERVICE_ENV "$VAULT_DIR/.env"

# Check for required files
if not test -f $DEPLOY_ENV
    echo -e "$RED❌ Deployment config not found: $DEPLOY_ENV$NC"
    exit 1
end

if not test -f $SERVICE_ENV
    echo -e "$RED❌ Service config not found: $SERVICE_ENV$NC"
    exit 1
end

# Load deployment configuration
echo -e "$GREEN🔧 Loading deployment configuration...$NC"
source $DEPLOY_ENV

# Check for required tools
if not command -v doctl &> /dev/null
    echo -e "$RED❌ doctl not found. Install with: wget https://github.com/digitalocean/doctl/releases/download/v1.109.0/doctl-1.109.0-linux-amd64.tar.gz$NC"
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
    echo -e "$YELLOW   Would you like to redeploy to the existing droplet? (y/n)$NC"
    read -l CONFIRM
    
    if test "$CONFIRM" != "y"
        echo -e "$RED❌ Deployment cancelled$NC"
        exit 1
    end
    
    set DROPLET_IP $EXISTING_DROPLET
else
    # Create SSH key
    echo -e "$GREEN🔑 Creating SSH key...$NC"
    set SSH_KEY_FILE "$HOME/.ssh/$SSH_KEY_NAME"
    
    if not test -f $SSH_KEY_FILE
        ssh-keygen -t ed25519 -f $SSH_KEY_FILE -N "" -C "oven@aesthetic.computer"
    end
    
    # Upload SSH key to DigitalOcean
    set SSH_PUBLIC_KEY (cat "$SSH_KEY_FILE.pub")
    doctl compute ssh-key import $SSH_KEY_NAME --public-key-file "$SSH_KEY_FILE.pub" 2>/dev/null; or true
    
    # Create droplet
    echo -e "$GREEN🚀 Creating droplet: $DROPLET_NAME...$NC"
    echo -e "$YELLOW   Size: $DROPLET_SIZE$NC"
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
        --name "oven-firewall" \
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
set SETUP_SCRIPT "/tmp/oven-setup.sh"

echo '#!/bin/bash
set -e

echo "🔧 Setting up Oven server..."

# Update system
echo "📦 Updating system packages..."
apt-get update
apt-get upgrade -y

# Install Node.js 20
echo "📦 Installing Node.js 20..."
curl -fsSL https://deb.nodesource.com/setup_20.x | bash -
apt-get install -y nodejs

# Install ffmpeg
echo "📦 Installing ffmpeg..."
apt-get install -y ffmpeg

# Install Caddy
echo "📦 Installing Caddy..."
apt-get install -y debian-keyring debian-archive-keyring apt-transport-https curl
curl -1sLf '"'"'https://dl.cloudsmith.io/public/caddy/stable/gpg.key'"'"' | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
curl -1sLf '"'"'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt'"'"' | tee /etc/apt/sources.list.d/caddy-stable.list
apt-get update
apt-get install -y caddy

# Create oven user
echo "👤 Creating oven user..."
useradd -m -s /bin/bash oven || true

# Create directories
echo "📁 Creating directories..."
mkdir -p /opt/oven
mkdir -p /var/log/oven
chown -R oven:oven /opt/oven /var/log/oven

echo "✅ Server setup complete"' > $SETUP_SCRIPT

# Upload and run setup script
echo -e "$GREEN📤 Uploading setup script...$NC"
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SETUP_SCRIPT root@$DROPLET_IP:/tmp/
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "chmod +x /tmp/oven-setup.sh && /tmp/oven-setup.sh"

# Upload service files
echo -e "$GREEN📤 Uploading service files...$NC"
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SCRIPT_DIR/package.json root@$DROPLET_IP:/opt/oven/
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SCRIPT_DIR/package-lock.json root@$DROPLET_IP:/opt/oven/
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SCRIPT_DIR/server.mjs root@$DROPLET_IP:/opt/oven/
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SCRIPT_DIR/baker.mjs root@$DROPLET_IP:/opt/oven/
scp -i "$HOME/.ssh/$SSH_KEY_NAME" -o StrictHostKeyChecking=no $SERVICE_ENV root@$DROPLET_IP:/opt/oven/.env

# Update .env for production
echo -e "$GREEN🔧 Configuring production environment...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "cd /opt/oven && sed -i 's/NODE_ENV=development/NODE_ENV=production/g' .env && sed -i 's/PORT=3002/PORT=3000/g' .env"

# Install dependencies
echo -e "$GREEN📦 Installing Node.js dependencies...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "cd /opt/oven && npm install --production"

# Create systemd service
echo -e "$GREEN🔧 Creating systemd service...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "printf '[Unit]\nDescription=Oven Video Processing Service\nAfter=network.target\n\n[Service]\nType=simple\nUser=oven\nWorkingDirectory=/opt/oven\nEnvironment=NODE_ENV=production\nExecStart=/usr/bin/node /opt/oven/server.mjs\nRestart=always\nRestartSec=10\nStandardOutput=append:/var/log/oven/oven.log\nStandardError=append:/var/log/oven/oven-error.log\n\n[Install]\nWantedBy=multi-user.target\n' > /etc/systemd/system/oven.service"

# Configure Caddy
echo -e "$GREEN🔧 Configuring Caddy...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "printf '{\n    email me@jas.life\n}\n\n$OVEN_HOSTNAME {\n    reverse_proxy localhost:3000\n    encode gzip\n    \n    log {\n        output file /var/log/caddy/oven.log\n    }\n}\n' > /etc/caddy/Caddyfile"

# Start services
echo -e "$GREEN🚀 Starting services...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "systemctl daemon-reload && systemctl enable oven && systemctl restart oven && systemctl restart caddy"

# Wait for service to start
echo -e "$GREEN⏳ Waiting for service to start...$NC"
sleep 5

# Check service status
echo -e "$GREEN🔍 Checking service status...$NC"
ssh -i "$HOME/.ssh/$SSH_KEY_NAME" root@$DROPLET_IP "systemctl status oven --no-pager | head -20"

# Configure DNS
echo -e "$GREEN🌐 Configuring DNS...$NC"
echo -e "$YELLOW   Creating A record: $OVEN_HOSTNAME -> $DROPLET_IP$NC"

# Use Cloudflare API to create/update DNS record
curl -X POST "https://api.cloudflare.com/client/v4/zones/$CLOUDFLARE_ZONE_ID/dns_records" \
    -H "Authorization: Bearer $CLOUDFLARE_API_TOKEN" \
    -H "Content-Type: application/json" \
    --data "{\"type\":\"A\",\"name\":\"oven\",\"content\":\"$DROPLET_IP\",\"ttl\":1,\"proxied\":false}" \
    2>/dev/null || \
curl -X PUT "https://api.cloudflare.com/client/v4/zones/$CLOUDFLARE_ZONE_ID/dns_records" \
    -H "Authorization: Bearer $CLOUDFLARE_API_TOKEN" \
    -H "Content-Type: application/json" \
    --data "{\"type\":\"A\",\"name\":\"oven\",\"content\":\"$DROPLET_IP\",\"ttl\":1,\"proxied\":false}"

echo ""
echo -e "$GREEN✅ Deployment complete!$NC"
echo ""
echo -e "$YELLOW📋 Service Information:$NC"
echo -e "   URL: https://$OVEN_HOSTNAME"
echo -e "   IP: $DROPLET_IP"
echo -e "   SSH: ssh -i ~/.ssh/$SSH_KEY_NAME root@$DROPLET_IP"
echo ""
echo -e "$YELLOW🔧 Useful commands:$NC"
echo -e "   Check status: systemctl status oven"
echo -e "   View logs: tail -f /var/log/oven/oven.log"
echo -e "   Restart: systemctl restart oven"
echo ""
echo -e "$YELLOW⚠️  Next steps:$NC"
echo -e "   1. Update Netlify env var: OVEN_URL=https://$OVEN_HOSTNAME"
echo -e "   2. Test health: curl https://$OVEN_HOSTNAME/health"
echo -e "   3. Test dashboard: https://$OVEN_HOSTNAME"
echo ""
