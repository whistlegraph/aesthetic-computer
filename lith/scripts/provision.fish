#!/usr/bin/env fish
# Provision a new DO droplet for lith
# Run locally — creates droplet, installs deps, clones repo

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set DROPLET_NAME "ac-lith"
set REGION "sfo3"
set SIZE "s-2vcpu-4gb"
set IMAGE "ubuntu-24-04-x64"

# Get SSH key IDs
echo -e "$GREEN-> Fetching SSH keys...$NC"
set SSH_KEYS (doctl compute ssh-key list --format ID --no-header | string join ',')

if test -z "$SSH_KEYS"
    echo -e "$RED x No SSH keys found in DO account$NC"
    exit 1
end

echo -e "$GREEN-> Creating droplet $DROPLET_NAME ($SIZE in $REGION)...$NC"
set DROPLET_ID (doctl compute droplet create $DROPLET_NAME \
    --region $REGION \
    --size $SIZE \
    --image $IMAGE \
    --ssh-keys $SSH_KEYS \
    --tag-names ac,lith,web \
    --wait \
    --format ID \
    --no-header)

set DROPLET_IP (doctl compute droplet get $DROPLET_ID --format PublicIPv4 --no-header)

echo -e "$GREEN-> Droplet created: $DROPLET_NAME @ $DROPLET_IP$NC"
echo -e "$YELLOW   Save this IP for Cloudflare DNS updates!$NC"
echo ""
echo "LITH_IP=$DROPLET_IP" > /tmp/lith-droplet.env
echo "LITH_ID=$DROPLET_ID" >> /tmp/lith-droplet.env

# Wait for SSH
echo -e "$GREEN-> Waiting for SSH...$NC"
sleep 10
while not ssh -o StrictHostKeyChecking=no -o ConnectTimeout=5 root@$DROPLET_IP "echo ok" &>/dev/null
    sleep 5
end

# Install stack
echo -e "$GREEN-> Installing Caddy + Node.js + system deps...$NC"
ssh root@$DROPLET_IP "
    # Caddy
    apt-get update -qq
    apt-get install -y -qq debian-keyring debian-archive-keyring apt-transport-https curl
    curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
    curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | tee /etc/apt/sources.list.d/caddy-stable.list
    apt-get update -qq
    apt-get install -y -qq caddy

    # Node.js 22 LTS
    curl -fsSL https://deb.nodesource.com/setup_22.x | bash -
    apt-get install -y -qq nodejs

    # Sharp + Puppeteer deps
    apt-get install -y -qq libvips-dev chromium-browser git

    # Clone repo
    git clone https://github.com/aesthetic-computer/aesthetic-computer.git /opt/ac
    cd /opt/ac/lith && npm install
    cd /opt/ac/system && npm install
"

echo -e "$GREEN-> Droplet provisioned.$NC"
echo -e "$YELLOW-> Next steps:$NC"
echo "   1. Upload .env:      scp vault/netlify-production.env root@$DROPLET_IP:/opt/ac/system/.env"
echo "   2. Upload Caddyfile: scp lith/Caddyfile root@$DROPLET_IP:/etc/caddy/Caddyfile"
echo "   3. Install service:  scp lith/lith.service root@$DROPLET_IP:/etc/systemd/system/"
echo "   4. Enable service:   ssh root@$DROPLET_IP 'systemctl daemon-reload && systemctl enable --now lith && systemctl reload caddy'"
echo "   5. Update DNS:       Point 36 records to $DROPLET_IP (see vault/cloudflare-dns-records.md)"
