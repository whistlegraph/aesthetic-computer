#!/bin/bash
# Enable direct MongoDB access on silo (with security)

set -e

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;36m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   Enable Direct MongoDB Access on Silo                    ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Get current public IP
CURRENT_IP=$(curl -s ifconfig.me)
echo -e "${YELLOW}Your current public IP: $CURRENT_IP${NC}"
echo ""

# Ask for confirmation
echo -e "${YELLOW}This will:${NC}"
echo "  1. Configure MongoDB to bind to 0.0.0.0 (all interfaces)"
echo "  2. Set up firewall to ONLY allow your IP ($CURRENT_IP)"
echo "  3. Keep authentication enabled (passwords still required)"
echo ""
echo -e "${RED}⚠️  MongoDB will be accessible from the internet (but only from your IP)${NC}"
echo ""
read -p "Continue? [y/N] " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
  echo "Aborted."
  exit 0
fi

# SSH into silo and configure
ssh -i aesthetic-computer-vault/home/.ssh/id_rsa root@silo.aesthetic.computer bash << EOF
set -e

echo "Configuring MongoDB to bind to all interfaces..."

# Backup config
cp /etc/mongod.conf /etc/mongod.conf.backup.before-direct-access

# Update bindIp to 0.0.0.0
if grep -q "bindIp:" /etc/mongod.conf; then
  sed -i 's/bindIp:.*/bindIp: 0.0.0.0/' /etc/mongod.conf
  echo "✓ Updated bindIp to 0.0.0.0"
else
  # Add bindIp if not present
  sed -i '/^net:/a\  bindIp: 0.0.0.0' /etc/mongod.conf
  echo "✓ Added bindIp: 0.0.0.0"
fi

# Enable UFW if not already enabled
if ! ufw status | grep -q "Status: active"; then
  echo "Enabling UFW firewall..."
  # Allow SSH first so we don't lock ourselves out!
  ufw allow 22/tcp
  ufw allow 443/tcp
  ufw allow 80/tcp
  ufw --force enable
  echo "✓ UFW enabled"
fi

# Allow MongoDB from specific IP only
echo "Adding firewall rule for MongoDB..."
ufw allow from $CURRENT_IP to any port 27017 comment 'MongoDB access from trusted IP'

echo "✓ Firewall rule added"

# Show firewall status
echo ""
echo "Current UFW rules:"
ufw status numbered

# Restart MongoDB
echo ""
echo "Restarting MongoDB..."
systemctl restart mongod
sleep 3

# Check if MongoDB is listening on all interfaces
echo ""
echo "MongoDB listening on:"
netstat -tlnp | grep 27017 || ss -tlnp | grep 27017

echo ""
echo "✓ MongoDB is now accessible from $CURRENT_IP"

EOF

echo ""
echo -e "${GREEN}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║                    SUCCESS!                                ║${NC}"
echo -e "${GREEN}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${YELLOW}Direct Connection String:${NC}"
echo ""

source aesthetic-computer-vault/silo/.env
DIRECT_CONN_STRING=$(echo "$MONGODB_CONNECTION_STRING" | sed 's/localhost:27018/silo.aesthetic.computer:27017/')

echo "$DIRECT_CONN_STRING"
echo ""
echo -e "${BLUE}Test the connection:${NC}"
echo "node -e \""
echo "  const { MongoClient } = require('mongodb');"
echo "  const client = new MongoClient('$DIRECT_CONN_STRING');"
echo "  client.connect()"
echo "    .then(() => client.db().admin().ping())"
echo "    .then(() => { console.log('✓ Direct connection works!'); return client.close(); })"
echo "    .catch(err => console.error('✗ Failed:', err.message));"
echo "\""
echo ""
echo -e "${YELLOW}Security Notes:${NC}"
echo "  • Only your IP ($CURRENT_IP) can access MongoDB"
echo "  • Authentication is still required (passwords)"
echo "  • To add more IPs: sudo ufw allow from NEW_IP to any port 27017"
echo "  • To disable: sudo ufw delete allow from $CURRENT_IP to any port 27017"
echo ""
