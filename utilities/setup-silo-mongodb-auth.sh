#!/bin/bash
# Setup MongoDB Authentication on silo.aesthetic.computer
# Run this script ON the silo server: bash setup-silo-mongodb-auth.sh

set -e

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;36m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║     MongoDB Authentication Setup for Silo                 ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check if running as root or with sudo
if [ "$EUID" -ne 0 ]; then
  echo -e "${RED}Please run with sudo: sudo bash setup-silo-mongodb-auth.sh${NC}"
  exit 1
fi

# Generate secure credentials
echo -e "${YELLOW}Generating secure credentials...${NC}"
MONGO_ADMIN_USER="admin"
MONGO_ADMIN_PASS=$(openssl rand -base64 32 | tr -d "=+/" | cut -c1-32)

MONGO_APP_USER="aesthetic_app"
MONGO_APP_PASS=$(openssl rand -base64 32 | tr -d "=+/" | cut -c1-32)

echo -e "${GREEN}✓ Generated secure passwords${NC}"
echo ""

# Check if MongoDB is running
echo -e "${YELLOW}Checking MongoDB status...${NC}"
if ! systemctl is-active --quiet mongod; then
  echo -e "${RED}MongoDB is not running. Starting it...${NC}"
  systemctl start mongod
  sleep 2
fi
echo -e "${GREEN}✓ MongoDB is running${NC}"
echo ""

# Create admin user (if not exists)
echo -e "${YELLOW}Creating MongoDB admin user...${NC}"
mongosh --quiet --eval "
  try {
    db = db.getSiblingDB('admin');
    const existing = db.getUser('${MONGO_ADMIN_USER}');
    if (existing) {
      print('⚠️  Admin user already exists, updating password...');
      db.changeUserPassword('${MONGO_ADMIN_USER}', '${MONGO_ADMIN_PASS}');
    } else {
      db.createUser({
        user: '${MONGO_ADMIN_USER}',
        pwd: '${MONGO_ADMIN_PASS}',
        roles: [ { role: 'root', db: 'admin' } ]
      });
      print('✓ Admin user created');
    }
  } catch(e) {
    print('Error creating admin user: ' + e);
    quit(1);
  }
"
echo ""

# Create application user for 'aesthetic' database
echo -e "${YELLOW}Creating application user for 'aesthetic' database...${NC}"
mongosh --quiet --eval "
  try {
    db = db.getSiblingDB('aesthetic');
    const existing = db.getUser('${MONGO_APP_USER}');
    if (existing) {
      print('⚠️  App user already exists, updating password...');
      db.changeUserPassword('${MONGO_APP_USER}', '${MONGO_APP_PASS}');
    } else {
      db.createUser({
        user: '${MONGO_APP_USER}',
        pwd: '${MONGO_APP_PASS}',
        roles: [
          { role: 'readWrite', db: 'aesthetic' },
          { role: 'dbAdmin', db: 'aesthetic' }
        ]
      });
      print('✓ Application user created');
    }
  } catch(e) {
    print('Error creating app user: ' + e);
    quit(1);
  }
"
echo ""

# Enable authentication in mongod.conf
echo -e "${YELLOW}Configuring MongoDB for authentication...${NC}"

MONGO_CONF="/etc/mongod.conf"
BACKUP_CONF="/etc/mongod.conf.backup.$(date +%Y%m%d_%H%M%S)"

# Backup existing config
cp "$MONGO_CONF" "$BACKUP_CONF"
echo -e "${GREEN}✓ Backed up config to $BACKUP_CONF${NC}"

# Check if authentication is already enabled
if grep -q "^security:" "$MONGO_CONF"; then
  echo -e "${YELLOW}Security section already exists in config${NC}"
else
  # Add security section
  cat >> "$MONGO_CONF" << 'EOF'

# Security settings
security:
  authorization: enabled
EOF
  echo -e "${GREEN}✓ Added authentication to MongoDB config${NC}"
fi

# Ask about network binding
echo ""
echo -e "${YELLOW}Network Configuration:${NC}"
echo "1. Keep MongoDB on localhost only (SSH tunnel required) - MOST SECURE"
echo "2. Bind to 0.0.0.0 to allow remote connections - REQUIRES FIREWALL SETUP"
echo ""
read -p "Choose option [1/2] (default: 1): " NETWORK_CHOICE
NETWORK_CHOICE=${NETWORK_CHOICE:-1}

if [ "$NETWORK_CHOICE" = "2" ]; then
  echo -e "${YELLOW}Configuring MongoDB to accept remote connections...${NC}"

  # Update bindIp in config
  if grep -q "bindIp:" "$MONGO_CONF"; then
    sed -i 's/bindIp:.*/bindIp: 0.0.0.0/' "$MONGO_CONF"
  else
    sed -i '/^net:/a\  bindIp: 0.0.0.0' "$MONGO_CONF"
  fi

  echo -e "${GREEN}✓ MongoDB will accept remote connections${NC}"
  echo -e "${RED}⚠️  IMPORTANT: Configure firewall to only allow trusted IPs!${NC}"
  echo -e "${YELLOW}Run: sudo ufw allow from YOUR_IP to any port 27017${NC}"
else
  echo -e "${GREEN}✓ MongoDB will remain localhost-only (most secure)${NC}"
fi

# Restart MongoDB to apply changes
echo ""
echo -e "${YELLOW}Restarting MongoDB...${NC}"
systemctl restart mongod
sleep 2

if systemctl is-active --quiet mongod; then
  echo -e "${GREEN}✓ MongoDB restarted successfully${NC}"
else
  echo -e "${RED}✗ MongoDB failed to restart. Check logs: journalctl -u mongod${NC}"
  exit 1
fi

# Test authentication
echo ""
echo -e "${YELLOW}Testing authentication...${NC}"
mongosh "mongodb://localhost:27017/aesthetic" \
  -u "$MONGO_APP_USER" \
  -p "$MONGO_APP_PASS" \
  --authenticationDatabase aesthetic \
  --quiet \
  --eval "db.runCommand({ping: 1})" > /dev/null

if [ $? -eq 0 ]; then
  echo -e "${GREEN}✓ Authentication working!${NC}"
else
  echo -e "${RED}✗ Authentication test failed${NC}"
  exit 1
fi

# Generate connection strings
echo ""
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║                    SUCCESS!                                ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

HOSTNAME=$(hostname -f 2>/dev/null || echo "silo.aesthetic.computer")

if [ "$NETWORK_CHOICE" = "2" ]; then
  CONNECTION_STRING="mongodb://${MONGO_APP_USER}:${MONGO_APP_PASS}@${HOSTNAME}:27017/aesthetic?authSource=aesthetic"
else
  CONNECTION_STRING="mongodb://${MONGO_APP_USER}:${MONGO_APP_PASS}@localhost:27017/aesthetic?authSource=aesthetic"
fi

ADMIN_CONNECTION="mongodb://${MONGO_ADMIN_USER}:${MONGO_ADMIN_PASS}@localhost:27017/admin?authSource=admin"

# Save to file
CRED_FILE="/root/mongodb-credentials.txt"
cat > "$CRED_FILE" << EOF
MongoDB Credentials for silo.aesthetic.computer
Generated: $(date)

=== Admin User (root access) ===
Username: $MONGO_ADMIN_USER
Password: $MONGO_ADMIN_PASS
Database: admin
Connection: $ADMIN_CONNECTION

=== Application User (aesthetic database) ===
Username: $MONGO_APP_USER
Password: $MONGO_APP_PASS
Database: aesthetic
Connection: $CONNECTION_STRING

=== For .env files ===
MONGODB_CONNECTION_STRING="$CONNECTION_STRING"
MONGODB_NAME="aesthetic"

=== If using SSH tunnel ===
# On local machine, create tunnel:
ssh -L 27018:localhost:27017 user@silo.aesthetic.computer

# Then use:
MONGODB_CONNECTION_STRING="mongodb://${MONGO_APP_USER}:${MONGO_APP_PASS}@localhost:27018/aesthetic?authSource=aesthetic"
MONGODB_NAME="aesthetic"
EOF

chmod 600 "$CRED_FILE"
echo -e "${GREEN}Credentials saved to: $CRED_FILE${NC}"
echo ""

# Display credentials
cat "$CRED_FILE"

echo ""
echo -e "${YELLOW}════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}IMPORTANT: Copy these credentials to your vault!${NC}"
echo -e "${YELLOW}File location: $CRED_FILE${NC}"
echo -e "${YELLOW}════════════════════════════════════════════════════════${NC}"
