#!/bin/bash
# Copy MongoDB credentials from silo to vault

set -e

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;36m'
RED='\033[0;31m'
NC='\033[0m'

SILO_HOST="${1:-root@silo.aesthetic.computer}"
VAULT_FILE="aesthetic-computer-vault/silo/.env"

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   Copy MongoDB Credentials from Silo to Vault             ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check if credentials exist on silo
echo -e "${YELLOW}Checking for credentials on silo...${NC}"
if ! ssh "$SILO_HOST" "test -f /root/mongodb-credentials.txt"; then
  echo -e "${RED}✗ Credentials file not found on silo${NC}"
  echo -e "${YELLOW}Run the setup script first: utilities/setup-silo-mongodb-auth.sh${NC}"
  exit 1
fi

# Download credentials
echo -e "${YELLOW}Downloading credentials...${NC}"
TEMP_FILE=$(mktemp)
scp "$SILO_HOST:/root/mongodb-credentials.txt" "$TEMP_FILE"

if [ ! -f "$TEMP_FILE" ]; then
  echo -e "${RED}✗ Failed to download credentials${NC}"
  exit 1
fi

echo -e "${GREEN}✓ Downloaded credentials${NC}"
echo ""

# Extract passwords
MONGO_ADMIN_PASS=$(grep "^Password:" "$TEMP_FILE" | head -1 | awk '{print $2}')
MONGO_APP_USER=$(grep "^Username:" "$TEMP_FILE" | tail -1 | awk '{print $2}')
MONGO_APP_PASS=$(grep "^Password:" "$TEMP_FILE" | tail -1 | awk '{print $2}')

# Update vault file
echo -e "${YELLOW}Updating vault file: $VAULT_FILE${NC}"

cat > "$VAULT_FILE" << EOF
# Silo MongoDB Credentials
# Generated: $(date)
# Server: silo.aesthetic.computer (64.23.151.169)

# Application User (for day-to-day use)
MONGODB_CONNECTION_STRING="mongodb://${MONGO_APP_USER}:${MONGO_APP_PASS}@localhost:27018/aesthetic?authSource=aesthetic"
MONGODB_NAME="aesthetic"

# For SSH Tunnel (recommended - most secure)
# In one terminal: ssh -L 27018:localhost:27017 root@silo.aesthetic.computer
# Then use the connection string above

# For Direct Connection (if MongoDB bound to 0.0.0.0)
# MONGODB_CONNECTION_STRING="mongodb://${MONGO_APP_USER}:${MONGO_APP_PASS}@silo.aesthetic.computer:27017/aesthetic?authSource=aesthetic"

# Admin credentials (use sparingly, only for admin tasks)
MONGO_ADMIN_USER="admin"
MONGO_ADMIN_PASSWORD="${MONGO_ADMIN_PASS}"
MONGO_ADMIN_CONNECTION="mongodb://admin:${MONGO_ADMIN_PASS}@localhost:27017/admin?authSource=admin"

# Server access
SILO_HOST="silo.aesthetic.computer"
SILO_IP="64.23.151.169"
SILO_SSH_USER="root"
SILO_SSH_PORT="22"

# For benchmark script
SILO_MONGODB_CONNECTION_STRING="mongodb://${MONGO_APP_USER}:${MONGO_APP_PASS}@localhost:27018/aesthetic?authSource=aesthetic"
SILO_MONGODB_NAME="aesthetic"
EOF

echo -e "${GREEN}✓ Updated $VAULT_FILE${NC}"
echo ""

# Display connection info
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║                    SUCCESS!                                ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${GREEN}Credentials stored in: $VAULT_FILE${NC}"
echo ""
echo -e "${YELLOW}To use these credentials:${NC}"
echo ""
echo -e "1. Create SSH tunnel:"
echo -e "   ${BLUE}ssh -L 27018:localhost:27017 root@silo.aesthetic.computer${NC}"
echo ""
echo -e "2. Source credentials:"
echo -e "   ${BLUE}export \$(grep -v '^#' $VAULT_FILE | xargs)${NC}"
echo ""
echo -e "3. Test connection:"
echo -e "   ${BLUE}mongosh \"\$MONGODB_CONNECTION_STRING\" --eval 'db.runCommand({ping: 1})'${NC}"
echo ""
echo -e "4. Run benchmark:"
echo -e "   ${BLUE}node utilities/benchmark-mongodb.mjs${NC}"
echo ""

# Cleanup
rm -f "$TEMP_FILE"
