#!/bin/bash
# Migrate MongoDB connection strings from Atlas to Silo

set -e

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;36m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║     MongoDB Migration: Atlas → Silo                        ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Load silo credentials
if [ ! -f "aesthetic-computer-vault/silo/.env" ]; then
  echo -e "${RED}✗ Silo credentials not found!${NC}"
  echo "Run setup first: utilities/setup-silo-mongodb-auth.sh"
  exit 1
fi

source aesthetic-computer-vault/silo/.env

SILO_CONN_STRING="$MONGODB_CONNECTION_STRING"
SILO_DB_NAME="$MONGODB_NAME"

echo -e "${YELLOW}Silo Connection String:${NC}"
echo "$SILO_CONN_STRING" | sed 's/:[^:@]*@/:****@/'
echo ""

# Ask for confirmation
echo -e "${YELLOW}This will update MongoDB connection strings in:${NC}"
echo "  • 7 vault .env files (at, censor, feed, judge, oven, paintings, session-server)"
echo "  • 3 service .env files (nanos, conductor)"
echo ""
echo -e "${RED}⚠️  You'll still need to manually update Netlify environment variables${NC}"
echo ""
read -p "Continue? [y/N] " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
  echo "Aborted."
  exit 0
fi

# Backup before changes
BACKUP_DIR="mongodb-migration-backup-$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"
echo -e "${YELLOW}Creating backups in $BACKUP_DIR...${NC}"

# Update vault files
VAULT_FILES=(
  "aesthetic-computer-vault/at/.env"
  "aesthetic-computer-vault/censor/.env"
  "aesthetic-computer-vault/feed/.env"
  "aesthetic-computer-vault/judge/.env"
  "aesthetic-computer-vault/oven/.env"
  "aesthetic-computer-vault/paintings/.env"
  "aesthetic-computer-vault/session-server/.env"
)

for file in "${VAULT_FILES[@]}"; do
  if [ -f "$file" ]; then
    # Backup
    cp "$file" "$BACKUP_DIR/$(basename $file).$(basename $(dirname $file))"

    # Update connection string and database name
    if grep -q "MONGODB_CONNECTION_STRING" "$file"; then
      sed -i.tmp "s|MONGODB_CONNECTION_STRING=.*|MONGODB_CONNECTION_STRING=\"$SILO_CONN_STRING\"|" "$file"
      sed -i.tmp "s|MONGODB_NAME=.*|MONGODB_NAME=\"$SILO_DB_NAME\"|" "$file"
      rm -f "$file.tmp"
      echo -e "${GREEN}✓${NC} Updated: $file"
    else
      echo -e "${YELLOW}⊘${NC} Skipped (no MongoDB config): $file"
    fi
  else
    echo -e "${YELLOW}⊘${NC} Not found: $file"
  fi
done

# Update service files
SERVICE_FILES=(
  "nanos/chat.env"
  "nanos/conductor.env"
)

for file in "${SERVICE_FILES[@]}"; do
  if [ -f "$file" ]; then
    cp "$file" "$BACKUP_DIR/$(basename $file)"

    if grep -q "MONGODB_CONNECTION_STRING" "$file"; then
      sed -i.tmp "s|MONGODB_CONNECTION_STRING=.*|MONGODB_CONNECTION_STRING=\"$SILO_CONN_STRING\"|" "$file"
      sed -i.tmp "s|MONGODB_NAME=.*|MONGODB_NAME=\"$SILO_DB_NAME\"|" "$file"
      rm -f "$file.tmp"
      echo -e "${GREEN}✓${NC} Updated: $file"
    fi
  fi
done

echo ""
echo -e "${GREEN}✓ Local files updated!${NC}"
echo -e "${YELLOW}Backups saved to: $BACKUP_DIR${NC}"
echo ""

# Generate Netlify env var update instructions
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║     Next: Update Netlify Environment Variables            ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo "1. Go to: https://app.netlify.com/sites/YOUR_SITE/settings/deploys#environment"
echo ""
echo "2. Update these variables:"
echo -e "   ${YELLOW}MONGODB_CONNECTION_STRING${NC}"
echo "   $SILO_CONN_STRING"
echo ""
echo -e "   ${YELLOW}MONGODB_NAME${NC}"
echo "   $SILO_DB_NAME"
echo ""
echo "3. Click 'Save' and trigger a new deploy"
echo ""

# Check if SSH tunnel is needed
if [[ "$SILO_CONN_STRING" == *"localhost:27018"* ]]; then
  echo -e "${YELLOW}⚠️  SSH Tunnel Required${NC}"
  echo ""
  echo "Your connection string uses localhost:27018, which requires an SSH tunnel."
  echo ""
  echo -e "${BLUE}Start SSH tunnel:${NC}"
  echo "  ssh -i aesthetic-computer-vault/home/.ssh/id_rsa -f -N -L 27018:localhost:27017 root@silo.aesthetic.computer"
  echo ""
  echo -e "${YELLOW}OR${NC} update /etc/mongod.conf on silo to bind to 0.0.0.0 and use:"
  echo "  mongodb://aesthetic_app:****@silo.aesthetic.computer:27017/aesthetic?authSource=aesthetic"
  echo ""
fi

# Rollback instructions
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║     Rollback Instructions (if needed)                     ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo "If you need to rollback:"
echo "  cp $BACKUP_DIR/* aesthetic-computer-vault/*/.env"
echo "  # And revert Netlify environment variables to Atlas"
echo ""

echo -e "${GREEN}Migration preparation complete!${NC}"
