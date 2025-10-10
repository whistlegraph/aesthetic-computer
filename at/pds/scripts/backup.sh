#!/usr/bin/env bash
# PDS Backup Script
# Backs up SQLite databases and configuration

set -euo pipefail

# Configuration
PDS_DIR="${PDS_DIR:-/pds}"
BACKUP_DIR="${BACKUP_DIR:-/backups/pds}"
RETENTION_DAYS="${RETENTION_DAYS:-30}"
TIMESTAMP=$(date +%Y%m%d-%H%M%S)

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "╔════════════════════════════════════════╗"
echo "║   PDS Backup                           ║"
echo "╚════════════════════════════════════════╝"
echo ""

# Create backup directory
mkdir -p "$BACKUP_DIR"

echo -e "${BLUE}ℹ${NC} Backup directory: $BACKUP_DIR"
echo -e "${BLUE}ℹ${NC} Timestamp: $TIMESTAMP"
echo ""

# Backup SQLite databases
backup_database() {
    local DB_NAME=$1
    local DB_PATH="$PDS_DIR/$DB_NAME"
    
    if [ ! -f "$DB_PATH" ]; then
        echo -e "${YELLOW}⊘${NC} $DB_NAME not found, skipping"
        return 0
    fi
    
    echo -n "Backing up $DB_NAME... "
    
    # Use SQLite backup command for consistency
    sqlite3 "$DB_PATH" ".backup '$BACKUP_DIR/${DB_NAME%.sqlite}-$TIMESTAMP.sqlite'" 2>/dev/null
    
    if [ $? -eq 0 ]; then
        SIZE=$(du -h "$BACKUP_DIR/${DB_NAME%.sqlite}-$TIMESTAMP.sqlite" | cut -f1)
        echo -e "${GREEN}✓${NC} ($SIZE)"
        return 0
    else
        echo -e "${RED}✗${NC} FAILED"
        return 1
    fi
}

# Backup configuration
backup_config() {
    echo -n "Backing up configuration... "
    
    if [ -f "$PDS_DIR/pds.env" ]; then
        cp "$PDS_DIR/pds.env" "$BACKUP_DIR/pds.env-$TIMESTAMP"
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${YELLOW}⊘${NC} pds.env not found"
    fi
}

# Backup databases
backup_database "accounts.sqlite"
backup_database "blocks.sqlite"
backup_database "pds.sqlite"

# Backup config
backup_config

# Create compressed archive
echo -n "Creating compressed archive... "
ARCHIVE_NAME="pds-backup-$TIMESTAMP.tar.gz"
tar czf "$BACKUP_DIR/$ARCHIVE_NAME" -C "$BACKUP_DIR" \
    $(ls "$BACKUP_DIR" | grep "$TIMESTAMP") 2>/dev/null

if [ $? -eq 0 ]; then
    ARCHIVE_SIZE=$(du -h "$BACKUP_DIR/$ARCHIVE_NAME" | cut -f1)
    echo -e "${GREEN}✓${NC} ($ARCHIVE_SIZE)"
    
    # Remove individual files
    rm -f "$BACKUP_DIR"/*-$TIMESTAMP.sqlite "$BACKUP_DIR"/*-$TIMESTAMP
else
    echo -e "${YELLOW}⚠${NC} Archive creation failed, keeping individual files"
fi

# Clean old backups
echo -n "Cleaning old backups (>$RETENTION_DAYS days)... "
DELETED=$(find "$BACKUP_DIR" -name "pds-backup-*.tar.gz" -mtime +$RETENTION_DAYS -delete -print | wc -l)
echo -e "${GREEN}✓${NC} (deleted $DELETED)"

# Summary
echo ""
echo "Backup Summary:"
echo "  Location: $BACKUP_DIR/$ARCHIVE_NAME"
echo "  Size: ${ARCHIVE_SIZE:-unknown}"
echo "  Retention: $RETENTION_DAYS days"

# Optional: Upload to Spaces/S3
if [ -n "${SPACES_BACKUP_BUCKET:-}" ]; then
    echo ""
    echo -n "Uploading to Spaces... "
    
    if command -v s3cmd &> /dev/null; then
        s3cmd put "$BACKUP_DIR/$ARCHIVE_NAME" "s3://$SPACES_BACKUP_BUCKET/backups/" &> /dev/null
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${YELLOW}⊘${NC} s3cmd not installed"
    fi
fi

echo ""
echo -e "${GREEN}Backup complete!${NC}"
