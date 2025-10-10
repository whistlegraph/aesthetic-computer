#!/usr/bin/env fish
# Generate PDS environment file from vault credentials
# Usage: fish generate-pds-env.fish [output-file]

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR (cd "$SCRIPT_DIR/../../../../aesthetic-computer-vault" && pwd)
set OUTPUT_FILE (test -n "$argv[1]" && echo $argv[1] || echo "$SCRIPT_DIR/../../config/pds.env")

echo "╔════════════════════════════════════════════════════════╗"
echo "║   Generate PDS Environment File                        ║"
echo "╚════════════════════════════════════════════════════════╝"
echo ""

# Load vault environment using bash-style format
if not test -f "$VAULT_DIR/at/deploy.env"
    echo "✗ Vault deployment config not found: $VAULT_DIR/at/deploy.env"
    exit 1
end

# Parse bash-style env file for Fish
for line in (cat "$VAULT_DIR/at/deploy.env" | grep -v '^#' | grep -v '^$')
    set -l parts (string split '=' -- $line)
    if test (count $parts) -ge 2
        set -gx $parts[1] (string join '=' -- $parts[2..-1])
    end
end

echo "✓ Loaded vault configuration"
echo ""

# Check required variables
set -l missing_vars 0

if test -z "$PDS_HOSTNAME"
    echo "✗ PDS_HOSTNAME not set"
    set missing_vars 1
end

if test -z "$SPACES_KEY"
    echo "✗ SPACES_KEY not set"
    set missing_vars 1
end

if test -z "$SPACES_SECRET"
    echo "✗ SPACES_SECRET not set"
    set missing_vars 1
end

if test $missing_vars -eq 1
    echo ""
    echo "Please configure missing variables in $VAULT_DIR/at/deploy.env"
    exit 1
end

# Warn if SMTP not configured
if test -z "$SMTP_URL"
    echo "⚠️  SMTP_URL not configured - email verification will not work!"
    echo ""
end

echo "Generating PDS environment file..."
echo ""
echo "Configuration:"
echo "  Hostname: $PDS_HOSTNAME"
echo "  Spaces Bucket: $SPACES_BUCKET"
echo "  Spaces Region: $SPACES_REGION"
echo "  SMTP: "(test -n "$SMTP_URL" && echo "✓ Configured" || echo "✗ Not configured")
echo ""

# Generate the file using printf (Fish doesn't support heredocs)
printf "# PDS Environment Configuration
# Generated from vault on %s
# DO NOT COMMIT THIS FILE

# =============================================================================
# REQUIRED: Basic PDS Configuration
# =============================================================================

PDS_HOSTNAME=%s
PDS_ADMIN_EMAIL=%s
PDS_PORT=3000
PDS_DATA_DIRECTORY=/pds

# =============================================================================
# REQUIRED: DID PLC Configuration
# =============================================================================

PDS_DID_PLC_URL=https://plc.directory

# =============================================================================
# REQUIRED: Relay/Firehose Configuration
# =============================================================================

PDS_BSKY_APP_VIEW_URL=https://api.bsky.app
PDS_BSKY_APP_VIEW_DID=did:web:api.bsky.app
PDS_CRAWLERS=https://bsky.network

# =============================================================================
# REQUIRED: Blob Storage - DigitalOcean Spaces
# =============================================================================

PDS_BLOBSTORE_DISK_LOCATION=s3
PDS_BLOBSTORE_S3_ENDPOINT=%s
PDS_BLOBSTORE_S3_BUCKET=%s
PDS_BLOBSTORE_S3_ACCESS_KEY_ID=%s
PDS_BLOBSTORE_S3_SECRET_ACCESS_KEY=%s
PDS_BLOBSTORE_S3_REGION=%s
PDS_BLOBSTORE_S3_FORCE_PATH_STYLE=true

# =============================================================================
# REQUIRED: Email Configuration (SMTP)
# =============================================================================

" (date) "$PDS_HOSTNAME" "$PDS_ADMIN_EMAIL" "$SPACES_ENDPOINT" "$SPACES_BUCKET" "$SPACES_KEY" "$SPACES_SECRET" "$SPACES_REGION" > "$OUTPUT_FILE"

if test -n "$SMTP_URL"
    echo "PDS_EMAIL_SMTP_URL=$SMTP_URL" >> "$OUTPUT_FILE"
    echo "PDS_EMAIL_FROM_ADDRESS=$SMTP_FROM_ADDRESS" >> "$OUTPUT_FILE"
else
    echo "# SMTP NOT CONFIGURED - ADD THESE:" >> "$OUTPUT_FILE"
    echo "# PDS_EMAIL_SMTP_URL=smtps://resend:YOUR_KEY@smtp.resend.com:465/" >> "$OUTPUT_FILE"
    echo "# PDS_EMAIL_FROM_ADDRESS=noreply@aesthetic.computer" >> "$OUTPUT_FILE"
end

printf "
# =============================================================================
# OPTIONAL: Service Configuration
# =============================================================================

# Service DID (generated on first run)
PDS_SERVICE_DID=

# JWT secret (generated automatically)
PDS_JWT_SECRET=

# =============================================================================
# OPTIONAL: Rate Limiting
# =============================================================================

PDS_RATE_LIMIT_ENABLED=true
PDS_RATE_LIMIT_GLOBAL_LIMIT_PER_PERIOD=3000
PDS_RATE_LIMIT_GLOBAL_PERIOD=300000

# =============================================================================
# OPTIONAL: Account Creation
# =============================================================================

PDS_INVITE_REQUIRED=true
PDS_MAX_ACCOUNTS=100

# =============================================================================
# OPTIONAL: Moderation
# =============================================================================

PDS_MODERATION_SERVICE_DID=did:plc:ar7c4by46qjdydhdevvrndac
PDS_MODERATION_SERVICE_URL=https://mod.bsky.app

# =============================================================================
# OPTIONAL: Logging
# =============================================================================

LOG_LEVEL=info
LOG_DESTINATION=/pds/pds.log

# =============================================================================
# OPTIONAL: Advanced Configuration
# =============================================================================

PDS_MAX_BLOB_SIZE=5242880
PDS_ACCOUNT_MIGRATION_ENABLED=true
PDS_WEBSOCKET_KEEPALIVE_INTERVAL=30000
PDS_DB_SQLITE_LOCATION=/pds
" >> "$OUTPUT_FILE"

chmod 600 "$OUTPUT_FILE"

echo "✓ Generated PDS environment file: $OUTPUT_FILE"
echo ""
echo "Next steps:"
echo "  1. Review the file: cat $OUTPUT_FILE"
if test -z "$SMTP_URL"
    echo "  2. ⚠️  Configure SMTP in $VAULT_DIR/at/deploy.env"
    echo "  3. Regenerate: fish $SCRIPT_DIR/generate-pds-env.fish"
end
echo ""
