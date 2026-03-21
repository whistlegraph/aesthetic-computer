#!/usr/bin/env fish
# Deploy Aesthetic Computer PDS to DigitalOcean
# This script wraps the bash deployment script with vault environment loading

# Get script directory
set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR (cd "$SCRIPT_DIR/../../../../aesthetic-computer-vault" && pwd)
set AT_DIR (cd "$SCRIPT_DIR/../.." && pwd)

echo "╔════════════════════════════════════════════════════════╗"
echo "║   Aesthetic Computer PDS Deployment (Fish)            ║"
echo "╚════════════════════════════════════════════════════════╝"
echo ""

# Load vault environment using bash-style format
if test -f "$VAULT_DIR/at/deploy.env"
    echo "✓ Loading deployment config from vault..."
    # Parse bash-style env file for Fish
    for line in (cat "$VAULT_DIR/at/deploy.env" | grep -v '^#' | grep -v '^$')
        set -l parts (string split '=' -- $line)
        if test (count $parts) -ge 2
            set -gx $parts[1] (string join '=' -- $parts[2..-1])
        end
    end
else
    echo "✗ Vault deployment config not found: $VAULT_DIR/at/deploy.env"
    exit 1
end

# Check for SMTP configuration
if test -z "$SMTP_URL"
    echo ""
    echo "⚠️  SMTP not configured!"
    echo ""
    echo "You need to configure SMTP for email verification."
    echo "Edit: $VAULT_DIR/at/deploy.env"
    echo ""
    echo "Recommended: Use Resend (free tier, 3k emails/month)"
    echo "  1. Sign up: https://resend.com"
    echo "  2. Get API key"
    echo "  3. Add to deploy.env:"
    echo "     SMTP_URL=smtps://resend:YOUR_KEY@smtp.resend.com:465/"
    echo ""
    read -P "Continue without SMTP? (y/N) " -n 1 confirm
    echo ""
    if test "$confirm" != "y" -a "$confirm" != "Y"
        exit 1
    end
end

# Export variables for bash script
export DO_TOKEN
export SPACES_KEY
export SPACES_SECRET
export PDS_HOSTNAME
export PDS_ADMIN_EMAIL
export SPACES_REGION
export SPACES_BUCKET
export SPACES_ENDPOINT
export DROPLET_NAME
export DROPLET_SIZE
export DROPLET_IMAGE
export DROPLET_REGION
export SMTP_URL
export SMTP_FROM_ADDRESS
export BACKUP_BUCKET
export ALERT_EMAIL

# Run the bash deployment script
echo ""
echo "→ Starting deployment..."
echo ""

# Set auto-confirm for non-interactive mode
set -x AUTO_CONFIRM yes

bash "$SCRIPT_DIR/deploy.sh" $argv
