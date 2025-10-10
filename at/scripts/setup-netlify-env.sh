#!/usr/bin/env bash
# setup-netlify-env.sh
# Sets up Netlify environment variables for ATProto integration

set -e

VAULT_ENV="/workspaces/aesthetic-computer/aesthetic-computer-vault/at/.env"

if [ ! -f "$VAULT_ENV" ]; then
    echo "❌ Vault env file not found: $VAULT_ENV"
    exit 1
fi

echo "🔧 Setting up Netlify environment variables for ATProto integration..."
echo ""

# Load env vars from vault
source "$VAULT_ENV" 2>/dev/null || true

cd /workspaces/aesthetic-computer/system

# Set PDS-related environment variables
echo "Setting PDS_URL..."
npx netlify env:set PDS_URL "$PDS_URL" --context production

echo "Setting PDS_ADMIN_PASSWORD..."
npx netlify env:set PDS_ADMIN_PASSWORD "$PDS_ADMIN_PASSWORD" --context production

echo ""
echo "✅ Netlify environment variables configured!"
echo ""
echo "Variables added:"
echo "  - PDS_URL: $PDS_URL"
echo "  - PDS_ADMIN_PASSWORD: [hidden]"
echo ""
echo "Note: MongoDB and Auth0 variables should already be set in Netlify."
