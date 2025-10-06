#!/usr/bin/env fish

# Build and deploy KidLisp feed
# This script loads secrets from vault and runs the build

echo "🎨 Building KidLisp Feed"
echo "========================"
echo ""

# Load environment variables from vault
set vault_env /workspaces/aesthetic-computer/aesthetic-computer-vault/feed/.env

if test -f $vault_env
    echo "📦 Loading secrets from vault..."
    # Export environment variables for fish
    for line in (cat $vault_env | grep -v '^#' | grep '=')
        set -gx (string split '=' $line)
    end
    echo "✅ Secrets loaded"
    echo ""
else
    echo "❌ Error: Vault environment file not found at $vault_env"
    echo "   Please ensure aesthetic-computer-vault/feed/.env exists"
    exit 1
end

# Check required variables
if not set -q FEED_API_SECRET
    echo "❌ Error: FEED_API_SECRET not set"
    exit 1
end

if not set -q MONGODB_CONNECTION_STRING
    echo "❌ Error: MONGODB_CONNECTION_STRING not set"
    exit 1
end

# Run the build script
echo "🚀 Running build script..."
echo ""
cd /workspaces/aesthetic-computer/feed
node build-kidlisp-feed.mjs

if test $status -eq 0
    echo ""
    echo "✅ Build complete!"
    echo "🌐 View channel: https://feed.aesthetic.computer/api/v1/channels/$KIDLISP_CHANNEL_ID"
else
    echo ""
    echo "❌ Build failed with exit code $status"
    exit 1
end
