#!/usr/bin/env bash
# Deploy to aesthetic.computer's Cloudflare Worker
# This script wraps the deployment process for aesthetic.computer

set -e

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DP1_FEED_DIR="$SCRIPT_DIR/../dp1-feed"
CONFIG_FILE="$DP1_FEED_DIR/wrangler.production.toml"

echo "🚀 Deploying aesthetic-feed to production..."
echo "📂 DP1 Feed directory: $DP1_FEED_DIR"
echo "⚙️  Config file: $CONFIG_FILE"
echo ""

# Check if wrangler.production.toml exists
if [ ! -f "$CONFIG_FILE" ]; then
    echo "❌ Error: wrangler.production.toml not found at $CONFIG_FILE"
    echo ""
    echo "Checking if backup config exists in dp1-deploy..."
    BACKUP_CONFIG="$SCRIPT_DIR/wrangler.production.toml"
    if [ -f "$BACKUP_CONFIG" ]; then
        echo "✅ Found backup config, copying to dp1-feed directory..."
        cp "$BACKUP_CONFIG" "$CONFIG_FILE"
    else
        echo "❌ No backup config found either."
        echo "Please ensure wrangler.production.toml exists."
        exit 1
    fi
fi

# Change to the dp1-feed directory (needed for wrangler context)
cd "$DP1_FEED_DIR"

# Optional: Run validation first (can be disabled with --skip-validation)
if [ "$1" != "--skip-validation" ]; then
    echo "🧪 Running tests and validation..."
    npm run validate || {
        echo ""
        echo "⚠️  Validation failed. Deploy anyway? (y/N)"
        read -r response
        if [[ ! "$response" =~ ^[Yy]$ ]]; then
            echo "Deployment cancelled."
            exit 1
        fi
    }
    echo ""
fi

# Deploy using the production config
echo "📦 Deploying with wrangler.production.toml..."
wrangler deploy --config wrangler.production.toml

echo ""
echo "✅ Deployment complete!"
echo "🌐 URL: https://aesthetic-feed.aesthetic-computer.workers.dev"
