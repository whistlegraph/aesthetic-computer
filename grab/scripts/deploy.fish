#!/usr/bin/env fish
# Deployment script for grab worker
# Usage: ./deploy.fish [production]

set -l ENV "dev"
if test (count $argv) -gt 0
    set ENV $argv[1]
end

echo "🚀 Deploying grab worker to $ENV..."

# Ensure we're in the right directory
cd (dirname (status -f))
cd ..

# Type check
echo "📝 Running type check..."
npm run type-check
if test $status -ne 0
    echo "❌ Type check failed!"
    exit 1
end

# Copy production config if deploying to production
if test "$ENV" = "production"
    echo "📋 Copying production config from vault..."
    
    if test -f ../aesthetic-computer-vault/grab/wrangler.production.toml
        cp ../aesthetic-computer-vault/grab/wrangler.production.toml ./wrangler.toml
        echo "✅ Production config copied"
    else
        echo "❌ Production config not found in vault!"
        exit 1
    end
end

# Deploy
echo "🚢 Deploying to Cloudflare..."
npx wrangler deploy

if test $status -eq 0
    echo "✅ Deployment successful!"
    
    if test "$ENV" = "production"
        echo ""
        echo "🌐 Worker deployed to: https://grab.aesthetic.computer"
        echo "📊 View metrics: https://dash.cloudflare.com/"
        echo "📝 View logs: npm run logs:production"
    else
        echo ""
        echo "🌐 Worker deployed to dev"
        echo "📝 View logs: npm run logs"
    end
else
    echo "❌ Deployment failed!"
    exit 1
end
