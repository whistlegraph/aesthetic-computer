#!/usr/bin/env fish
# Setup browser rendering binding for Cloudflare Workers
# This script helps create the browser rendering binding

echo "🌐 Setting up Browser Rendering for Cloudflare Workers"
echo ""

# Check if logged in
echo "Checking Cloudflare authentication..."
npx wrangler whoami
if test $status -ne 0
    echo "❌ Not logged in to Cloudflare. Please run: wrangler login"
    exit 1
end

echo ""
echo "✅ Authenticated with Cloudflare"
echo ""

# Note: Browser Rendering must be enabled in your account
echo "📝 Browser Rendering Setup Instructions:"
echo ""
echo "1. Go to https://dash.cloudflare.com/"
echo "2. Select your account"
echo "3. Navigate to Workers & Pages"
echo "4. Click on 'Browser Rendering' in the sidebar"
echo "5. Click 'Enable Browser Rendering'"
echo ""
echo "Once enabled, Browser Rendering will be available in your workers."
echo ""
echo "📖 Documentation: https://developers.cloudflare.com/browser-rendering/"
echo ""

read -P "Have you enabled Browser Rendering in your account? (y/n) " -l confirm
if test "$confirm" != "y"
    echo "Please enable Browser Rendering first, then run this script again."
    exit 1
end

echo ""
echo "✅ Browser Rendering is enabled!"
echo ""
echo "The browser binding will be configured in wrangler.toml:"
echo ""
echo "[[browser]]"
echo "binding = \"BROWSER\""
echo ""
echo "This binding is automatically available when you deploy your worker."
echo ""
echo "🎉 Setup complete! You can now deploy your worker."
