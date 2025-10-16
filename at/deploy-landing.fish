#!/usr/bin/env fish
# Deploy ATProto landing page to at.aesthetic.computer
# Usage: ./deploy-landing.fish

set SOURCE /workspaces/aesthetic-computer/at/landing-page.html
set SERVER root@165.227.120.137
set DEST_HOST /var/www/at.aesthetic.computer/index.html
set DEST_CONTAINER caddy:/var/www/at.aesthetic.computer/index.html

echo "📤 Uploading landing page to server..."
scp -i ~/.ssh/aesthetic_pds $SOURCE $SERVER:$DEST_HOST

echo "🐳 Copying into Caddy container..."
printf "docker cp $DEST_HOST $DEST_CONTAINER\n" | ac-at

echo "✅ Deployment complete!"
echo "🌐 View at: https://at.aesthetic.computer"
