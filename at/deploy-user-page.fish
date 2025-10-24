#!/usr/bin/env fish
# Deploy ATProto user page to art.at.aesthetic.computer
# Usage: ./deploy-user-page.fish

set SOURCE /workspaces/aesthetic-computer/at/user-page.html
set SERVER root@165.227.120.137
set DEST_TMP /tmp/user.html
set DEST_CONTAINER caddy:/data/www/user.html

echo "📤 Uploading user page to server..."
scp -i ~/.ssh/aesthetic_pds $SOURCE $SERVER:$DEST_TMP

echo "🐳 Copying into Caddy container..."
printf "docker cp $DEST_TMP $DEST_CONTAINER\n" | ac-at

echo "✅ Deployment complete!"
echo "🌐 View at: https://art.at.aesthetic.computer"
