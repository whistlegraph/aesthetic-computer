#!/bin/bash
# Deploy custom landing page to at.aesthetic.computer PDS
# Usage: bash deploy-landing-page.sh

LANDING_PAGE_LOCAL="../../landing-page.html"
PDS_SERVER="root@138.197.35.160"
WEB_ROOT="/var/www/at.aesthetic.computer"
SSH_KEY="$HOME/.ssh/aesthetic_pds"

echo "🚀 Deploying custom landing page to at.aesthetic.computer"
echo ""

# Check if landing page exists
if [ ! -f "$LANDING_PAGE_LOCAL" ]; then
    echo "❌ Error: landing-page.html not found at $LANDING_PAGE_LOCAL"
    exit 1
fi

echo "📤 Uploading landing page to PDS server..."

# SSH into server and create web root if it doesn't exist
ssh -i "$SSH_KEY" "$PDS_SERVER" "mkdir -p $WEB_ROOT"

# Upload the landing page
scp -i "$SSH_KEY" "$LANDING_PAGE_LOCAL" "$PDS_SERVER:$WEB_ROOT/index.html"

if [ $? -ne 0 ]; then
    echo "❌ Failed to upload landing page"
    exit 1
fi

echo "✅ Landing page uploaded successfully"
echo ""
echo "🔧 Configuring Caddy to serve custom landing page..."

# SSH into server and update Caddyfile
ssh -i "$SSH_KEY" "$PDS_SERVER" << 'ENDSSH'
    # Backup existing Caddyfile
    cp /pds/Caddyfile /pds/Caddyfile.backup
    
    # Create new Caddyfile with custom landing page
    cat > /pds/Caddyfile << 'EOF'
{
    email mail@aesthetic.computer
}

# Main PDS subdomain with custom landing page
at.aesthetic.computer {
    # Serve custom landing page for root
    handle / {
        root * /var/www/at.aesthetic.computer
        file_server
    }
    
    # Proxy all other requests to PDS
    handle {
        reverse_proxy localhost:3000
    }
}

# Wildcard for user handles
*.at.aesthetic.computer {
    reverse_proxy localhost:3000
}

# Legacy pds subdomain (if used)
pds.aesthetic.computer {
    reverse_proxy localhost:3000
}

*.pds.aesthetic.computer {
    reverse_proxy localhost:3000
}
EOF
    
    # Reload Caddy
    docker exec caddy caddy reload --config /etc/caddy/Caddyfile
ENDSSH

if [ $? -eq 0 ]; then
    echo ""
    echo "✅ Deployment complete!"
    echo ""
    echo "🌐 Landing page: https://at.aesthetic.computer"
    echo "🔍 Health check: https://at.aesthetic.computer/xrpc/_health"
    echo "📝 PDS admin: https://pds.aesthetic.computer"
    echo ""
    echo "Note: The landing page is served by Caddy at the root URL"
    echo "      All /xrpc/* routes still go to the PDS backend"
else
    echo ""
    echo "❌ Deployment failed"
    exit 1
fi
