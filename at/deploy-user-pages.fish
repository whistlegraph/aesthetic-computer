#!/usr/bin/env fish
# Deploy user pages to at.aesthetic.computer PDS
# This updates the Caddy config to serve custom user pages on wildcard subdomains
# Usage: fish deploy-user-pages.fish

# Get the directory where this script lives
set SCRIPT_DIR (dirname (status --current-filename))

set LANDING_PAGE_LOCAL "$SCRIPT_DIR/landing-page.html"
set USER_PAGE_LOCAL "$SCRIPT_DIR/user-page.html"
set PDS_SERVER "root@165.227.120.137"
set WEB_ROOT "/pds/caddy/data/www"
set CADDY_WEB_ROOT "/data/www"
set SSH_KEY "$HOME/.ssh/aesthetic_pds"

echo "🚀 Deploying user pages to at.aesthetic.computer"
echo ""

# Check if files exist
if not test -f $LANDING_PAGE_LOCAL
    echo "❌ Error: landing-page.html not found"
    exit 1
end

if not test -f $USER_PAGE_LOCAL
    echo "❌ Error: user-page.html not found"
    exit 1
end

echo "📤 Uploading pages to PDS server..."

# SSH into server and create web root if it doesn't exist
ssh -i $SSH_KEY $PDS_SERVER "mkdir -p $WEB_ROOT"

# Upload both pages
scp -i $SSH_KEY $LANDING_PAGE_LOCAL "$PDS_SERVER:$WEB_ROOT/index.html"
scp -i $SSH_KEY $USER_PAGE_LOCAL "$PDS_SERVER:$WEB_ROOT/user.html"

if test $status -ne 0
    echo "❌ Failed to upload pages"
    exit 1
end

echo "✅ Pages uploaded successfully"
echo ""
echo "🔧 Configuring Caddy to serve custom user pages..."

# SSH into server and update Caddyfile
ssh -i $SSH_KEY $PDS_SERVER '
    # Backup existing Caddyfile
    cp /pds/caddy/etc/caddy/Caddyfile /pds/caddy/etc/caddy/Caddyfile.backup
    
    # Create new Caddyfile with custom pages
    cat > /pds/caddy/etc/caddy/Caddyfile << "EOF"
{
    email me@jas.life
    on_demand_tls {
        ask http://localhost:3000/tls-check
    }
}

# Main domain with custom landing page
at.aesthetic.computer {
    tls {
        on_demand
    }
    
    # Serve custom landing page for root
    handle / {
        root * /data/www
        file_server
    }
    
    # Proxy all other requests to PDS
    handle {
        reverse_proxy http://localhost:3000
    }
}

# Wildcard for user handles - serve custom user page at root
*.at.aesthetic.computer {
    tls {
        on_demand
    }
    
    root * /data/www
    
    # Proxy /xrpc/* to PDS for API access
    handle /xrpc/* {
        reverse_proxy http://localhost:3000
    }
    
    # Proxy .well-known for ATProto
    handle /.well-known/* {
        reverse_proxy http://localhost:3000
    }
    
    # Serve custom user page for root path
    @root path /
    handle @root {
        rewrite * /user.html
        file_server
    }
    
    # Serve all other static files
    handle {
        file_server
    }
}
EOF
    
    # Reload Caddy
    docker exec caddy caddy reload --config /etc/caddy/Caddyfile
'

if test $status -eq 0
    echo ""
    echo "✅ Deployment complete!"
    echo ""
    echo "🌐 Landing page: https://at.aesthetic.computer"
    echo "👤 User pages: https://[handle].at.aesthetic.computer"
    echo "🔍 Health check: https://at.aesthetic.computer/xrpc/_health"
    echo ""
    echo "Example user pages:"
    echo "  https://fifi.at.aesthetic.computer"
    echo "  https://jeffrey.at.aesthetic.computer"
    echo ""
    echo "Note: User pages are served at subdomain root"
    echo "      All /xrpc/* routes still go to the PDS backend for API access"
else
    echo ""
    echo "❌ Deployment failed"
    exit 1
end
