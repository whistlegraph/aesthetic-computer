#!/usr/bin/env fish
# Deploy grab worker with automatic DNS configuration
# Based on the nanos/conductor.mjs pattern

set -e

echo "🚀 Deploying grab worker with automatic DNS setup..."
echo ""

# Load environment variables from vault
set VAULT_DIR /workspaces/aesthetic-computer/aesthetic-computer-vault/grab
if test -f $VAULT_DIR/.env
    source $VAULT_DIR/.env
    echo "✅ Loaded environment variables from vault"
else
    echo "⚠️  Warning: No .env file found in vault"
    echo "    Continuing with system environment variables..."
end

echo ""

# Step 1: Deploy the worker
echo "📦 Step 1: Deploying worker to Cloudflare..."
cd /workspaces/aesthetic-computer/grab
npx wrangler deploy

echo ""
echo "✅ Worker deployed successfully!"
echo ""

# Step 2: Configure DNS via Cloudflare API
echo "🌐 Step 2: Configuring DNS (CNAME record)..."

# Check for required environment variables
if not set -q CLOUDFLARE_EMAIL
    echo "❌ Error: CLOUDFLARE_EMAIL not set"
    echo "   Please add to $VAULT_DIR/.env"
    exit 1
end

if not set -q CLOUDFLARE_API_TOKEN
    echo "❌ Error: CLOUDFLARE_API_TOKEN not set"
    echo "   Please add to $VAULT_DIR/.env"
    exit 1
end

# Cloudflare configuration
set ZONE_ID "a23b54e8877a833a1cf8db7765bce3ca"
set SUBDOMAIN "grab"
set FULL_DOMAIN "grab.aesthetic.computer"
set TARGET "aesthetic-grab.aesthetic-computer.workers.dev"

echo "   Zone: aesthetic.computer ($ZONE_ID)"
echo "   Creating: $FULL_DOMAIN → $TARGET"
echo ""

# Step 2a: Check if DNS record already exists
echo "   Checking for existing DNS record..."
set CHECK_RESPONSE (curl -s -X GET \
    "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records?type=CNAME&name=$FULL_DOMAIN" \
    -H "X-Auth-Email: $CLOUDFLARE_EMAIL" \
    -H "X-Auth-Key: $CLOUDFLARE_API_TOKEN" \
    -H "Content-Type: application/json")

set RECORD_ID (echo $CHECK_RESPONSE | jq -r '.result[0].id // empty')

# Step 2b: Create or update DNS record
if test -n "$RECORD_ID"
    echo "   🟡 Updating existing CNAME record (ID: $RECORD_ID)..."
    set DNS_RESPONSE (curl -s -X PUT \
        "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records/$RECORD_ID" \
        -H "X-Auth-Email: $CLOUDFLARE_EMAIL" \
        -H "X-Auth-Key: $CLOUDFLARE_API_TOKEN" \
        -H "Content-Type: application/json" \
        --data "{
            \"type\": \"CNAME\",
            \"name\": \"$SUBDOMAIN\",
            \"content\": \"$TARGET\",
            \"ttl\": 1,
            \"proxied\": true
        }")
else
    echo "   🟡 Creating new CNAME record..."
    set DNS_RESPONSE (curl -s -X POST \
        "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records" \
        -H "X-Auth-Email: $CLOUDFLARE_EMAIL" \
        -H "X-Auth-Key: $CLOUDFLARE_API_TOKEN" \
        -H "Content-Type: application/json" \
        --data "{
            \"type\": \"CNAME\",
            \"name\": \"$SUBDOMAIN\",
            \"content\": \"$TARGET\",
            \"ttl\": 1,
            \"proxied\": true
        }")
end

# Check if DNS operation was successful
set DNS_SUCCESS (echo $DNS_RESPONSE | jq -r '.success')

if test "$DNS_SUCCESS" = "true"
    echo "   ✅ DNS record configured successfully!"
else
    echo "   ❌ DNS configuration failed:"
    echo $DNS_RESPONSE | jq -r '.errors[]?.message // "Unknown error"'
    echo ""
    echo "   You can manually configure via:"
    echo "   https://dash.cloudflare.com/$ZONE_ID/aesthetic.computer/dns/records"
    exit 1
end

echo ""

# Step 3: Wait for DNS propagation
echo "⏳ Step 3: Waiting for DNS propagation..."
echo "   This usually takes 30-60 seconds..."
echo ""

sleep 5

# Try to verify DNS is working
set MAX_ATTEMPTS 12
set ATTEMPT 1

while test $ATTEMPT -le $MAX_ATTEMPTS
    echo -n "   Attempt $ATTEMPT/$MAX_ATTEMPTS: "
    
    set HTTP_CODE (curl -s -o /dev/null -w "%{http_code}" "https://$FULL_DOMAIN/icon/128x128/prompt.png" 2>/dev/null)
    
    if test "$HTTP_CODE" = "200"
        echo "✅ DNS working!"
        break
    else
        echo "⏳ Waiting... (HTTP $HTTP_CODE)"
        if test $ATTEMPT -eq $MAX_ATTEMPTS
            echo ""
            echo "   ⚠️  DNS may still be propagating. Manual check:"
            echo "   dig $FULL_DOMAIN"
            echo "   curl -I https://$FULL_DOMAIN/icon/128x128/prompt.png"
        else
            sleep 5
        end
    end
    
    set ATTEMPT (math $ATTEMPT + 1)
end

echo ""

# Step 4: Success summary
echo "✨ Deployment complete!"
echo ""
echo "📊 Deployment Summary:"
echo "   Worker URL:  https://aesthetic-grab.aesthetic-computer.workers.dev"
echo "   Custom URL:  https://$FULL_DOMAIN"
echo "   DNS Record:  $FULL_DOMAIN → $TARGET (CNAME, Proxied)"
echo ""
echo "🧪 Test Commands:"
echo "   curl -I \"https://$FULL_DOMAIN/icon/128x128/prompt.png\""
echo "   curl -I \"https://$FULL_DOMAIN/preview/1200x630/prompt.png\""
echo ""
echo "📋 Next Steps:"
echo "   1. Test screenshot generation with various pieces"
echo "   2. Monitor logs: npx wrangler tail"
echo "   3. Check Cloudflare dashboard for errors"
echo "   4. Validate production integration on aesthetic.computer"
echo ""
