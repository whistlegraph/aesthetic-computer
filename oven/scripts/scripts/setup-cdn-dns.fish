#!/usr/bin/env fish
# Setup Cloudflare CNAME for at-blobs.aesthetic.computer → DigitalOcean Space
# Provides a cleaner CDN URL for tape videos and thumbnails

echo "🌐 Setting up CDN domain for at-blobs Space..."
echo ""

# Load environment variables from vault (directly set them)
set -gx CLOUDFLARE_ACCOUNT_ID "a23b54e8877a833a1cf8db7765bce3ca"
set -gx CLOUDFLARE_EMAIL "me@jas.life"
set -gx CLOUDFLARE_API_KEY "0346704765b61e560b36592010c98a23bc2c6"
set -gx ZONE_ID "da794a6ae8f17b80424907f81ed0db7c"

echo "✅ Loaded Cloudflare credentials"
echo ""

# Cloudflare configuration
set SUBDOMAIN "at-blobs"
set FULL_DOMAIN "at-blobs.aesthetic.computer"
set TARGET "at-blobs-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com"

echo "   Zone: aesthetic.computer ($ZONE_ID)"
echo "   Creating: $FULL_DOMAIN → $TARGET"
echo ""

# Check if DNS record already exists
echo "   Checking for existing DNS record..."
set CHECK_RESPONSE (curl -s -X GET \
    "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records?type=CNAME&name=$FULL_DOMAIN" \
    -H "X-Auth-Email: $CLOUDFLARE_EMAIL" \
    -H "X-Auth-Key: $CLOUDFLARE_API_KEY" \
    -H "Content-Type: application/json")

set RECORD_ID (echo $CHECK_RESPONSE | jq -r '.result[0].id // empty')

if test -n "$RECORD_ID"
    echo "   Found existing record: $RECORD_ID"
    echo "   Updating..."
    
    set UPDATE_RESPONSE (curl -s -X PUT \
        "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records/$RECORD_ID" \
        -H "X-Auth-Email: $CLOUDFLARE_EMAIL" \
        -H "X-Auth-Key: $CLOUDFLARE_API_KEY" \
        -H "Content-Type: application/json" \
        --data "{
            \"type\": \"CNAME\",
            \"name\": \"$SUBDOMAIN\",
            \"content\": \"$TARGET\",
            \"ttl\": 1,
            \"proxied\": true
        }")
    
    if echo $UPDATE_RESPONSE | jq -e '.success' > /dev/null
        echo "   ✅ DNS record updated!"
    else
        echo "   ❌ Failed to update DNS record"
        echo $UPDATE_RESPONSE | jq
        exit 1
    end
else
    echo "   No existing record found. Creating new..."
    
    set CREATE_RESPONSE (curl -s -X POST \
        "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records" \
        -H "X-Auth-Email: $CLOUDFLARE_EMAIL" \
        -H "X-Auth-Key: $CLOUDFLARE_API_KEY" \
        -H "Content-Type: application/json" \
        --data "{
            \"type\": \"CNAME\",
            \"name\": \"$SUBDOMAIN\",
            \"content\": \"$TARGET\",
            \"ttl\": 1,
            \"proxied\": true
        }")
    
    if echo $CREATE_RESPONSE | jq -e '.success' > /dev/null
        echo "   ✅ DNS record created!"
    else
        echo "   ❌ Failed to create DNS record"
        echo $CREATE_RESPONSE | jq
        exit 1
    end
end

echo ""
echo "✅ CDN domain configured successfully!"
echo ""
echo "📋 Summary:"
echo "   Domain: https://$FULL_DOMAIN"
echo "   Target: $TARGET"
echo "   Proxied: Yes (Cloudflare CDN)"
echo "   SSL: Automatic (Cloudflare Universal SSL)"
echo ""
echo "🔗 Manage DNS:"
echo "   https://dash.cloudflare.com/$ZONE_ID/aesthetic.computer/dns/records"
echo ""
echo "⏱️  Note: DNS propagation may take a few minutes"
echo "   Test with: curl -I https://$FULL_DOMAIN/tapes/"
