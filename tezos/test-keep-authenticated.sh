#!/bin/bash
#
# Test keep-mint endpoint with authentication
#
# Usage: ./test-keep-authenticated.sh <piece-name>
# Note: Run 'ac-login' first to authenticate
#

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <piece-name>"
    echo "Example: $0 test-keep-123"
    echo ""
    echo "Authenticate first with: ac-login"
    exit 1
fi

PIECE=$1
ENDPOINT="${ENDPOINT:-https://localhost:8888/api/keep-mint}"

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║  🧪 Testing Keep-Mint (Authenticated)                        ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""

# Check if AC_TOKEN is set (from ac-login fish function)
if [ -z "$AC_TOKEN" ]; then
    # Try to load from file as fallback
    TOKEN_FILE="$HOME/.ac-token"
    if [ ! -f "$TOKEN_FILE" ]; then
        echo "❌ Not logged in"
        echo ""
        echo "Run first: ac-login"
        exit 1
    fi
    
    # Extract access token from file
    ACCESS_TOKEN=$(node -e "console.log(JSON.parse(require('fs').readFileSync('$TOKEN_FILE', 'utf8')).access_token)")
else
    # Use environment variable from ac-login
    ACCESS_TOKEN="$AC_TOKEN"
fi

if [ -z "$ACCESS_TOKEN" ]; then
    echo "❌ Could not read access token"
    exit 1
fi

echo "✅ Authenticated"
if [ -n "$AC_USER_EMAIL" ]; then
    echo "👤 User:     $AC_USER_EMAIL"
fi
echo "📍 Piece:    $PIECE"
echo "🌐 Endpoint: $ENDPOINT"
echo "🔧 Mode:     mint (server-side)"
echo ""

# Make authenticated request
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📤 Sending authenticated request..."
echo ""

curl -N -X POST "$ENDPOINT" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $ACCESS_TOKEN" \
  -d "{\"piece\": \"$PIECE\", \"mode\": \"mint\"}" \
  2>&1 | while IFS= read -r line; do
    # Parse SSE events
    if [[ "$line" =~ ^data:\ (.*)$ ]]; then
        DATA="${BASH_REMATCH[1]}"
        
        # Pretty print JSON if jq is available
        if command -v jq &> /dev/null; then
            echo "$DATA" | jq -C '.'
        else
            echo "$DATA"
        fi
        echo ""
    fi
done

echo ""
echo "✅ Test complete"
