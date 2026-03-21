#!/bin/bash
#
# Test the keep-mint endpoint with a real piece
#
# Usage: ./test-keep-endpoint.sh <piece-name>
#

if [ -z "$1" ]; then
    echo "Usage: $0 <piece-name>"
    echo "Example: $0 test-piece-123"
    exit 1
fi

PIECE=$1
ENDPOINT="http://localhost:8888/api/keep-mint"

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║  🧪 Testing Keep-Mint Endpoint                               ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""
echo "📍 Piece:    $PIECE"
echo "🌐 Endpoint: $ENDPOINT"
echo "🔧 Mode:     mint (server-side)"
echo ""

# Make request
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📤 Sending request..."
echo ""

curl -N -X POST "$ENDPOINT" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer test-token" \
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
