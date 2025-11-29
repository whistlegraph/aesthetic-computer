#!/usr/bin/env fish
# Test Chat Integration, 25.11.28
# CLI test script for chat functionality

set HOST localhost:8889

echo "🧪 Chat Integration Tests"
echo "========================="
echo ""

# Test 1: HTTP health check (main session server)
echo "📍 Test 1: Session Server Health"
set response (curl -sk https://$HOST/ 2>&1)
if string match -q "*session-server*" $response
    echo "   ✅ Session server responding"
else
    echo "   ⚠️  Response: $response"
end
echo ""

# Test 2: Chat status endpoint
echo "📍 Test 2: Chat Status Endpoint"
set response (curl -sk https://$HOST/chat/status 2>&1)
if string match -q "*chat-system*" $response
    echo "   ✅ Chat status endpoint working"
    echo "   📊 $response"
else
    echo "   ⚠️  Response: $response"
end
echo ""

# Test 3: WebSocket connection (requires websocat or similar)
echo "📍 Test 3: WebSocket Connection"
if command -v websocat > /dev/null
    set ws_response (echo '{"type":"ping"}' | timeout 2 websocat -1 --insecure wss://$HOST/ 2>&1)
    if test $status -eq 0
        echo "   ✅ WebSocket connection successful"
    else
        echo "   ⚠️  WebSocket test requires chat host header"
    end
else
    echo "   ⏭️  Skipped (websocat not installed)"
end
echo ""

# Test 4: Log endpoint (requires auth)
echo "📍 Test 4: Log Endpoint (Auth Required)"
set response (curl -sk -X POST https://$HOST/chat/log \
    -H "Authorization: Bearer invalid" \
    -H "Content-Type: application/json" \
    -d '{"text":"Test message"}' 2>&1)
if string match -q "*Forbidden*" $response
    echo "   ✅ Auth check working (correctly rejected invalid token)"
else
    echo "   ⚠️  Response: $response"
end
echo ""

echo "========================="
echo "🏁 Tests complete!"
echo ""
echo "For full WebSocket testing, connect to:"
echo "   wss://chat-system.aesthetic.computer"
echo "   wss://chat-clock.aesthetic.computer"
echo "   wss://chat.sotce.net"
