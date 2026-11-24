#!/bin/bash
# test-notification.sh - Test the AC notification overlay system

echo "🧪 Testing AC Notification Overlay System"
echo ""

# Check if daemon is running
if pgrep -f "ac-event-daemon" > /dev/null; then
    echo "✅ Daemon is running"
else
    echo "❌ Daemon is not running"
    echo "Start it with: cd ac-event-daemon && ./target/release/ac-event-daemon"
    exit 1
fi

# Check if HTTP server is running
if curl -s http://localhost:3333/overlay-test.html > /dev/null; then
    echo "✅ HTTP server is serving overlay files"
else
    echo "❌ HTTP server not accessible"
    echo "Start it with: cd system/public && python3 -m http.server 3333"
    exit 1
fi

echo ""
echo "🚀 Sending test notifications..."

# Test different notification types
echo "Sending SUCCESS notification..."
echo "prompt-complete:success:Build Complete!" | nc -u 127.0.0.1 9999

sleep 2

echo "Sending ERROR notification..."
echo "prompt-complete:error:Build Failed!" | nc -u 127.0.0.1 9999

sleep 2

echo "Sending INFO notification..."
echo "prompt-complete:info:Processing..." | nc -u 127.0.0.1 9999

sleep 2

echo "Sending WARNING notification..."
echo "prompt-complete:warning:Low Memory" | nc -u 127.0.0.1 9999

echo ""
echo "✨ Test complete!"
echo ""
echo "Note: In a dev container, Tauri may show permission errors."
echo "The system will fall back to browser-based overlays automatically."
