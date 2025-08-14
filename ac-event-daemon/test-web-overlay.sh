#!/bin/bash

# Test the web overlay functionality

echo "🌐 Testing web overlay..."

# Test the static overlay page directly
echo "📱 Opening overlay page in browser for testing..."

if command -v xdg-open >/dev/null 2>&1; then
    xdg-open "https://aesthetic.computer/overlay.html?type=success&word=TEST&duration=5000"
elif command -v open >/dev/null 2>&1; then
    open "https://aesthetic.computer/overlay.html?type=success&word=TEST&duration=5000"
else
    echo "🌍 Navigate to: https://aesthetic.computer/overlay.html?type=success&word=TEST&duration=5000"
fi

echo ""
echo "🧪 Testing different notification types:"
echo "✅ Success: https://aesthetic.computer/overlay.html?type=success&word=SUCCESS&duration=3000"
echo "❌ Error: https://aesthetic.computer/overlay.html?type=error&word=ERROR&duration=3000"
echo "ℹ️  Info: https://aesthetic.computer/overlay.html?type=info&word=INFO&duration=3000"
echo "⚠️  Warning: https://aesthetic.computer/overlay.html?type=warning&word=WARNING&duration=3000"

echo ""
echo "📋 URL Parameters:"
echo "  type: success|error|info|warning"
echo "  word: The text to display (will be uppercased)"
echo "  duration: Display time in milliseconds (default: 2500)"
