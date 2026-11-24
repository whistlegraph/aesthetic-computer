#!/bin/bash

# Test overlay methods to see which ones work on this system

echo "🧪 Testing overlay methods..."
echo "==============================================="

test_overlay() {
    local method="$1"
    local command="$2"
    echo -n "Testing $method... "
    
    if command -v "${command%% *}" >/dev/null 2>&1; then
        echo "✅ Available"
        return 0
    else
        echo "❌ Not found"
        return 1
    fi
}

# Test available overlay tools
test_overlay "Google Chrome" "google-chrome"
test_overlay "Chromium" "chromium"
test_overlay "Chromium Browser" "chromium-browser"
test_overlay "Firefox" "firefox"
test_overlay "Zenity" "zenity"
test_overlay "YAD" "yad"
test_overlay "XMessage" "xmessage"
test_overlay "GXMessage" "gxmessage"
test_overlay "Rofi" "rofi"
test_overlay "Figlet" "figlet"
test_overlay "Toilet" "toilet"

echo ""
echo "🖥️  Display info:"
echo "DISPLAY: ${DISPLAY:-not set}"
echo "XDG_SESSION_TYPE: ${XDG_SESSION_TYPE:-not set}"
echo "USER: ${USER:-not set}"
echo "EUID: $EUID"

echo ""
echo "🔧 Recommended setup for best overlays:"
echo "- Install chromium: sudo dnf install chromium"
echo "- Install zenity: sudo dnf install zenity"
echo "- Install figlet: sudo dnf install figlet"
