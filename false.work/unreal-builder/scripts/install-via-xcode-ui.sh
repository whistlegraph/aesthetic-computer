#!/bin/bash
# Install SpiderLily via Xcode UI automation

source /workspaces/aesthetic-computer/aesthetic-computer-vault/false.work/mac-builder-credentials.env

APP_PATH="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app"

echo "🎯 Opening Xcode Devices window..."

sshpass -p "$MAC_PASSWORD" ssh -o StrictHostKeyChecking=no falsework@host.docker.internal << 'ENDSSH'
# Open Xcode Devices window
osascript << 'EOF'
tell application "Xcode"
    activate
    delay 1
end tell

tell application "System Events"
    tell process "Xcode"
        -- Open Devices window (Cmd+Shift+2)
        keystroke "2" using {command down, shift down}
        delay 2
    end tell
end tell
EOF
ENDSSH

echo ""
echo "✅ Xcode Devices window opened!"
echo ""
echo "Manual steps:"
echo "  1. Select 'aesthetic.computer' device from the left sidebar"
echo "  2. Click the '+' button under 'Installed Apps'"
echo "  3. Navigate to: $APP_PATH"
echo "  4. Click 'Open' to install"
echo ""
echo "The app will install and you can launch it from your phone!"
