#!/bin/bash
set -e

# Generate timestamp for this build
TIMESTAMP=$(date +%H%M)
echo "🕐 Build timestamp: $TIMESTAMP"

# Update version.h with new timestamp
sed -i '' "s/AC Notepat [0-9]*/AC Notepat $TIMESTAMP/" ~/Desktop/code/aesthetic-computer/ac-vst/source/version.h

echo "🔨 Building ACNotepat..."
cd ~/Desktop/code/aesthetic-computer/ac-vst/build
cmake --build . --target ACNotepat --config Release 2>&1 | grep -E '(error:|warning:|Built target|%)' | tail -10

echo "📦 Installing to VST3 folder..."
rm -rf ~/Library/Audio/Plug-Ins/VST3/ACNotepat.vst3
cp -R ~/Desktop/code/aesthetic-computer/ac-vst/build/VST3/Release/ACNotepat.vst3 ~/Library/Audio/Plug-Ins/VST3/

echo "🔐 Signing with hardened runtime..."
codesign --force --deep --sign - --options runtime ~/Library/Audio/Plug-Ins/VST3/ACNotepat.vst3 2>/dev/null

echo "🗑️  Clearing Ableton plugin cache..."
sqlite3 ~/Library/"Application Support"/Ableton/"Live Database"/Live-plugins-1.db "DELETE FROM plugin_modules WHERE path LIKE '%ACNotepat%'; DELETE FROM plugins WHERE name LIKE '%AC Notepat%';" 2>/dev/null || true

# Force Ableton to rescan by sending AppleScript
echo "🔄 Triggering Ableton rescan..."
osascript -e '
tell application "System Events"
    tell process "Live"
        keystroke "," using command down
    end tell
end tell
' 2>/dev/null || echo "   (Open Preferences manually)"

echo ""
echo "✅ Built: AC Notepat $TIMESTAMP"
echo "   → In Preferences: Plug-ins → Rescan Plug-ins"

