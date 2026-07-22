#!/bin/zsh
set -euo pipefail
cd "${0:A:h}"
swift build -c release
mkdir -p "$HOME/.local/bin" "$HOME/.local/share/menu-fighter" "$HOME/Library/LaunchAgents"
cp .build/release/menu-fighter "$HOME/.local/bin/menu-fighter"
cp "../../pop/samples/whats-inside-your-heart/sfx/Jeffrey count in.wav" "$HOME/.local/share/menu-fighter/jeffrey-count-in.wav"
chmod 755 "$HOME/.local/bin/menu-fighter"
plist="$HOME/Library/LaunchAgents/computer.aesthetic.menu-fighter.plist"
launchctl bootout "gui/$UID/computer.aesthetic.menu-fighter" 2>/dev/null || true
/usr/libexec/PlistBuddy -c Clear "$plist" 2>/dev/null || true
/usr/libexec/PlistBuddy -c 'Add :Label string computer.aesthetic.menu-fighter' "$plist"
/usr/libexec/PlistBuddy -c 'Add :ProgramArguments array' "$plist"
/usr/libexec/PlistBuddy -c "Add :ProgramArguments:0 string $HOME/.local/bin/menu-fighter" "$plist"
/usr/libexec/PlistBuddy -c 'Add :ProgramArguments:1 string --watch' "$plist"
/usr/libexec/PlistBuddy -c 'Add :RunAtLoad bool true' "$plist"
/usr/libexec/PlistBuddy -c 'Add :KeepAlive bool true' "$plist"
launchctl bootstrap "gui/$UID" "$plist"
echo "Installed Menu Fighter corner-pose watcher."
