#!/bin/zsh
set -euo pipefail
cd "${0:A:h}"
swift build -c release
mkdir -p "$HOME/.local/bin" "$HOME/.local/share/trackpad-fighter" "$HOME/Library/LaunchAgents"
cp .build/release/trackpad-fighter "$HOME/.local/bin/trackpad-fighter"
ln -sf trackpad-fighter "$HOME/.local/bin/menu-fighter"
chmod 755 "$HOME/.local/bin/trackpad-fighter"
old_plist="$HOME/Library/LaunchAgents/computer.aesthetic.menu-fighter.plist"
launchctl bootout "gui/$UID/computer.aesthetic.menu-fighter" 2>/dev/null || true
plist="$HOME/Library/LaunchAgents/computer.aesthetic.trackpad-fighter.plist"
launchctl bootout "gui/$UID/computer.aesthetic.trackpad-fighter" 2>/dev/null || true
rm -f "$old_plist"
plutil -create xml1 "$plist"
/usr/libexec/PlistBuddy -c 'Add :Label string computer.aesthetic.trackpad-fighter' "$plist"
/usr/libexec/PlistBuddy -c 'Add :ProgramArguments array' "$plist"
/usr/libexec/PlistBuddy -c "Add :ProgramArguments:0 string $HOME/.local/bin/trackpad-fighter" "$plist"
/usr/libexec/PlistBuddy -c 'Add :ProgramArguments:1 string --watch' "$plist"
/usr/libexec/PlistBuddy -c 'Add :RunAtLoad bool true' "$plist"
/usr/libexec/PlistBuddy -c 'Add :KeepAlive bool true' "$plist"
/usr/libexec/PlistBuddy -c "Add :StandardOutPath string $HOME/.local/share/trackpad-fighter/watcher.log" "$plist"
/usr/libexec/PlistBuddy -c "Add :StandardErrorPath string $HOME/.local/share/trackpad-fighter/watcher.log" "$plist"
launchctl bootstrap "gui/$UID" "$plist"
echo "Installed Trackpad Fighter corner-pose watcher."
