#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"
HOST="${1:-blueberry}"
APP="build/BlueberryWallpaper.app"
REMOTE_STAGE="/Users/jas/Applications/.BlueberryWallpaper.install-$$.app"

./build.sh
ssh "$HOST" 'mkdir -p /Users/jas/Applications /Users/jas/.local/share/blueberry-wallpaper /Users/jas/Library/LaunchAgents'
rsync -a "$APP/" "$HOST:$REMOTE_STAGE/"

ssh "$HOST" bash -s -- "$REMOTE_STAGE" <<'REMOTE'
set -euo pipefail
stage="$1"
app="/Users/jas/Applications/BlueberryWallpaper.app"
agent="/Users/jas/Library/LaunchAgents/computer.aesthetic.blueberry-wallpaper.plist"
domain="gui/$(id -u)"

launchctl bootout "$domain/computer.aesthetic.blueberry-wallpaper" 2>/dev/null || true
if [[ -e "$app" ]]; then
    backup="/Users/jas/.Trash/BlueberryWallpaper-$(date +%Y%m%d-%H%M%S).app"
    mv "$app" "$backup"
    echo "previous app moved to $backup"
fi
mv "$stage" "$app"
cp "$app/Contents/Resources/computer.aesthetic.blueberry-wallpaper.plist" "$agent"
plutil -lint "$agent"
launchctl bootstrap "$domain" "$agent"
launchctl kickstart -k "$domain/computer.aesthetic.blueberry-wallpaper"
echo "✓ installed and launched $app"
REMOTE
