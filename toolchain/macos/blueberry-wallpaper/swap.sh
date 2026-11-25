#!/bin/bash
# On a mini: compile Sources/, swap the binary into the installed app, restart
# the agent. push-minis.sh copies this beside the Sources it ships.
set -e
cd "$(dirname "$0")"
APP=~/Applications/BlueberryWallpaper.app
mkdir -p build/next
swiftc -O -target arm64-apple-macos14.0 -framework AppKit -framework QuartzCore -framework SceneKit -framework AVFoundation \
    -framework CoreBluetooth -framework CoreLocation -framework CoreWLAN Sources/*.swift -o build/next/BlueberryWallpaper
rm -f $APP/Contents/MacOS/BlueberryWallpaper
cp build/next/BlueberryWallpaper $APP/Contents/MacOS/BlueberryWallpaper
# The usage strings come with the binary: macOS kills an app that touches
# Bluetooth without one in its Info.plist.
[ -f Info.plist ] && cp Info.plist $APP/Contents/Info.plist
ENT=(); [ -f entitlements.plist ] && ENT=(--entitlements entitlements.plist)
codesign --force --options runtime "${ENT[@]}" --sign - $APP 2>/dev/null
launchctl kickstart -k gui/$(id -u)/computer.aesthetic.blueberry-wallpaper
# A swapped-in-place binary can trip one transient codesigning kill; retry once.
sleep 6; pgrep -q BlueberryWallpaper || launchctl kickstart -k gui/$(id -u)/computer.aesthetic.blueberry-wallpaper
