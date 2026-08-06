#!/bin/zsh
set -euo pipefail

app="$HOME/Applications/BlueberryWallpaper.app"
source_file="${0:A:h}/BlueberryWallpaper.swift"
mkdir -p "$app/Contents/MacOS"
xcrun swiftc -O -framework AppKit -framework QuartzCore \
  "$source_file" -o "$app/Contents/MacOS/BlueberryWallpaper"
codesign --force --sign - "$app"
