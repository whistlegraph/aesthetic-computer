#!/bin/zsh

set -eu

HERE="${0:A:h}"
BUILD="$HERE/build"
APP="$HOME/Applications/Blueberry Cabinet.app"

mkdir -p "$BUILD" "$APP/Contents/MacOS"
mkdir -p "$HOME/Arcade/ctrlr"
swiftc -O -framework AppKit -framework CoreGraphics \
  "$HERE/Sources/main.swift" -o "$BUILD/BlueberryCabinet"
cp "$BUILD/BlueberryCabinet" "$APP/Contents/MacOS/BlueberryCabinet"
cp "$HERE/Info.plist" "$APP/Contents/Info.plist"
cp "$HERE/blueberry.cfg" "$HOME/Arcade/ctrlr/blueberry.cfg"
codesign --force --deep --sign - "$APP"
echo "Installed $APP"
