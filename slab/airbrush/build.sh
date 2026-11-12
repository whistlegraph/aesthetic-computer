#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
swift build -c release
bin="$(swift build -c release --show-bin-path)"
app="$PWD/build/Airbrush.app"
mkdir -p "$app/Contents/MacOS"
cp "$bin/Airbrush" "$app/Contents/MacOS/Airbrush"
cp Info.plist "$app/Contents/Info.plist"
codesign --force --sign - "$app"
echo "$app"
