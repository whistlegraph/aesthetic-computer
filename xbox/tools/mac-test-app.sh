#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
app="$repo_root/xbox/builds/mac-test/OSKIEWAR.app"
contents="$app/Contents"

rm -rf "$app"
mkdir -p "$contents/MacOS" "$contents/Resources/live"
swiftc -O "$repo_root/xbox/mac-test/main.swift" \
  -framework Cocoa -framework WebKit \
  -o "$contents/MacOS/Oskiewar"
cp "$repo_root/xbox/mac-test/Info.plist" "$contents/Info.plist"
cp "$repo_root/xbox/live/mac-test.html" "$contents/Resources/live/mac-test.html"
cp "$repo_root/xbox/live/hello.js" "$contents/Resources/live/hello.js"
cp "$repo_root/system/public/type/webfonts/ywft-processing-regular.ttf" \
  "$contents/Resources/live/ywft-processing-regular.ttf"

echo "Mac fight test: $app"
open "$app"
