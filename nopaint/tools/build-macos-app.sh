#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
build_root="$repo_root/nopaint/builds/macos"
app="$build_root/No Paint.app"
contents="$app/Contents"
install=0
if [ "${1:-}" = "--install" ]; then install=1; fi

rm -rf "$app"
mkdir -p "$contents/MacOS" "$contents/Resources"
xcrun swiftc -swift-version 5 -O "$repo_root/nopaint/macos-native/main.swift" \
  -framework AppKit -framework WebKit \
  -o "$contents/MacOS/nopaint"
cp "$repo_root/nopaint/macos-native/Info.plist" "$contents/Info.plist"

work_dir=$(mktemp -d "${TMPDIR:-/tmp}/nopaint-icon.XXXXXX")
trap 'rm -rf "$work_dir"' EXIT
xcrun swiftc -swift-version 5 -O \
  "$repo_root/nopaint/macos-native/NoPaintIcon.swift" \
  "$repo_root/nopaint/macos-native/render-icon.swift" \
  -framework AppKit -o "$work_dir/render-nopaint-icon"
"$work_dir/render-nopaint-icon" "$build_root/NoPaintIcon-1024.png"

iconset="$work_dir/NoPaint.iconset"
mkdir -p "$iconset"
for spec in "16:icon_16x16.png" "32:icon_16x16@2x.png" \
            "32:icon_32x32.png" "64:icon_32x32@2x.png" \
            "128:icon_128x128.png" "256:icon_128x128@2x.png" \
            "256:icon_256x256.png" "512:icon_256x256@2x.png" \
            "512:icon_512x512.png" "1024:icon_512x512@2x.png"; do
  size=${spec%%:*}
  name=${spec#*:}
  sips -z "$size" "$size" "$build_root/NoPaintIcon-1024.png" \
    --out "$iconset/$name" >/dev/null
done
iconutil -c icns "$iconset" -o "$contents/Resources/NoPaint.icns"
codesign --force --deep --sign - "$app" >/dev/null

if [ "$install" -eq 1 ]; then
  destination="/Applications/No Paint.app"
  rm -rf "$destination"
  ditto "$app" "$destination"
  open "$destination"
  echo "Installed and opened $destination"
else
  echo "Built $app"
fi
