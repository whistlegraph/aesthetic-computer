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
  dock_snapshot=$(mktemp "${TMPDIR:-/tmp}/nopaint-dock.XXXXXX.plist")
  dock_found=0
  if defaults export com.apple.dock "$dock_snapshot" >/dev/null 2>&1; then
    dock_index=0
    while dock_url=$(/usr/libexec/PlistBuddy -c \
        "Print :persistent-apps:$dock_index:tile-data:file-data:_CFURLString" \
        "$dock_snapshot" 2>/dev/null); do
      normalized_url=$(printf '%s' "$dock_url" | sed 's/%20/ /g' | tr '[:upper:]' '[:lower:]')
      if [ "$normalized_url" = "file:///applications/no paint.app/" ]; then
        /usr/libexec/PlistBuddy -c \
          "Set :persistent-apps:$dock_index:tile-data:file-data:_CFURLString file:///Applications/No%20Paint.app/" \
          "$dock_snapshot"
        /usr/libexec/PlistBuddy -c \
          "Set :persistent-apps:$dock_index:tile-data:file-label No Paint" \
          "$dock_snapshot"
        defaults import com.apple.dock "$dock_snapshot" >/dev/null
        dock_found=1
        break
      fi
      dock_index=$((dock_index + 1))
    done
  fi
  rm -f "$dock_snapshot"
  if [ "$dock_found" -eq 0 ]; then
    defaults write com.apple.dock persistent-apps -array-add \
      '{"tile-data"={"file-data"={"_CFURLString"="file:///Applications/No%20Paint.app/";"_CFURLStringType"=15;};"file-label"="No Paint";};"tile-type"="file-tile";}'
  fi
  killall Dock 2>/dev/null || true
  open "$destination"
  echo "Installed, pinned, and opened $destination"
else
  echo "Built $app"
fi
