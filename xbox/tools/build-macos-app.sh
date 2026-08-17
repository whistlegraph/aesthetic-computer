#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
build_root="$repo_root/xbox/builds/macos"
app="$build_root/oskiewar.app"
contents="$app/Contents"
install=0
if [ "${1:-}" = "--install" ]; then install=1; fi

rm -rf "$app"
mkdir -p "$contents/MacOS" "$contents/Resources/live"
# The Metal scene and its glyph atlas are shared with the iOS app under
# apple/oskiewar — one renderer, compiled into both. main.swift keeps its
# top-level code as the entry point, which stays legal as long as it is the
# file literally named main.swift.
swiftc -swift-version 5 -O "$repo_root/xbox/macos-native/main.swift" \
  "$repo_root/apple/oskiewar/Sources/MetalSceneView.swift" \
  "$repo_root/apple/oskiewar/Sources/GlyphAtlas.swift" \
  -framework AppKit -framework AVFoundation -framework CoreVideo \
  -framework GameController -framework JavaScriptCore \
  -framework Metal -framework MetalKit -framework CoreText \
  -o "$contents/MacOS/oskiewar"
cp "$repo_root/xbox/macos-native/Info.plist" "$contents/Info.plist"
cp "$repo_root/xbox/live/oskiewar.js" "$contents/Resources/live/oskiewar.js"
cp "$repo_root/system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs" \
  "$contents/Resources/live/qr.mjs"
cp "$repo_root/system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf" \
  "$contents/Resources/ComicRelief-Regular.ttf"

sh "$repo_root/xbox/tools/make-macos-icon.sh" "$contents/Resources/Oskiewar.icns"
# Ad-hoc, but against the App Store entitlements: the desk build is sandboxed
# and JIT-enabled exactly like the reviewed one, so a sandbox denial shows up
# here rather than in review — and JavaScriptCore gets its JIT, which is worth
# roughly 4x on the game loop.
codesign --force --deep --options runtime \
  --entitlements "$repo_root/apple/oskiewar-mac/Oskiewar-macOS.entitlements" \
  --sign - "$app" >/dev/null

if [ "$install" -eq 1 ]; then
  destination="/Applications/oskiewar.app"
  rm -rf "$destination"
  ditto "$app" "$destination"
  dock_snapshot=$(mktemp "${TMPDIR:-/tmp}/oskiewar-dock.XXXXXX.plist")
  dock_found=0
  if defaults export com.apple.dock "$dock_snapshot" >/dev/null 2>&1; then
    dock_index=0
    while dock_url=$(/usr/libexec/PlistBuddy -c \
        "Print :persistent-apps:$dock_index:tile-data:file-data:_CFURLString" \
        "$dock_snapshot" 2>/dev/null); do
      if [ "$(printf '%s' "$dock_url" | tr '[:upper:]' '[:lower:]')" = \
          "file:///applications/oskiewar.app/" ]; then
        /usr/libexec/PlistBuddy -c \
          "Set :persistent-apps:$dock_index:tile-data:file-data:_CFURLString file:///Applications/oskiewar.app/" \
          "$dock_snapshot"
        /usr/libexec/PlistBuddy -c \
          "Set :persistent-apps:$dock_index:tile-data:file-label oskiewar" \
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
      '{"tile-data"={"file-data"={"_CFURLString"="file:///Applications/oskiewar.app/";"_CFURLStringType"=15;};"file-label"="oskiewar";};"tile-type"="file-tile";}'
  fi
  killall Dock 2>/dev/null || true
  open "$destination"
  echo "Installed and opened $destination"
else
  echo "Built $app"
fi
