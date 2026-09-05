#!/bin/sh
# Render the shared native-app icon master into a macOS .icns at the path given.
#
# Two builders want the same icon and neither should own the recipe: the
# hand-assembled dev bundle (build-macos-app.sh) and the Xcode target that
# archives for the App Store (apple/oskiewar-mac). 512@2x is the largest rep
# iconutil is asked for, which is the 1024px the store requires.
set -eu

out=${1:?usage: make-macos-icon.sh <path/to/Oskiewar.icns>}
repo_root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
# The mac app wears the same face as the iOS app — one identity per game.
master="$repo_root/apple/oskiewar/Assets.xcassets/AppIcon.appiconset/AppIcon.png"

work=$(mktemp -d "${TMPDIR:-/tmp}/oskiewar-icon.XXXXXX")
trap 'rm -rf "$work"' EXIT
iconset="$work/Oskiewar.iconset"
mkdir -p "$iconset" "$(dirname "$out")"

for size in 16 32 128 256 512; do
  sips -s format png -z "$size" "$size" "$master" \
    --out "$iconset/icon_${size}x${size}.png" >/dev/null
  double=$((size * 2))
  sips -s format png -z "$double" "$double" "$master" \
    --out "$iconset/icon_${size}x${size}@2x.png" >/dev/null
done

iconutil -c icns "$iconset" -o "$out"
