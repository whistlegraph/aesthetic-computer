#!/bin/sh
# Package the IMAB / Butterfly Cosplayer adaptation for iOS.
set -eu
app_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
sips -s format png -z 1024 1024 "$app_dir/Artwork/imab-icon.png" --out "$app_dir/Resources/Assets.xcassets/AppIcon.appiconset/icon.png" >/dev/null
