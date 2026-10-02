#!/bin/sh
# Reuse the published Whistlegraph Dot Org artist mark, without redrawing it.
set -eu
app_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
sips -s format png -z 1024 1024 "$app_dir/../../pop/artist/whistlegraph-dot-org/wgdo-avatar-3000.jpg" --out "$app_dir/Resources/Assets.xcassets/AppIcon.appiconset/icon.png" >/dev/null
