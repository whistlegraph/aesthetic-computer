#!/bin/bash
# Rebuild the native icon from the repository's vector mascot, on macOS.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
for tool in rsvg-convert sips iconutil; do
  command -v "$tool" >/dev/null || { echo "Missing $tool (install librsvg for rsvg-convert)." >&2; exit 1; }
done
icon_tmp="$(mktemp -d "${TMPDIR:-/tmp}/easel-icon.XXXXXX")"
trap 'rm -rf "$icon_tmp"' EXIT
mkdir "$icon_tmp/aesel.iconset"
rsvg-convert -w 1024 -h 1024 "$here/icon.svg" -o "$here/icon.png"
for size in 16 32 128 256 512; do
  sips -z "$size" "$size" "$here/icon.png" --out "$icon_tmp/aesel.iconset/icon_${size}x${size}.png" >/dev/null
  double=$((size * 2))
  sips -z "$double" "$double" "$here/icon.png" --out "$icon_tmp/aesel.iconset/icon_${size}x${size}@2x.png" >/dev/null
done
iconutil -c icns "$icon_tmp/aesel.iconset" -o "$here/icon.icns"
echo "Generated icon.png (1024 px) and icon.icns from icon.svg."
