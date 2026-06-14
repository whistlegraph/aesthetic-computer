#!/bin/sh
# juke-wizard/bin/gen-icons.sh — build light + dark dock icons from the
# JukeWizard mascot illustration (ImageMagick), same recipe as wave/clip.
#   light = mascot full-bleed in a macOS squircle
#   dark  = the same art night-tinted
set -e
REPO="$(cd "$(dirname "$0")/../.." && pwd)"
ASSETS="$REPO/juke-wizard/Sources/JukeWizard/Assets"
MASCOT="$ASSETS/jukewizard-mascot.png"
NAME="jukewizard"
[ -f "$MASCOT" ] || { echo "✗ mascot missing: $MASCOT (run gen-mascot.mjs first)"; exit 1; }

MAGICK="$(command -v magick || command -v convert)"
[ -n "$MAGICK" ] || { echo "✗ ImageMagick not found"; exit 1; }

mask_png="$(mktemp -t wizmask).png"
"$MAGICK" -size 1024x1024 xc:none -draw "roundrectangle 0,0,1023,1023,235,235" \
  -alpha extract "$mask_png"
# light — art as-is
"$MAGICK" "$MASCOT" -resize 1024x1024^ -gravity center -extent 1024x1024 \
  "$mask_png" -compose CopyOpacity -composite \
  "$ASSETS/$NAME-icon-light.png"
# dark — night tint
"$MAGICK" "$MASCOT" -resize 1024x1024^ -gravity center -extent 1024x1024 \
  -modulate 52,82,100 -fill '#10141c' -colorize 28% -brightness-contrast 0x8 \
  "$mask_png" -compose CopyOpacity -composite \
  "$ASSETS/$NAME-icon-dark.png"
rm -f "$mask_png"
echo "✓ $NAME icons → $ASSETS"
