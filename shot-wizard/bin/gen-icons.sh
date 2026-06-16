#!/bin/sh
# shot-wizard/bin/gen-icons.sh — build light + dark dock icons from the
# ShotWizard mascot (ImageMagick), matching the wizard-app family.
#   light = mascot art full-bleed in a macOS squircle
#   dark  = same art night-tinted (dimmed + cool slate cast)
set -e
REPO="$(cd "$(dirname "$0")/../.." && pwd)"
ASSETS="$REPO/shot-wizard/Sources/ShotWizard/Assets"
MASCOT="$ASSETS/shotwizard-mascot.png"
[ -f "$MASCOT" ] || { echo "✗ mascot missing: $MASCOT (run gen-mascot.mjs first)"; exit 1; }

mask_png="$(mktemp -t wizmask).png"
magick -size 1024x1024 xc:none -draw "roundrectangle 0,0,1023,1023,235,235" \
  -alpha extract "$mask_png"
magick "$MASCOT" -resize 1024x1024^ -gravity center -extent 1024x1024 \
  "$mask_png" -compose CopyOpacity -composite \
  "$ASSETS/shotwizard-icon-light.png"
magick "$MASCOT" -resize 1024x1024^ -gravity center -extent 1024x1024 \
  -modulate 52,82,100 -fill '#10141c' -colorize 28% -brightness-contrast 0x8 \
  "$mask_png" -compose CopyOpacity -composite \
  "$ASSETS/shotwizard-icon-dark.png"
rm -f "$mask_png"
echo "✓ shotwizard icons → $ASSETS"
