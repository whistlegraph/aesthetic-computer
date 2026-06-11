#!/bin/sh
# clip-wizard/bin/gen-icons.sh — build light + dark dock icons for BOTH
# wizard apps from their mascot illustrations (ImageMagick).
#
#   light  = the mascot art full-bleed in a macOS squircle
#   dark   = the same art night-tinted (dimmed + cool slate cast)
#
# Outputs <name>-icon-light.png / <name>-icon-dark.png (1024px) into
# each app's Assets dir; DockIcon.swift swaps them with the system
# theme at runtime.
set -e
REPO="$(cd "$(dirname "$0")/../.." && pwd)"

make_icons() {
  mascot="$1"; assets="$2"; name="$3"
  [ -f "$mascot" ] || { echo "✗ mascot missing: $mascot"; return 1; }
  mask_png="$(mktemp -t wizmask).png"
  # macOS Big-Sur-style squircle approximated by a rounded rect
  # (radius ≈ 23% of edge).
  magick -size 1024x1024 xc:none -draw "roundrectangle 0,0,1023,1023,235,235" \
    -alpha extract "$mask_png"
  # light — art as-is
  magick "$mascot" -resize 1024x1024^ -gravity center -extent 1024x1024 \
    "$mask_png" -compose CopyOpacity -composite \
    "$assets/$name-icon-light.png"
  # dark — night tint: dim + cool slate colorize, slight contrast lift
  magick "$mascot" -resize 1024x1024^ -gravity center -extent 1024x1024 \
    -modulate 52,82,100 -fill '#10141c' -colorize 28% -brightness-contrast 0x8 \
    "$mask_png" -compose CopyOpacity -composite \
    "$assets/$name-icon-dark.png"
  rm -f "$mask_png"
  echo "✓ $name icons → $assets"
}

make_icons "$REPO/clip-wizard/Sources/ClipWizard/Assets/clipwizard-mascot.png" \
           "$REPO/clip-wizard/Sources/ClipWizard/Assets" clipwizard
make_icons "$REPO/wave-wizard/Sources/WaveWizard/Assets/wavewizard-mascot.png" \
           "$REPO/wave-wizard/Sources/WaveWizard/Assets" wavewizard
