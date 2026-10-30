#!/usr/bin/env bash
# export-web.sh — the pencil donkey's tool variants → aesel.app hero webps.
#
# Each gens/tool-*.png is cut from its paper (flood from the corners) and the
# blank easel canvas is keyed out too, so the page can slide MIME slides
# *under* the picture: the frame and the tool held over the canvas stay on top.
# CANVAS is the canvas quad in 1024px source space (TL TR BR BL); the page's
# site.js carries the same corners at 720px.
set -euo pipefail
cd "$(dirname "$0")"
OUT=../../../system/public/aesel
CANVAS="818,551 935,545 908,738 775,738"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

for src in gens/tool-*.png; do
  name=$(basename "$src" .png)
  name=${name#tool-}
  # 1. Background: flood the paper from all four corners.
  magick "$src" -alpha set -fuzz 9% -fill none \
    -draw "color 0,0 floodfill" -draw "color 1023,0 floodfill" \
    -draw "color 0,1023 floodfill" -draw "color 1023,1023 floodfill" \
    -alpha extract -morphology Open Disk:2 -morphology Close Disk:3 "$TMP/bg.png"
  # 2. Canvas hole: paper-coloured pixels inside the canvas quad.
  magick "$src" \( +clone -fill "srgb(253,244,231)" -colorize 100 \) \
    -compose difference -composite -colorspace gray -threshold 8% -negate \
    \( -size 1024x1024 xc:black -fill white -draw "polygon $CANVAS" \) \
    -compose multiply -composite -morphology Open Disk:1 "$TMP/hole.png"
  # 3. Alpha = background mask minus the hole, softened by a pixel.
  magick "$TMP/bg.png" \( "$TMP/hole.png" -negate \) -compose multiply -composite \
    -blur 0x0.8 "$TMP/alpha.png"
  magick "$src" "$TMP/alpha.png" -alpha off -compose copy_opacity -composite \
    -resize 720x720 -quality 82 -define webp:alpha-quality=90 "$OUT/aesel-tool-$name.webp"
  echo "$OUT/aesel-tool-$name.webp"
done

