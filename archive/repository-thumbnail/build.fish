#!/usr/bin/env fish#!/usr/bin/env fish



# Build script for repository thumbnail image# Build script for repository thumbnail image

# Requires: ImageMagick, qrencode# Requires: ImageMagick, qrencode



set SCRIPT_DIR (dirname (status --current-filename))set SCRIPT_DIR (dirname (status --current-filename))

set REPO_ROOT (realpath "$SCRIPT_DIR/..")set REPO_ROOT (realpath "$SCRIPT_DIR/..")

set OUTPUT_DIR "$SCRIPT_DIR"set OUTPUT_DIR "$SCRIPT_DIR"



echo "🎨 Building repository thumbnail..."echo "🎨 Building repository thumbnail..."



# Generate QR code for the repository# Generate QR code for the repository

echo "📱 Generating QR code..."echo "📱 Generating QR code..."

qrencode -o "$OUTPUT_DIR/qr-code.png" -s 10 -m 1 'https://github.com/whistlegraph/aesthetic-computer'qrencode -o "$OUTPUT_DIR/qr-code.png" -s 10 -m 1 'https://github.com/whistlegraph/aesthetic-computer'



# Create the social preview image# Create the social preview image

echo "🖼️  Creating social preview image..."echo "🖼️  Creating social preview image..."

magick \magick \

  "$REPO_ROOT/system/public/assets/screenshots/images/june-17-2023-at-7-34-pm.webp" -resize 1280x640^ -gravity center -extent 1280x640 \  "$REPO_ROOT/system/public/assets/screenshots/images/june-17-2023-at-7-34-pm.webp" -resize 1280x640^ -gravity center -extent 1280x640 \

  -blur 0x6 \  -blur 0x6 \

  \( -size 1280x640 xc:none -fill 'rgba(0,0,0,0.5)' -draw 'rectangle 0,0 1280,640' \) -composite \  \( -size 1280x640 xc:none -fill 'rgba(0,0,0,0.5)' -draw 'rectangle 0,0 1280,640' \) -composite \

  \( "$REPO_ROOT/system/public/assets/aesthetic-inc/pals.png" -resize 180x108 \) -gravity southwest -geometry +60+60 -composite \  \( "$REPO_ROOT/system/public/assets/aesthetic-inc/pals.png" -resize 180x108 \) -gravity southwest -geometry +60+50 -composite \

  \( "$OUTPUT_DIR/qr-code.png" -resize 120x120 \) -gravity southeast -geometry +60+60 -composite \  \( "$OUTPUT_DIR/qr-code.png" -resize 120x120 \) -gravity southeast -geometry +60+50 -composite \

  -pointsize 36 -fill '#ffff00' -font "$REPO_ROOT/system/public/assets/type/webfonts/ywft-processing-regular.ttf" -gravity northwest -annotate +60+60 'github.com/' \  -pointsize 36 -fill '#ffff00' -font "$REPO_ROOT/system/public/assets/type/webfonts/ywft-processing-regular.ttf" -gravity northwest -annotate +60+60 'github.com/' \

  -pointsize 36 -fill '#00ffff' -font "$REPO_ROOT/system/public/assets/type/webfonts/ywft-processing-regular.ttf" -gravity northwest -annotate +260+60 '@whistlegraph/' \  -pointsize 36 -fill '#00ffff' -font "$REPO_ROOT/system/public/assets/type/webfonts/ywft-processing-regular.ttf" -gravity northwest -annotate +260+60 '@whistlegraph/' \

  -pointsize 64 -fill '#ff69b4' -font "$REPO_ROOT/system/public/assets/type/webfonts/ywft-processing-bold.ttf" -gravity northwest -annotate +60+110 'aesthetic-computer' \  -pointsize 64 -fill '#ff69b4' -font "$REPO_ROOT/system/public/assets/type/webfonts/ywft-processing-bold.ttf" -gravity northwest -annotate +60+110 'aesthetic-computer' \

  "$OUTPUT_DIR/social-preview.png"  "$OUTPUT_DIR/social-preview.png"



echo "✅ Done! Output saved to:"echo "✅ Done! Output saved to:"

echo "   $OUTPUT_DIR/social-preview.png"echo "   $OUTPUT_DIR/social-preview.png"

echo "   $OUTPUT_DIR/qr-code.png"echo "   $OUTPUT_DIR/qr-code.png"

