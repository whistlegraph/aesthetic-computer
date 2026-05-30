#!/usr/bin/env bash
# finalize-amazing-cover.sh — upscale the 1024² cover illy to a 3000²
# DistroKid-ready jpg, mirror to ~/Desktop and the durable Working
# Desktop dir, then re-embed it into the brightened release mp3.
#
# Usage: bash finalize-amazing-cover.sh [VARIANT]    (default v1)
set -euo pipefail
VAR="${1:-v1}"
SECROOT="$HOME/Documents/Shelf/gens/amazing-grace-sections"
DUR_DIR="$HOME/Documents/Shelf/amazing-grace"
DESK="$HOME/Desktop"
SRC="$SECROOT/cover/gens/${VAR}.png"
COVER3000="$DUR_DIR/amazing-grace-cover-3000.jpg"
mkdir -p "$DUR_DIR"

if [[ ! -f "$SRC" ]]; then
  echo "✗ missing $SRC — run gen-amazing-illys.sh first"
  exit 1
fi

echo "▸ upscaling cover ${VAR} → 3000²"
magick "$SRC" -filter Lanczos -resize 3000x3000^ -gravity center -extent 3000x3000 \
       -quality 92 "$COVER3000"
cp "$COVER3000" "$DESK/amazing-grace-cover-3000.jpg"
ls -lh "$COVER3000" "$DESK/amazing-grace-cover-3000.jpg"

echo "▸ re-embedding 3000² cover into release mp3"
MASTER_MP3="$DUR_DIR/amazing-grace.mp3"
TMP_MP3="$DUR_DIR/amazing-grace.mp3.tmp"
ffmpeg -y -hide_banner -loglevel error \
  -i "$MASTER_MP3" -i "$COVER3000" \
  -map 0:a -map 1 -c copy -id3v2_version 3 \
  -metadata:s:v title="Album cover" -metadata:s:v comment="Cover (front)" \
  -disposition:v attached_pic \
  "$TMP_MP3"
mv "$TMP_MP3" "$MASTER_MP3"
cp "$MASTER_MP3" "$DESK/amazing-grace.mp3"
ls -lh "$MASTER_MP3" "$DESK/amazing-grace.mp3"

echo "✓ cover staged → $COVER3000"
echo "✓ release mp3  → $MASTER_MP3 (with 3000² cover embedded)"
