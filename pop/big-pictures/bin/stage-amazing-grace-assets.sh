#!/usr/bin/env bash
# stage-amazing-grace-assets.sh — the release print, the cover, the
# section stills, the canvas and the DistroKid packet, from the bake.
#
#   bash pop/big-pictures/bin/stage-amazing-grace-assets.sh [cover.png]
#
# CDN tree (gitignored, `npm run pop:assets:up` mirrors it):
#   system/public/assets/pop/amazing-grace.mp3          320 k, cover embedded
#   system/public/assets/pop/amazing-grace.jpg          1200² web copy
#   system/public/assets/pop/amazing-grace/sec-0..5.jpg 1024² crops of the cover
#   system/public/assets/pop/amazing-grace/amazing-grace-master.flac
#   system/public/assets/pop/amazing-grace-canvas.mp4   Spotify Canvas, 2 bars
# The 3000² store master stays in pop/big-pictures/covers/amazing-grace/.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
LANE="$(cd -- "$HERE/.." && pwd)"
REPO="$(cd -- "$LANE/../.." && pwd)"
OUT="$LANE/out/amazing-grace"
COVERS="$LANE/covers/amazing-grace"
PICK="${1:-$COVERS/out/amazing-grace-cover-cover.png}"
COVER="$COVERS/amazing-grace-cover.jpg"           # 3000² store master
DEST="$REPO/system/public/assets/pop"
SECS="$DEST/amazing-grace"
DK="$HOME/Documents/Shelf/amazing-grace-DISTROKID"
FLAC="$OUT/amazing-grace-release.flac"
MP3="$OUT/amazing-grace-release.mp3"
[ -f "$PICK" ] || { echo "✗ no cover pick at $PICK"; exit 1; }
[ -f "$FLAC" ] || { echo "✗ no master — bash pop/big-pictures/bin/bake-amazing.sh"; exit 1; }
mkdir -p "$SECS" "$DK"

echo "→ cover 3000² (store) + 1200² (web)"
magick "$PICK" -filter Lanczos -resize 3000x3000 -unsharp 0x1+0.6+0.02 \
  -colorspace sRGB -strip -quality 92 "$COVER"
magick "$COVER" -resize 1200x1200 -quality 82 -strip "$DEST/amazing-grace.jpg"

echo "→ release print with the cover embedded"
ffmpeg -y -loglevel error -i "$MP3" -i "$DEST/amazing-grace.jpg" -map 0:a -map 1:v \
  -c:a copy -c:v mjpeg -id3v2_version 3 \
  -metadata:s:v title="Album cover" -metadata:s:v comment="Cover (front)" \
  "$DEST/amazing-grace.mp3"
cp "$FLAC" "$SECS/amazing-grace-master.flac"

echo "→ section stills (six crops of the cover, in play order)"
# intro · line 1 · line 2 · line 3 · line 4 · amen — WxH+X+Y in the 3000² space
CROPS=(
  "3000x3000+0+0"        # 0 intro — the whole sanctuary
  "1500x1500+600+1250"   # 1 amazing grace — jeffrey at the keys
  "1400x1400+1500+950"   # 2 that saved a wretch — the pews behind him
  "1300x1300+0+900"      # 3 i once was lost — the woman singing beside him
  "1200x1200+1100+500"   # 4 was blind but now i see — the cross in the light
  "1500x1500+0+0"        # 5 amen — the stained glass
)
for i in "${!CROPS[@]}"; do
  magick "$COVER" -crop "${CROPS[$i]}" +repage -resize 1024x1024 -quality 82 -strip "$SECS/sec-$i.jpg"
done

echo "→ canvas (Spotify, 2 bars @ 70 = 6.857 s, silent, seamless)"
node "$REPO/pop/bin/photo-canvas.mjs" --photo "$COVER" --bpm 70 --bars 2 \
  --out "$DEST/amazing-grace-canvas.mp4" | tail -2

echo "→ DistroKid packet → $DK"
cp "$FLAC" "$DK/amazing-grace-MASTER.flac"
cp "$COVER" "$DK/amazing-grace-cover.jpg"
cp "$LANE/amazing-grace.release.json" "$DK/release.json"
node "$REPO/pop/bin/distrokid-submit.mjs" "$DK" --dry-run | tail -4
ls -la "$DEST/amazing-grace.mp3" "$DEST/amazing-grace.jpg" "$DEST/amazing-grace-canvas.mp4" "$SECS" | sed 's/^/   /'
