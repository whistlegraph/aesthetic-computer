#!/usr/bin/env bash
# Regenerate media/ for the CultureHub program brief.
# Thumbnails from the canonical packet + release assets; frames from the score videos.
# Requires macOS `sips` and `ffmpeg`.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ACC="$(cd "$HERE/.." && pwd)"
REL="$(cd "$ACC/../../../pop/nullabye/release/special-sign" && pwd)"
OUT="$HERE/media"
mkdir -p "$OUT"

cp "$ACC/packet/assets/special-sign-cover.jpg"                "$OUT/cover.jpg"
cp "$ACC/packet/assets/special-sign-graphic-score-square.jpg" "$OUT/gscore.jpg"
cp "$ACC/packet/assets/special-sign-live-spatial-3x2.jpg"     "$OUT/spatial.jpg"
cp "$ACC/assets/jeffrey-alan-scudder-headshot-3x2.jpg"        "$OUT/head-main.jpg"
cp "$ACC/assets/jeffrey-alan-scudder-headshot-3x2-green-laptop.jpg" "$OUT/head-green.jpg"
cp "$ACC/assets/jeffrey-alan-scudder-headshot-3x2-seated.jpg" "$OUT/head-seated.jpg"
cp "$ACC/assets/notepat-jam-3x2.png"                          "$OUT/header.png"

for f in "$OUT"/*.jpg "$OUT"/*.png; do sips -Z 900 "$f" --out "$f" >/dev/null; done
sips -s format jpeg "$OUT/header.png" --out "$OUT/header.jpg" >/dev/null
rm -f "$OUT/header.png"

# MacNeoPolitan campaign art (marketing/campaigns/macneopolitan/gens/)
MN="$(cd "$ACC/../../../marketing/campaigns/macneopolitan/gens" && pwd)"
for n in block trio menubar program; do
  sips -Z 900 -s format jpeg "$MN/$n.png" --out "$OUT/mn-$n.jpg" >/dev/null
done

ffmpeg -y -loglevel error -i "$REL/special-sign-circular-score.mp4" \
  -ss 00:00:50 -frames:v 1 -vf scale=900:-1 "$OUT/circular.jpg"
ffmpeg -y -loglevel error -i "$REL/special-sign-graphic-score.mp4" \
  -ss 00:00:50 -frames:v 1 -vf scale=900:-1 "$OUT/gscore-mid.jpg"

echo "media/ rebuilt:"
ls -1 "$OUT"
