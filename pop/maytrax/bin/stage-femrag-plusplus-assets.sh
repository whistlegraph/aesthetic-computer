#!/usr/bin/env bash
# stage-femrag-plusplus-assets.sh — put Femrag++ where the AC piece can see it.
#
# The `femrag++` piece is a lib/pop.mjs slideshow, so it wants an mp3, a
# cover, and one still per section. Femrag++ has no illustration campaign
# behind it — its cover is a photograph — so the section stills are twelve
# crops of that same photograph: the slideshow walks the room the record was
# made in, one object per section, and closes on the wide shot.
#
# Writes into system/public/assets/pop/, which is the staging tree for
# assets.aesthetic.computer — publish with `npm run assets:sync:up` and the
# CDN flush from pop/RELEASES.md.
#
#   bash pop/maytrax/bin/stage-femrag-plusplus-assets.sh

set -euo pipefail
cd "$(dirname "$0")/../../.."

COVER=pop/maytrax/covers/femrag-plusplus-cover.jpg
MP3=pop/maytrax/out/femrag-plusplus-release.mp3
DEST=system/public/assets/pop
SECS=$DEST/femrag-plusplus

for f in "$COVER" "$MP3"; do
  [ -f "$f" ] || { echo "✗ missing $f"; exit 1; }
done
mkdir -p "$SECS"

# The 3000² master stays in covers/ (that is the file the stores got). The
# web copy is 1200² — pop.mjs preloads the cover as a backdrop, and a 3000²
# decode is ~36 MB of pixels for something that never fills more than a phone.
magick "$COVER" -resize 1200x1200 -quality 82 -strip "$DEST/femrag-plusplus.jpg"
cp "$MP3" "$DEST/femrag-plusplus.mp3"

# One crop per section, in play order. Geometry is in the cover's 3000² space.
CROPS=(
  "1400x1400+480+1560"  # 0  drop1a        the fight stick, the instrument
  "1050x1050+810+180"   # 1  drop1b        the fighting game on the TV
  "620x620+1130+1140"   # 2  breakdown     the laptop screen, mid-conversation
  "620x620+1650+1180"   # 3  buildup2      cold brew and the iced coffee
  "780x780+1010+1330"   # 4  drop2a        the citrus laptop's keyboard
  "760x760+2140+1000"   # 5  drop2b        the second laptop, running the DAW
  "700x700+1950+1400"   # 6  ragga-a       the book stack under the table
  "700x700+300+120"     # 7  ragga-b       the fiddle-leaf fig
  "900x900+1600+300"    # 8  ragga-breathe the paper lamp, lit
  "600x600+0+1650"      # 9  ragga-push    the lime Xbox pad
  "600x600+120+1120"    # 10 ragga-push-b  the orange blanket
  "3000x3000+0+0"       # 11 outro         the whole room again
)

for i in "${!CROPS[@]}"; do
  magick "$COVER" -crop "${CROPS[$i]}" +repage \
    -resize 1024x1024 -quality 82 -strip "$SECS/sec-$i.jpg"
done

echo "✓ staged $(ls "$SECS" | wc -l | tr -d ' ') section stills + cover + mp3 → $DEST"
du -sh "$DEST"
