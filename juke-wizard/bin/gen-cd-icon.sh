#!/bin/sh
# juke-wizard/bin/gen-cd-icon.sh — render JukeWizard's identity as a compact
# disc: an iridescent diffraction wheel (hue = angle) dissolved over a brushed
# silver base, center hub hole + clamp rings, soft specular streak. Writes the
# bare disc (jukewizard-cd.png), the roster mascot (jukewizard-mascot.png,
# backing up the old wizard art once), and the light/dark dock icons in a macOS
# squircle. ImageMagick 7 — everything kept in sRGB TrueColor so the rainbow
# survives compositing.
set -e
REPO="$(cd "$(dirname "$0")/../.." && pwd)"
ASSETS="$REPO/juke-wizard/Sources/JukeWizard/Assets"
NAME="jukewizard"
S=1024
MAGICK="$(command -v magick || command -v convert)"
[ -n "$MAGICK" ] || { echo "✗ ImageMagick not found"; exit 1; }
TMP="$(mktemp -d -t cdicon)"
trap 'rm -rf "$TMP"' EXIT

C=$((S/2)); ROUT=$((S/2 - 16)); RHOLE=$((S*27/256)); RCLAMP=$((S*25/128)); RCLAMPO=$((S*7/32))

# ── 1. angular diffraction wheel (hue = angle), kept sRGB ──────────────────
"$MAGICK" \
 \( -size 1x240 gradient:red-yellow    -rotate 90 \) \
 \( -size 1x240 gradient:yellow-lime   -rotate 90 \) \
 \( -size 1x240 gradient:lime-cyan     -rotate 90 \) \
 \( -size 1x240 gradient:cyan-blue     -rotate 90 \) \
 \( -size 1x240 gradient:blue-magenta  -rotate 90 \) \
 \( -size 1x240 gradient:magenta-red   -rotate 90 \) \
 +append -resize ${S}x${S}\! -distort Polar 0 -resize ${S}x${S}\! \
 -colorspace sRGB -type TrueColor "$TMP/wheel.png"

# ── 2. bright glassy silver base; rainbow dissolved VERY lightly and
#       desaturated → a translucent shiny disc with just a whisper of the
#       diffraction sheen (classic pressed-CD look, not a rainbow wheel) ────
"$MAGICK" -size ${S}x${S} radial-gradient:'gray98-gray70' -colorspace sRGB -type TrueColor "$TMP/silver.png"
"$MAGICK" "$TMP/silver.png" \
  \( "$TMP/wheel.png" -modulate 100,45,100 \) -compose dissolve -define compose:args=26 -composite \
  \( "$TMP/wheel.png" -rotate 90 -modulate 100,40,100 \) -compose dissolve -define compose:args=14 -composite \
  -modulate 104,100,100 "$TMP/sheen.png"

# ── 3. hub clamp rings + soft glossy sweep + hot sparkle ───────────────────
"$MAGICK" -size ${S}x${S} xc:none -stroke 'rgba(255,255,255,0.35)' -strokewidth 6 -fill none \
  -draw "circle $C,$C $C,$((C-RCLAMP))" -draw "circle $C,$C $C,$((C-RCLAMPO))" "$TMP/clamp.png"
"$MAGICK" -size ${S}x${S} xc:none \
  -fill 'rgba(255,255,255,0.32)' -draw "translate $C,$C rotate -28 ellipse 0,0 $((S/2-60)),148 205,335" -blur 0x80 \
  -fill 'rgba(255,255,255,0.55)' -draw "translate $((C-156)),$((C-192)) ellipse 0,0 96,40 0,360" -blur 0x44 "$TMP/gloss.png"

# ── 4. alpha mask (disc + hole), rim shade, assemble ───────────────────────
"$MAGICK" -size ${S}x${S} xc:black -fill white -draw "circle $C,$C $C,$((C-ROUT))" \
  -fill black -draw "circle $C,$C $C,$((C-RHOLE))" -blur 0x1 "$TMP/amask.png"
"$MAGICK" "$TMP/sheen.png" "$TMP/clamp.png" -compose over -composite \
  "$TMP/gloss.png" -compose screen -composite \
  \( -size ${S}x${S} radial-gradient:'none-rgba(0,0,0,0.18)' \) -compose over -composite \
  "$TMP/amask.png" -alpha off -compose CopyOpacity -composite "$ASSETS/$NAME-cd.png"

# roster mascot = the bare disc (back up the original wizard art once)
[ -f "$ASSETS/$NAME-mascot.png" ] && [ ! -f "$ASSETS/$NAME-mascot-wizard.png" ] \
  && cp "$ASSETS/$NAME-mascot.png" "$ASSETS/$NAME-mascot-wizard.png"
cp "$ASSETS/$NAME-cd.png" "$ASSETS/$NAME-mascot.png"

# ── 5. dock icons: disc centered on a macOS squircle ───────────────────────
"$MAGICK" -size ${S}x${S} xc:none -draw "roundrectangle 0,0,$((S-1)),$((S-1)),235,235" \
  -alpha extract "$TMP/squircle.png"
make_icon() { # $1 bg radial-gradient  $2 out
  "$MAGICK" -size ${S}x${S} radial-gradient:"$1" -colorspace sRGB -type TrueColor \
    \( "$ASSETS/$NAME-cd.png" -resize 84% \) -gravity center -compose over -composite \
    "$TMP/squircle.png" -compose CopyOpacity -composite "$2"
}
make_icon 'gray26-gray9'    "$ASSETS/$NAME-icon-light.png"
make_icon '#123a44-#04090c' "$ASSETS/$NAME-icon-dark.png"

echo "✓ $NAME CD icon → $ASSETS ($NAME-cd.png, -mascot.png, -icon-light/dark.png)"
