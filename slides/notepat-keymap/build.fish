#!/usr/bin/env fish
# Render the notepat keymap slide to a 16:9 PNG (2400×1350, retina for
# Google Slides widescreen). Generates QR codes + base64 data URIs for
# the pals mark and Jeffrey portrait, substitutes them into template.html,
# then captures with headless Chrome.
#
# Output: slides/notepat-keymap/notepat-keymap.png
# Optional first arg: alternative output path.
#
# Deps: qrencode (brew), sips (macOS built-in), python3, Google Chrome.

set -l here (dirname (status filename))
set -l repo (realpath "$here/../..")
set -l out (test -n "$argv[1]"; and echo $argv[1]; or echo "$here/notepat-keymap.png")
set -l tmp (mktemp -d)

set -l template "$here/template.html"
set -l portrait "$repo/portraits/jeffrey/corpus/shoot/jeffery-av--01.jpg"
set -l pals_svg "$repo/papers/arxiv-kidlisp/figures/pals.svg"
set -l fonts "$repo/system/public/type/webfonts"

# 1. QR codes
qrencode -o "$tmp/qr-notepat.png"  -s 12 -m 2 -l Q "https://notepat.com"
qrencode -o "$tmp/qr-menuband.png" -s 12 -m 2 -l Q "https://prompt.ac/menuband"

# 2. Square Jeffrey portrait (top-aligned crop, scaled to 600×600)
sips -c 4815 4815 --cropOffset 0 0 "$portrait" --out "$tmp/jeffrey-raw.jpg" >/dev/null
sips -z 600 600 "$tmp/jeffrey-raw.jpg" --out "$tmp/jeffrey.jpg" >/dev/null

# 3. Base64 data URIs (images + AC fonts so the rendered PNG ships with
#    YWFT Processing / Berkeley Mono baked in)
echo "data:image/png;base64,"(base64 -i "$tmp/qr-notepat.png")  > "$tmp/qr-notepat.b64"
echo "data:image/png;base64,"(base64 -i "$tmp/qr-menuband.png") > "$tmp/qr-menuband.b64"
echo "data:image/svg+xml;base64,"(base64 -i "$pals_svg")        > "$tmp/pals.b64"
echo "data:image/jpeg;base64,"(base64 -i "$tmp/jeffrey.jpg")    > "$tmp/jeffrey.b64"
echo "data:font/woff2;base64,"(base64 -i "$fonts/ywft-processing-bold.woff2")     > "$tmp/ywft-bold.b64"
echo "data:font/woff2;base64,"(base64 -i "$fonts/ywft-processing-light.woff2")    > "$tmp/ywft-light.b64"
echo "data:font/woff2;base64,"(base64 -i "$fonts/BerkeleyMonoVariable-Regular.woff2") > "$tmp/berkeley.b64"

# 4. Template substitution
python3 -c "
import sys
html = open('$template').read()
for placeholder, path in [
    ('__QR_NOTEPAT__', '$tmp/qr-notepat.b64'),
    ('__QR_MENUBAND__', '$tmp/qr-menuband.b64'),
    ('__PALS_LOGO__', '$tmp/pals.b64'),
    ('__JEFFREY_PIC__', '$tmp/jeffrey.b64'),
    ('__YWFT_BOLD__', '$tmp/ywft-bold.b64'),
    ('__YWFT_LIGHT__', '$tmp/ywft-light.b64'),
    ('__BERKELEY__', '$tmp/berkeley.b64'),
]:
    html = html.replace(placeholder, open(path).read().strip())
open('$tmp/keymap.html', 'w').write(html)
"

# 5. Headless Chrome render → PNG
"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
    --headless=new --no-sandbox --disable-gpu --hide-scrollbars \
    --window-size=2400,1350 \
    --virtual-time-budget=500 \
    --screenshot="$out" \
    "file://$tmp/keymap.html" 2>/dev/null

rm -rf "$tmp"
echo "wrote $out"
