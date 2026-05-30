#!/usr/bin/env fish
# Render the three session-5 slides at 2400×1500 (16:10 retina) for the
# UCLA Scores for Social Software Session #5 zoom on 2026-05-28.
#
# Outputs:
#   session-5-slide-1-front-door.png
#   session-5-slide-2-surfaces.png
#   session-5-slide-3-frog.png
#
# Uses chrome-shot.mjs (toolchain/macos) to avoid orphaned headless Chrome.

set -l here (dirname (status filename))
set -l repo (realpath "$here/../..")
set -l tmp (mktemp -d)

set -l portrait "$repo/portraits/jeffrey/corpus/shoot/jeffery-av--01.jpg"
set -l pals_svg "$repo/papers/arxiv-kidlisp/figures/pals.svg"
set -l fonts "$repo/system/public/type/webfonts"
set -l keymap_png "$repo/slides/notepat-keymap/notepat-keymap.png"
set -l shared_css "$here/_shared.css"
set -l chrome_shot "$repo/toolchain/macos/chrome-shot.mjs"

# Square Jeffrey portrait (top-aligned crop, scaled to 600×600)
sips -c 4815 4815 --cropOffset 0 0 "$portrait" --out "$tmp/jeffrey-raw.jpg" >/dev/null
sips -z 600 600 "$tmp/jeffrey-raw.jpg" --out "$tmp/jeffrey.jpg" >/dev/null

# Base64 data URIs (images + fonts so the rendered PNG ships self-contained)
echo "data:image/svg+xml;base64,"(base64 -i "$pals_svg")        > "$tmp/pals.b64"
echo "data:image/jpeg;base64,"(base64 -i "$tmp/jeffrey.jpg")    > "$tmp/jeffrey.b64"
echo "data:image/png;base64,"(base64 -i "$keymap_png")          > "$tmp/keymap.b64"
echo "data:font/woff2;base64,"(base64 -i "$fonts/ywft-processing-bold.woff2")     > "$tmp/ywft-bold.b64"
echo "data:font/woff2;base64,"(base64 -i "$fonts/ywft-processing-light.woff2")    > "$tmp/ywft-light.b64"
echo "data:font/woff2;base64,"(base64 -i "$fonts/BerkeleyMonoVariable-Regular.woff2") > "$tmp/berkeley.b64"

# Inject shared CSS (with font URIs already substituted) into every slide.
python3 -c "
import sys
shared = open('$shared_css').read()
shared = shared.replace('__YWFT_BOLD__',  open('$tmp/ywft-bold.b64').read().strip())
shared = shared.replace('__YWFT_LIGHT__', open('$tmp/ywft-light.b64').read().strip())
shared = shared.replace('__BERKELEY__',   open('$tmp/berkeley.b64').read().strip())
open('$tmp/shared.css', 'w').write(shared)
"

for slide in slide-1-front-door slide-2-surfaces slide-3-frog
    set -l src "$here/$slide.html"
    set -l html_out "$tmp/$slide.html"
    set -l png_out "$here/session-5-$slide.png"

    python3 -c "
html = open('$src').read()
shared = open('$tmp/shared.css').read()
html = html.replace('__SHARED_CSS__', shared)
html = html.replace('__PALS_LOGO__',  open('$tmp/pals.b64').read().strip())
html = html.replace('__JEFFREY_PIC__', open('$tmp/jeffrey.b64').read().strip())
html = html.replace('__KEYMAP_PNG__', open('$tmp/keymap.b64').read().strip())
open('$html_out', 'w').write(html)
"

    node "$chrome_shot" \
        "file://$html_out" \
        "$png_out" \
        --size 2400x1500 \
        --budget 1500 \
        --wait 30000

    echo "wrote $png_out"
end

rm -rf "$tmp"
