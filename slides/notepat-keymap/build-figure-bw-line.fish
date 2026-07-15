#!/usr/bin/env fish
# Render the B&W notepat keymap FIGURE (stripped-back line-art variant) — piano + QWERTY +
# connectors only, on white. No QR codes, no portrait, no brand block.
# See figure-bw-line.html.
#
# Output: slides/notepat-keymap/notepat-keymap-figure-bw-line.png
# Arg 1 (optional): alternative output path.
# Arg 2 (optional): device scale factor for print DPI (default 2).
#   Base layout is 2400×860 (≈300dpi @ 8in wide). Scale 2 → 4800×1720
#   (≈600dpi @ 8in wide); scale 3 → 7200×2580.
#
# Deps: python3, Google Chrome.

set -l here (dirname (status filename))
set -l repo (realpath "$here/../..")
set -l out (test -n "$argv[1]"; and echo $argv[1]; or echo "$here/notepat-keymap-figure-bw-line.png")
set -l scale (test -n "$argv[2]"; and echo $argv[2]; or echo 2)
set -l tmp (mktemp -d)

set -l template "$here/figure-bw-line.html"
set -l fonts "$repo/system/public/type/webfonts"

echo "data:font/woff2;base64,"(base64 -i "$fonts/ywft-processing-bold.woff2") > "$tmp/ywft-bold.b64"
echo "data:font/woff2;base64,"(base64 -i "$fonts/BerkeleyMonoVariable-Regular.woff2") > "$tmp/berkeley.b64"

python3 -c "
html = open('$template').read()
for placeholder, path in [
    ('__YWFT_BOLD__', '$tmp/ywft-bold.b64'),
    ('__BERKELEY__', '$tmp/berkeley.b64'),
]:
    html = html.replace(placeholder, open(path).read().strip())
open('$tmp/keymap-figure-bw-line.html', 'w').write(html)
"

"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
    --headless=new --no-sandbox --disable-gpu --hide-scrollbars \
    --window-size=2400,760 \
    --force-device-scale-factor=$scale \
    --virtual-time-budget=500 \
    --screenshot="$out" \
    "file://$tmp/keymap-figure-bw-line.html" 2>/dev/null

# Crop away all surrounding whitespace → tight bounding box.
magick "$out" -trim +repage "$out"

rm -rf "$tmp"
echo "wrote $out (scale $scale)"
