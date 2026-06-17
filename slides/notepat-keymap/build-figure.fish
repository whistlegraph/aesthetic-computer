#!/usr/bin/env fish
# Render the canonical notepat keymap FIGURE (paper variant) — piano +
# QWERTY + connectors only, 2400×860 on white. No QR codes, no portrait,
# no brand block. See figure.html.
#
# Output: slides/notepat-keymap/notepat-keymap-figure.png
# Optional first arg: alternative output path.
#
# Deps: python3, Google Chrome.

set -l here (dirname (status filename))
set -l repo (realpath "$here/../..")
set -l out (test -n "$argv[1]"; and echo $argv[1]; or echo "$here/notepat-keymap-figure.png")
set -l tmp (mktemp -d)

set -l template "$here/figure.html"
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
open('$tmp/keymap-figure.html', 'w').write(html)
"

"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
    --headless=new --no-sandbox --disable-gpu --hide-scrollbars \
    --window-size=2400,860 \
    --virtual-time-budget=500 \
    --screenshot="$out" \
    "file://$tmp/keymap-figure.html" 2>/dev/null

rm -rf "$tmp"
echo "wrote $out"
