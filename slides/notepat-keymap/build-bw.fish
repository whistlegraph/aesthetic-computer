#!/usr/bin/env fish
# Generic B&W notepat-keymap figure renderer for iterating on variations.
#   ./build-bw.fish <template.html> [out.png] [scale] [piano-scale] [octave-gap-extra] [piano-position]
# Substitutes the two AC webfonts as data URIs, renders headless Chrome,
# then trims to a tight bounding box (no surrounding whitespace).
#
# Deps: python3, Google Chrome, magick (ImageMagick).

set -l here (dirname (status filename))
set -l repo (realpath "$here/../..")
set -l template $argv[1]

if test -z "$template" -o ! -f "$template"
    echo "usage: "(status filename)" <template.html> [out.png] [scale]" >&2
    exit 2
end

set -l out (test -n "$argv[2]"; and echo $argv[2]; or echo (string replace -r '\.html$' '.png' -- "$template"))
set -l scale (test -n "$argv[3]"; and echo $argv[3]; or echo 2)
set -l piano_scale $argv[4]
set -l octave_gap_extra $argv[5]
set -l piano_position $argv[6]
set -l tmp (mktemp -d)
set -l fonts "$repo/system/public/type/webfonts"
set -l chrome "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

for command in python3 base64 magick
    type -q $command; or begin
        echo "missing dependency: $command" >&2
        rm -rf "$tmp"
        exit 1
    end
end

if test ! -x "$chrome"
    echo "missing dependency: $chrome" >&2
    rm -rf "$tmp"
    exit 1
end

mkdir -p (dirname "$out"); or begin
    rm -rf "$tmp"
    exit 1
end

echo "data:font/woff2;base64,"(base64 -i "$fonts/ywft-processing-bold.woff2") > "$tmp/ywft-bold.b64"
echo "data:font/woff2;base64,"(base64 -i "$fonts/BerkeleyMonoVariable-Regular.woff2") > "$tmp/berkeley.b64"

python3 -c '
from pathlib import Path
import sys

template, output, ywft, berkeley = map(Path, sys.argv[1:])
html = template.read_text()
for placeholder, path in (("__YWFT_BOLD__", ywft), ("__BERKELEY__", berkeley)):
    html = html.replace(placeholder, path.read_text().strip())
output.write_text(html)
' "$template" "$tmp/render.html" "$tmp/ywft-bold.b64" "$tmp/berkeley.b64"; or begin
    rm -rf "$tmp"
    exit 1
end

set -l render_url "file://$tmp/render.html"
if test -n "$piano_scale"
    set render_url "$render_url?pianoScale=$piano_scale"
end
if test -n "$octave_gap_extra"
    if string match -q '*?*' -- "$render_url"
        set render_url "$render_url&octaveGapExtra=$octave_gap_extra"
    else
        set render_url "$render_url?octaveGapExtra=$octave_gap_extra"
    end
end
if test -n "$piano_position"
    if string match -q '*?*' -- "$render_url"
        set render_url "$render_url&pianoPosition=$piano_position"
    else
        set render_url "$render_url?pianoPosition=$piano_position"
    end
end

"$chrome" \
    --headless=new --no-sandbox --disable-gpu --hide-scrollbars \
    --window-size=2800,1500 \
    --force-device-scale-factor=$scale \
    --virtual-time-budget=800 \
    --screenshot="$out" \
    "$render_url" 2>/dev/null; or begin
    rm -rf "$tmp"
    exit 1
end

magick "$out" -trim +repage "$out"; or begin
    rm -rf "$tmp"
    exit 1
end

rm -rf "$tmp"
echo "wrote $out (scale $scale)"
