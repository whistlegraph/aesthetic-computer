#!/usr/bin/env bash
# book.sh — typeset the band book (autobiographies of use) as PDF.
# Rides the shared culturehub-packet.sty; fonts/graphics resolve via an
# assets symlink because the .sty loads them with cwd-relative paths.
set -euo pipefail
cd "$(dirname "$0")/../book"
PACKET=../../acceptance/packet
[ -e assets ] || ln -s "$PACKET/assets" assets
export TEXINPUTS="$PACKET:"
xelatex -interaction=nonstopmode -halt-on-error the-macneopolitan-trio.tex >/dev/null
xelatex -interaction=nonstopmode -halt-on-error the-macneopolitan-trio.tex >/dev/null
rm -f ./*.aux ./*.log ./*.out
echo "book/the-macneopolitan-trio.pdf"
