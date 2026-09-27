#!/usr/bin/env bash
# scorebook.sh — regenerate the entries and typeset MNPT Scores.
# Rides the shared culturehub-packet.sty; fonts/graphics resolve through the
# assets symlink because the .sty loads them with cwd-relative paths.
set -euo pipefail
LANE="$(cd "$(dirname "$0")/.." && pwd)"
node "$LANE/bin/scorebook.mjs" "$@"
cd "$LANE/book"
PACKET=../../acceptance/packet
[ -e assets ] || ln -s "$PACKET/assets" assets
export TEXINPUTS="$PACKET:"
xelatex -interaction=nonstopmode -halt-on-error mnpt-scores.tex >/dev/null
xelatex -interaction=nonstopmode -halt-on-error mnpt-scores.tex >/dev/null
rm -f mnpt-scores.aux mnpt-scores.log mnpt-scores.out
echo "book/mnpt-scores.pdf"
