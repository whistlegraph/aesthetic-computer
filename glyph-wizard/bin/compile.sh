#!/usr/bin/env bash
# compile.sh — UFO → OTF/TTF via fontTools/fontmake.
# Requires: pipx install fontmake   (or: pip install fontmake)
set -euo pipefail
UFO="${1:-}"
[ -z "$UFO" ] && { echo "usage: bin/compile.sh <font.ufo>"; exit 1; }
OUTDIR="$(dirname "$UFO")"
fontmake -u "$UFO" -o otf ttf --output-dir "$OUTDIR"
echo "✓ compiled → $OUTDIR (.otf / .ttf)"
