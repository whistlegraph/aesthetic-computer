#!/bin/bash
# Install Captutor onto a filming host without copying runtime state or secrets.
set -euo pipefail

SOURCE="$(cd "$(dirname "$0")/.." && pwd)"
DEST="${CAPTUTOR_HOME:-$HOME/Developer/captutor}"

mkdir -p "$DEST/out"
rsync -a --delete \
  --exclude out/ \
  --exclude .git/ \
  "$SOURCE/" "$DEST/"

mkdir -p "$HOME/.local/bin" "$HOME/Desktop/outbox"
install -m 755 "$SOURCE/vendor/reel.mjs" "$HOME/.local/bin/reel.mjs"

echo "✓ Captutor installed at $DEST"
echo "✓ reel controller installed at $HOME/.local/bin/reel.mjs"
echo "✓ delivery outbox at $HOME/Desktop/outbox"
