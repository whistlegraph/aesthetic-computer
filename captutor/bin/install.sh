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
/usr/bin/swiftc -O "$SOURCE/bin/captutor-pointer.swift" -o "$HOME/.local/bin/captutor-pointer"
/usr/bin/swiftc -O "$SOURCE/bin/captutor-cursor.swift" -o "$HOME/.local/bin/captutor-cursor"

echo "✓ Captutor installed at $DEST"
echo "✓ reel controller installed at $HOME/.local/bin/reel.mjs"
echo "✓ native tutorial cursor installed at $HOME/.local/bin/captutor-cursor"
echo "✓ delivery outbox at $HOME/Desktop/outbox"
