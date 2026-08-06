#!/bin/bash
# Install Captutor onto a filming host without copying runtime state or secrets.
set -euo pipefail

SOURCE="$(cd "$(dirname "$0")/.." && pwd)"
REPO_ROOT="$(cd "$SOURCE/.." && pwd)"
DEST="${CAPTUTOR_HOME:-$HOME/Developer/captutor}"
POP_DEST="$DEST/vendor/pop"

mkdir -p "$DEST/out"
rsync -a --delete \
  --exclude out/ \
  --exclude .git/ \
  "$SOURCE/" "$DEST/"

# Captutor masters narration with the same /pop C DSP used by the studio. The
# filming workspace is an installed carve-out, so carry the source and build a
# host-native binary instead of depending on another monorepo checkout.
mkdir -p "$POP_DEST/lib" "$POP_DEST/dsp/c" "$POP_DEST/dsp"
install -m 644 "$REPO_ROOT/pop/lib/master.mjs" "$POP_DEST/lib/master.mjs"
install -m 644 "$REPO_ROOT/pop/dsp/eq-graph.json" "$POP_DEST/dsp/eq-graph.json"
rsync -a --delete \
  --exclude acdsp \
  --exclude '*.o' \
  "$REPO_ROOT/pop/dsp/c/" "$POP_DEST/dsp/c/"
make -C "$POP_DEST/dsp/c" clean all

mkdir -p "$HOME/.local/bin" "$HOME/Desktop/outbox"
install -m 755 "$SOURCE/vendor/reel.mjs" "$HOME/.local/bin/reel.mjs"
/usr/bin/swiftc -O "$SOURCE/bin/captutor-pointer.swift" -o "$HOME/.local/bin/captutor-pointer"
/usr/bin/swiftc -O "$SOURCE/bin/captutor-cursor.swift" -o "$HOME/.local/bin/captutor-cursor"

echo "✓ Captutor installed at $DEST"
echo "✓ /pop spoken-word DSP built at $POP_DEST/dsp/c/acdsp"
echo "✓ reel controller installed at $HOME/.local/bin/reel.mjs"
echo "✓ native tutorial cursor installed at $HOME/.local/bin/captutor-cursor"
echo "✓ delivery outbox at $HOME/Desktop/outbox"
