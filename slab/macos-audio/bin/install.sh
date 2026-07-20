#!/usr/bin/env bash
# Build AC's native room-audio helper and install it on this Mac. Pass one or
# more SSH aliases to install the same binary on peer Macs.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
swift build -c release --package-path "$ROOT"
BIN="$ROOT/.build/release/ac-audio-room"
DEST="$HOME/.local/bin/ac-audio-room"
mkdir -p "$(dirname "$DEST")"
install -m 755 "$BIN" "$DEST"
codesign --force --sign - "$DEST"
echo "installed $DEST"

for host in "$@"; do
  ssh "$host" 'mkdir -p "$HOME/.local/bin"'
  scp -q "$BIN" "$host:.local/bin/ac-audio-room"
  ssh "$host" 'chmod 755 "$HOME/.local/bin/ac-audio-room" && codesign --force --sign - "$HOME/.local/bin/ac-audio-room"'
  echo "installed $host:~/.local/bin/ac-audio-room"
done
