#!/bin/sh
set -eu
POP="$(cd "$(dirname "$0")/.." && pwd)"
mkdir -p "$HOME/.local/bin"
chmod +x "$POP/bin/juke-cache.py"
ln -sfn "$POP/bin/juke-cache.py" "$HOME/.local/bin/ac-juke-cache"
echo "Installed ac-juke-cache"
