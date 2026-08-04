#!/bin/zsh
set -euo pipefail
cd "${0:A:h}"
swift build -c release
mkdir -p "$HOME/.local/bin" "$HOME/.local/share/menu-fighter"
rm -f "$HOME/.local/bin/menu-fighter"
cp .build/release/menu-fighter "$HOME/.local/bin/menu-fighter"
cp "../../pop/samples/whats-inside-your-heart/sfx/Jeffrey count in.wav" "$HOME/.local/share/menu-fighter/jeffrey-count-in.wav"
chmod 755 "$HOME/.local/bin/menu-fighter"
echo "Installed Menu Fighter. Trackpad Fighter keeps ownership of the four-corner pose."
echo "Launch with: menu-fighter --desktop"
