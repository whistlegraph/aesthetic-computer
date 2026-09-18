#!/bin/bash
set -euo pipefail
ui_source="$(cd "$(dirname "$0")/.." && pwd)"
ui_target="${1:-$HOME/Applications/Easel.app/Contents/Resources/easel/desktop-ui}"
mkdir -p "$ui_target/node_modules"
for pattern in '*.js' '*.html' '*.css'; do
  find "$ui_source" -maxdepth 1 -type f -name "$pattern" -exec cp {} "$ui_target/" \;
done
for asset in fonts assets vendor donkey-actions; do
  if [[ -d "$ui_source/$asset" ]]; then rsync -a "$ui_source/$asset/" "$ui_target/$asset/"; fi
done
if [[ -d "$ui_source/node_modules/@xterm" ]]; then
  rsync -a "$ui_source/node_modules/@xterm/" "$ui_target/node_modules/@xterm/"
fi
printf '%s\n' 'UI files installed. Choose Aesel → Reload Interface.'
