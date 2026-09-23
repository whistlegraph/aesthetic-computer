#!/bin/bash
# Install an already-built bundle for this user and pin it without replacing other Dock items.
set -euo pipefail
cd "$(dirname "$0")"
source_app="${1:-$PWD/.build/No Paint Brushes.app}"
installed_app="$HOME/Applications/No Paint.app"
test -x "$source_app/Contents/MacOS/BrushApp"
codesign --verify --deep --strict "$source_app"
mkdir -p "$HOME/Applications"
if [ "$source_app" != "$installed_app" ]; then
  ditto "$source_app" "$installed_app"
fi
codesign --verify --deep --strict "$installed_app"
python3 - "$installed_app" <<'PY'
import pathlib, plistlib, subprocess, sys, urllib.parse
app = pathlib.Path(sys.argv[1]).resolve()
export = subprocess.run(['defaults', 'export', 'com.apple.dock', '-'], check=True, capture_output=True).stdout
prefs = plistlib.loads(export)
for item in prefs.get('persistent-apps', []):
    url = item.get('tile-data', {}).get('file-data', {}).get('_CFURLString', '')
    path = urllib.parse.unquote(urllib.parse.urlparse(url).path).rstrip('/')
    if path == str(app):
        print('Already pinned:', app)
        break
else:
    backup = pathlib.Path.home() / 'Library/Application Support/No Paint/dock-before-install.plist'
    backup.parent.mkdir(parents=True, exist_ok=True)
    if not backup.exists():
        backup.write_bytes(export)
    url = app.as_uri() + '/'
    tile = '{"tile-data"={"file-data"={"_CFURLString"="' + url + '";"_CFURLStringType"=15;};"file-label"="No Paint";};"tile-type"="file-tile";}'
    subprocess.run(['defaults', 'write', 'com.apple.dock', 'persistent-apps', '-array-add', tile], check=True)
    subprocess.run(['killall', 'Dock'], check=False, capture_output=True)
    print('Pinned:', app)
# Read back the persisted Dock entry.
updated = plistlib.loads(subprocess.run(['defaults', 'export', 'com.apple.dock', '-'], check=True, capture_output=True).stdout)
assert any(urllib.parse.unquote(urllib.parse.urlparse(item.get('tile-data', {}).get('file-data', {}).get('_CFURLString', '')).path).rstrip('/') == str(app)
           for item in updated.get('persistent-apps', [])), 'Dock entry did not persist'
PY
printf 'Installed: %s\n' "$installed_app"
