#!/bin/zsh
set -eu
HERE=${0:A:h}
SIGN_IDENTITY=${NOPAINT_SIGN_IDENTITY:--}
swift build --package-path "$HERE" --jobs 1
BIN=$(swift build --package-path "$HERE" --show-bin-path)
APP="$HERE/.build/No Paint.app"
mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"
cp "$BIN/NoPaint" "$APP/Contents/MacOS/NoPaint"
ICONSET="$HERE/.build/NoPaint.iconset"
mkdir -p "$ICONSET"
ICON_SOURCE="$HERE/../../../nopaint/construct/icons/icon-1024.png"
for size in 16 32 128 256 512; do
  sips -z "$size" "$size" "$ICON_SOURCE" --out "$ICONSET/icon_${size}x${size}.png" >/dev/null
  retina=$((size * 2))
  sips -z "$retina" "$retina" "$ICON_SOURCE" --out "$ICONSET/icon_${size}x${size}@2x.png" >/dev/null
done
iconutil -c icns "$ICONSET" -o "$APP/Contents/Resources/NoPaint.icns"
python3 - "$APP" "$HERE" <<'PY'
from pathlib import Path
import os, plistlib, sys
app, root = map(Path, sys.argv[1:])
(app/'Contents/Info.plist').write_bytes(plistlib.dumps({
    'CFBundleExecutable':'NoPaint', 'CFBundleIdentifier':'computer.aesthetic.nopaint',
    'CFBundleName':'No Paint', 'CFBundleDisplayName':'No Paint',
    'CFBundleIconFile':'NoPaint.icns',
    'CFBundlePackageType':'APPL', 'CFBundleVersion':'1', 'CFBundleShortVersionString':'0.1.0',
    'LSMinimumSystemVersion':'14.0', 'NSHighResolutionCapable':True,
    'NSAppTransportSecurity':{'NSAllowsLocalNetworking':True},
    'NoPaintBackend':os.environ.get('NOPAINT_BACKEND', str(root/'start-backend.sh')),
}))
PY
if [[ "$SIGN_IDENTITY" == "-" ]]; then
  codesign --force --sign - "$APP"
else
  codesign --force --options runtime --timestamp --sign "$SIGN_IDENTITY" "$APP"
fi
codesign --verify --deep --strict "$APP"
if [[ "${1:-}" == "--install" ]]; then
  DEST="$HOME/Applications/No Paint.app"
  if [[ -d "$DEST" ]]; then
    mv "$DEST" "$HOME/Applications/No Paint.previous-$(date +%Y%m%d-%H%M%S).app"
  fi
  cp -R "$APP" "$DEST"
  open "$DEST"
else
  print -r -- "$APP"
fi
