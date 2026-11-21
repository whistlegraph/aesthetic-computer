#!/usr/bin/env bash
set -euo pipefail
APPLE_DIR=$(cd "$(dirname "$0")" && pwd)
DERIVED=${AC_MAC_DERIVED_DATA:-/tmp/ac-universal-build}
DEST=${AC_MAC_APP_PATH:-/Applications/Aesthetic.Computer.app}
IDENTITY=${AC_MAC_SIGN_IDENTITY:-Developer ID Application: Jeffrey Scudder (FB5948YR3S)}

xcodebuild -project "$APPLE_DIR/aesthetic.computer.xcodeproj" \
  -scheme AestheticComputerMac -configuration Release \
  -destination 'generic/platform=macOS' -derivedDataPath "$DERIVED" -jobs 2 \
  'ARCHS=arm64 x86_64' ONLY_ACTIVE_ARCH=NO \
  CODE_SIGN_STYLE=Manual CODE_SIGN_IDENTITY="$IDENTITY" build
BUILT="$DERIVED/Build/Products/Release/Aesthetic.Computer.app"
codesign --verify --deep --strict "$BUILT"

# A native installation may still use the former spaced name. The dotted
# destination may already contain the earlier Electron desktop app.
previous=("$DEST")
legacy="/Applications/Aesthetic Computer.app"
if [[ "$DEST" == "/Applications/Aesthetic.Computer.app" && -d "$legacy" ]] &&
   [[ "$(/usr/libexec/PlistBuddy -c 'Print :CFBundleIdentifier' "$legacy/Contents/Info.plist")" == aesthetic.computer ]]; then
  previous+=("$legacy")
fi
for prior in "${previous[@]}"; do
  [[ -d "$prior" ]] || continue
  osascript - "$prior" <<'APPLESCRIPT'
on run argv
  set appPath to item 1 of argv
  if application appPath is running then tell application appPath to quit
  repeat 100 times
    if not (application appPath is running) then exit repeat
    delay 0.1
  end repeat
  if application appPath is running then error "App did not quit: " & appPath
end run
APPLESCRIPT
done
for prior in "${previous[@]}"; do
  [[ -d "$prior" ]] || continue
  BACKUPS="$HOME/Library/Application Support/Aesthetic Computer/Previous Builds"
  mkdir -p "$BACKUPS"
  backup=$(mktemp -d "$BACKUPS/install-XXXXXXXX")
  mv "$prior" "$backup/"
done
ditto "$BUILT" "$DEST"
codesign --verify --deep --strict "$DEST"
python3 "$APPLE_DIR/scripts/pin-mac-dock.py" "$DEST"
open "$DEST"
