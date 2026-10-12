#!/usr/bin/env bash
set -euo pipefail
APPLE_DIR=$(cd "$(dirname "$0")" && pwd)
DERIVED=${AC_MAC_DERIVED_DATA:-/tmp/ac-universal-build}
DEST=${AC_MAC_APP_PATH:-/Applications/Aesthetic Computer.app}
IDENTITY=${AC_MAC_SIGN_IDENTITY:-Developer ID Application: Jeffrey Scudder (FB5948YR3S)}

xcodebuild -project "$APPLE_DIR/aesthetic.computer.xcodeproj" \
  -scheme AestheticComputerMac -configuration Release \
  -destination 'generic/platform=macOS' -derivedDataPath "$DERIVED" -jobs 2 \
  'ARCHS=arm64 x86_64' ONLY_ACTIVE_ARCH=NO \
  CODE_SIGN_STYLE=Manual CODE_SIGN_IDENTITY="$IDENTITY" build
BUILT="$DERIVED/Build/Products/Release/Aesthetic Computer.app"
codesign --verify --deep --strict "$BUILT"

# Quit only this app if already running, then retain the previous installation.
if [[ -d "$DEST" ]]; then
  osascript - "$DEST" <<'APPLESCRIPT'
on run argv
  set appPath to item 1 of argv
  if application appPath is running then tell application appPath to quit
  repeat 100 times
    if not (application appPath is running) then exit repeat
    delay 0.1
  end repeat
  if application appPath is running then error "Aesthetic Computer did not quit; keeping the current installation."
end run
APPLESCRIPT
  BACKUPS="$HOME/Library/Application Support/Aesthetic Computer/Previous Builds"
  mkdir -p "$BACKUPS"
  mv "$DEST" "$BACKUPS/Aesthetic Computer-$(date +%Y%m%d-%H%M%S).app"
fi
ditto "$BUILT" "$DEST"
codesign --verify --deep --strict "$DEST"
python3 "$APPLE_DIR/scripts/pin-mac-dock.py" "$DEST"
open "$DEST"
