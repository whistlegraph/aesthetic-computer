#!/bin/bash
# make-dmg.sh — wrap dist/MacPal.app in a drag-to-Applications dmg.
#
#   ./make-dmg.sh [out.dmg]        # default: ~/Desktop/MacPal.dmg
#
# The window opens in icon view with the pal on the left, an /Applications
# symlink on the right — the classic "drag me across" install. Layout is
# arranged by Finder scripting on a writable dmg, then compressed to UDZO.
set -e
cd "$(dirname "$0")"

APP="dist/MacPal.app"
OUT="${1:-$HOME/Desktop/MacPal.dmg}"
VOL="MacPal"
[ -d "$APP" ] || { echo "✗ $APP missing — run ./package.sh first"; exit 1; }

STAGE=$(mktemp -d /tmp/macpal-dmg.XXXXXX)
RW="$STAGE/rw.dmg"
mkdir "$STAGE/root"
cp -R "$APP" "$STAGE/root/MacPal.app"
ln -s /Applications "$STAGE/root/Applications"

echo "› building writable dmg"
hdiutil create -volname "$VOL" -srcfolder "$STAGE/root" -format UDRW -fs HFS+ "$RW" >/dev/null

echo "› arranging the window"
MOUNT=$(hdiutil attach -readwrite -noverify -noautoopen "$RW" | awk -F'\t' '/\/Volumes\//{print $NF; exit}')
osascript <<EOF
tell application "Finder"
  tell disk "$VOL"
    open
    set current view of container window to icon view
    set toolbar visible of container window to false
    set statusbar visible of container window to false
    set the bounds of container window to {200, 200, 660, 480}
    set opts to the icon view options of container window
    set arrangement of opts to not arranged
    set icon size of opts to 104
    set text size of opts to 13
    set position of item "MacPal.app" of container window to {120, 130}
    set position of item "Applications" of container window to {340, 130}
    close
  end tell
end tell
EOF
sync
hdiutil detach "$MOUNT" -quiet

echo "› compressing"
rm -f "$OUT"
hdiutil convert "$RW" -format UDZO -o "$OUT" >/dev/null
rm -rf "$STAGE"
echo "✓ $OUT"
