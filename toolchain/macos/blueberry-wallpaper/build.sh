#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

APP="build/BlueberryWallpaper.app"
PALS_GLB="${PALS_GLB:-../../../xbox/assets/pals-mesh-nat-amethyst.glb}"
SIGN_ID="${DEV_ID:--}"

if [[ ! -f "$PALS_GLB" && -f "../../../.worktrees/xbox-native-v11/xbox/assets/pals-mesh-nat-amethyst.glb" ]]; then
    PALS_GLB="../../../.worktrees/xbox-native-v11/xbox/assets/pals-mesh-nat-amethyst.glb"
fi
if [[ ! -f "$PALS_GLB" ]]; then
    echo "missing Xbox Pals Meshy GLB: $PALS_GLB" >&2
    exit 1
fi

rm -rf "$APP"
mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources"

/usr/bin/usdcat "$PALS_GLB" -o build/pals-mesh.usdc
swiftc -O -framework AppKit -framework Metal -framework SceneKit \
    render-sprites.swift -o build/render-pals-sprites
build/render-pals-sprites build/pals-mesh.usdc "$APP/Contents/Resources"

swiftc -O \
    -framework AppKit \
    -framework QuartzCore \
    Sources/main.swift \
    -o "$APP/Contents/MacOS/BlueberryWallpaper"

cp Resources/Info.plist "$APP/Contents/Info.plist"
cp Resources/computer.aesthetic.blueberry-wallpaper.plist "$APP/Contents/Resources/"

codesign --force --options runtime --sign "$SIGN_ID" "$APP"
echo "✓ built $APP"
