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

MODEL_DIR="$APP/Contents/Resources/PalsModel"
mkdir -p "$MODEL_DIR"
/usr/bin/usdextract "$PALS_GLB" -o "$MODEL_DIR"
/usr/bin/usdcat "$MODEL_DIR/pals-mesh-nat-amethyst.usdc" -o build/pals-mesh.usda
/usr/bin/sed -E -i '' 's|@[^@]*pals-mesh-nat-amethyst\.glb\[([^]]+)\]@|@\1@|g' build/pals-mesh.usda
/usr/bin/usdcat build/pals-mesh.usda -o "$MODEL_DIR/pals-mesh.usdc"
rm "$MODEL_DIR/pals-mesh-nat-amethyst.usdc"
cp "$PALS_GLB" "$APP/Contents/Resources/pals-mesh.glb"

swiftc -O \
    -target "$(uname -m)-apple-macos14.0" \
    -framework AppKit \
    -framework QuartzCore \
    -framework SceneKit \
    -framework AVFoundation \
    -framework CoreBluetooth \
    -framework CoreLocation \
    -framework CoreWLAN \
    Sources/*.swift \
    -o "$APP/Contents/MacOS/BlueberryWallpaper"

cp Resources/Info.plist "$APP/Contents/Info.plist"
cp Resources/computer.aesthetic.blueberry-wallpaper.plist "$APP/Contents/Resources/"
cp Resources/unifont.otf Resources/chunky8.ttf "$APP/Contents/Resources/"

# The hardened runtime hides Location (and so the Wi-Fi name) without this entitlement.
codesign --force --options runtime --entitlements Resources/entitlements.plist --sign "$SIGN_ID" "$APP"
echo "✓ built $APP"
