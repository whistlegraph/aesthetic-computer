#!/usr/bin/env bash
# Build Aesel and put it somewhere you can use it.
#
#   ./run.sh                 → the simulator
#   ./run.sh device          → the iPhone plugged into this Mac
#   ./run.sh device "ask…"   → same, running one prompt on launch
#
# The shared JavaScript session is bundled into the app.
set -euo pipefail

cd "$(dirname "$0")"
REPO=$(cd ../.. && pwd)
MODE=${1:-simulator}
ASK=${2:-}
DERIVED=${DERIVED:-/tmp/aesel-dd}
./bundle-session.sh

xcodegen generate >/dev/null

if [ "$MODE" = "device" ]; then
  xcodebuild -project Aesel.xcodeproj -scheme Aesel \
    -destination 'generic/platform=iOS' \
    -derivedDataPath "$DERIVED" \
    -allowProvisioningUpdates -jobs 2 build

  APP="$DERIVED/Build/Products/Debug-iphoneos/Aesel.app"
  DEVICE=${DEVICE:-$(xcrun devicectl list devices --json-output - 2>/dev/null | python3 -c '
import json, sys
for device in json.load(sys.stdin).get("result", {}).get("devices", []):
    hardware = device.get("hardwareProperties", {})
    connection = device.get("connectionProperties", {})
    if hardware.get("deviceType") == "iPhone" and connection.get("pairingState") == "paired" and connection.get("transportType") == "wired":
        print(device["identifier"])
        break
')}
  if [ -z "$DEVICE" ]; then
    echo
    echo "Built, but no device is connected. Plug the iPhone in, trust this Mac, and re-run."
    echo "App: $APP"
    exit 1
  fi
  xcrun devicectl device install app --device "$DEVICE" "$APP"
  DEVICECTL_CHILD_AESEL_ASK="$ASK" xcrun devicectl device process launch --device "$DEVICE" --terminate-existing computer.aesthetic.easel
  exit 0
fi

xcodebuild -project Aesel.xcodeproj -scheme Aesel \
  -sdk iphonesimulator \
  -destination 'platform=iOS Simulator,name=iPhone 17 Pro' \
  -derivedDataPath "$DERIVED" -jobs 2 build

xcrun simctl boot "iPhone 17 Pro" 2>/dev/null || true
open -a Simulator
xcrun simctl install booted "$DERIVED/Build/Products/Debug-iphonesimulator/Aesel.app"
xcrun simctl terminate booted computer.aesthetic.easel 2>/dev/null || true
if [ -n "$ASK" ]; then
  SIMCTL_CHILD_AESEL_ASK="$ASK" xcrun simctl launch booted computer.aesthetic.easel
else
  xcrun simctl launch booted computer.aesthetic.easel
fi
