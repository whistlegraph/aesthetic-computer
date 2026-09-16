#!/usr/bin/env bash
# Build Aesel and put it somewhere you can use it.
#
#   ./run.sh                 → the simulator
#   ./run.sh device          → the iPhone plugged into this Mac
#   ./run.sh device "ask…"   → same, running one prompt on launch
#
# The app loads its session from `easel/phone/serve.mjs` on this machine rather
# than from a bundled copy, so a JavaScript edit reaches the phone on relaunch
# with no rebuild. That address is a LAN IP, and a LAN IP is not stable — this
# Mac moved from a phone hotspot to Wi-Fi mid-session — so it is discovered at
# build time rather than committed.
set -euo pipefail

cd "$(dirname "$0")"
REPO=$(cd ../.. && pwd)
MODE=${1:-simulator}
ASK=${2:-}
PORT=8770
DERIVED=${DERIVED:-/tmp/aesel-dd}

lan() { ipconfig getifaddr en0 2>/dev/null || ipconfig getifaddr en1 2>/dev/null || echo 127.0.0.1; }

if ! curl -fsS -o /dev/null "http://localhost:$PORT/easel/phone/host.html" 2>/dev/null; then
  echo "starting the session server…"
  (cd "$REPO" && nohup node easel/phone/serve.mjs --port "$PORT" --token >/tmp/aesel-serve.log 2>&1 &)
  sleep 2
fi

xcodegen generate >/dev/null

if [ "$MODE" = "device" ]; then
  HOST="http://$(lan):$PORT/easel/phone/host.html"
  echo "session → $HOST"
  # The device has to reach this Mac, so the address is stamped into the built
  # Info.plist. The simulator shares the Mac's network and can just use
  # localhost, which is why only this branch rewrites the plist.
  /usr/libexec/PlistBuddy -c "Set :AeselHostURL $HOST" Info.plist

  xcodebuild -project Aesel.xcodeproj -scheme Aesel \
    -destination 'generic/platform=iOS' \
    -derivedDataPath "$DERIVED" \
    -allowProvisioningUpdates build

  APP="$DERIVED/Build/Products/Debug-iphoneos/Aesel.app"
  DEVICE=$(xcrun devicectl list devices 2>/dev/null | awk '/connected/ {print $(NF-1); exit}')
  if [ -z "$DEVICE" ]; then
    echo
    echo "Built, but no device is connected. Plug the iPhone in, trust this Mac, and re-run."
    echo "App: $APP"
    exit 1
  fi
  xcrun devicectl device install app --device "$DEVICE" "$APP"
  xcrun devicectl device process launch --device "$DEVICE" computer.aesthetic.aesel
  exit 0
fi

xcodebuild -project Aesel.xcodeproj -scheme Aesel \
  -sdk iphonesimulator \
  -destination 'platform=iOS Simulator,name=iPhone 17 Pro' \
  -derivedDataPath "$DERIVED" build

xcrun simctl boot "iPhone 17 Pro" 2>/dev/null || true
open -a Simulator
xcrun simctl install booted "$DERIVED/Build/Products/Debug-iphonesimulator/Aesel.app"
xcrun simctl terminate booted computer.aesthetic.aesel 2>/dev/null || true
if [ -n "$ASK" ]; then
  SIMCTL_CHILD_AESEL_ASK="$ASK" xcrun simctl launch booted computer.aesthetic.aesel
else
  xcrun simctl launch booted computer.aesthetic.aesel
fi
