#!/usr/bin/env bash
# Build and install the isolated Walkieware demo. DEVICE may select a paired phone.
# ./run.sh device | ./run.sh simulator
set -euo pipefail
cd "$(dirname "$0")"
node bundle.mjs
xcodegen generate >/dev/null
mode=${1:-device}
if [[ "$mode" == simulator ]]; then
  derived=${DERIVED:-/tmp/walkieware-sim-dd}
  simulator=${SIMULATOR:-FEB14FE3-7FDA-4D18-BCD6-F06509360BEA}
  xcodebuild -project Walkieware.xcodeproj -scheme Walkieware -configuration Debug -destination 'generic/platform=iOS Simulator' -derivedDataPath "$derived" -jobs 2 CODE_SIGNING_ALLOWED=NO build
  xcrun simctl boot "$simulator" 2>/dev/null || true
  xcrun simctl install "$simulator" "$derived/Build/Products/Debug-iphonesimulator/Walkieware.app"
  xcrun simctl launch "$simulator" computer.aesthetic.walkieware
elif [[ "$mode" == device ]]; then
  derived=${DERIVED:-/tmp/walkieware-dd}
  xcodebuild -project Walkieware.xcodeproj -scheme Walkieware -configuration Debug -destination 'generic/platform=iOS' -derivedDataPath "$derived" -allowProvisioningUpdates -jobs 2 build
  device=${DEVICE:-$(xcrun devicectl list devices --json-output - 2>/dev/null | python3 -c 'import json,sys; devices=json.load(sys.stdin).get("result",{}).get("devices",[]); matches=[d["identifier"] for d in devices if d.get("hardwareProperties",{}).get("deviceType")=="iPhone" and d.get("connectionProperties",{}).get("pairingState")=="paired"]; print(matches[0] if len(matches)==1 else "")')}
  [[ -n "$device" ]] || { echo 'Select a paired iPhone with DEVICE=<identifier>.' >&2; exit 1; }
  xcrun devicectl device install app --device "$device" "$derived/Build/Products/Debug-iphoneos/Walkieware.app"
  xcrun devicectl device process launch --device "$device" --terminate-existing computer.aesthetic.walkieware
else
  echo 'Usage: ./run.sh device|simulator' >&2; exit 1
fi
