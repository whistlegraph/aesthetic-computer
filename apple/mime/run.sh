#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
xcodegen generate >/dev/null
derived=${DERIVED:-/tmp/mime-dd}
xcodebuild -project Mime.xcodeproj -scheme Mime -configuration Debug -destination 'generic/platform=iOS' -derivedDataPath "$derived" -allowProvisioningUpdates -jobs 2 build
device=${DEVICE:-$(xcrun devicectl list devices --json-output - 2>/dev/null | python3 -c 'import json,sys; devices=json.load(sys.stdin).get("result",{}).get("devices",[]); matches=[d["identifier"] for d in devices if d.get("hardwareProperties",{}).get("deviceType")=="iPhone" and d.get("hardwareProperties",{}).get("reality")=="physical" and d.get("connectionProperties",{}).get("pairingState")=="paired" and d.get("connectionProperties",{}).get("tunnelState")=="connected"]; print(matches[0] if len(matches)==1 else "")')}
[[ -n "$device" ]] || { echo 'Select the connected iPhone with DEVICE=<identifier>.' >&2; exit 1; }
xcrun devicectl device install app --device "$device" "$derived/Build/Products/Debug-iphoneos/Mime.app"
xcrun devicectl device process launch --device "$device" --terminate-existing computer.aesthetic.mime
