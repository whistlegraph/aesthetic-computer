#!/usr/bin/env bash
# Remote-control MIME on a phone through UITests/RemoteDriver.swift.
#   tools/drive.sh start            build for testing, install, run the driver (background)
#   tools/drive.sh shot [out.png]   screenshot
#   tools/drive.sh tap X Y | tap-id ID | doubletap X Y | pinch [SCALE] | swipe X1 Y1 X2 Y2 [HOLD]
#                 type TEXT | tree | home | stop
set -euo pipefail
cd "$(dirname "$0")/.."
device=${DEVICE:?Set DEVICE=<phone identifier>}
port=${MIME_DRIVER_PORT:-8765}
derived=${DERIVED:-/tmp/mime-dd}
host() { xcrun devicectl device info details --device "$device" 2>/dev/null | awk '/Tunnel IP Address/ {print $NF}'; }
url() { echo "http://[$(host)]:$port$1"; }
enc() { python3 -c 'import sys,urllib.parse; print(urllib.parse.quote(sys.argv[1]))' "$1"; }
case "${1:-}" in
  start)
    node ../whistlegraph/bundle.mjs >/dev/null
    xcodegen generate >/dev/null
    xcodebuild build-for-testing -project Mime.xcodeproj -scheme Mime -destination "id=$device" \
      -derivedDataPath "$derived" -allowProvisioningUpdates -jobs 2 -quiet
    TEST_RUNNER_MIME_DRIVER=1 TEST_RUNNER_MIME_DRIVER_PORT=$port \
      xcodebuild test-without-building -project Mime.xcodeproj -scheme Mime -destination "id=$device" \
      -derivedDataPath "$derived" -only-testing:MimeUITests/RemoteDriver/testRemote ;;
  shot) curl -sf "$(url /shot)" -o "${2:-shot.png}" && echo "${2:-shot.png}" ;;
  tap) curl -sf "$(url "/tap?x=$2&y=$3")" ;;
  tap-id) curl -sf "$(url "/tap?id=$(enc "$2")")" ;;
  doubletap) curl -sf "$(url "/doubletap?x=$2&y=$3")" ;;
  pinch) curl -sf "$(url "/pinch?scale=${2:-3}&v=${3:-2}")" ;;
  swipe) curl -sf "$(url "/swipe?x1=$2&y1=$3&x2=$4&y2=$5&t=${6:-0.05}")" ;;
  type) curl -sf "$(url "/type?text=$(enc "$2")")" ;;
  tree|home|launch|activate|stop) curl -sf "$(url "/$1")" ;;
  *) sed -n 2,5p "$0"; exit 1 ;;
esac
