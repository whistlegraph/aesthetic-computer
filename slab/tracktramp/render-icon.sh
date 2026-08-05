#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT="${1:?usage: render-icon.sh OUTPUT.icns [WORK_DIR]}"
WORK_DIR="${2:-$(mktemp -d /tmp/trackdrum-icon.XXXXXX)}"
SDK="$(xcrun --sdk macosx --show-sdk-path)"
RENDERER="${WORK_DIR}/render-trackdrum-icon"
PNG="${WORK_DIR}/TrackDrumIcon-1024.png"
ICONSET="${WORK_DIR}/TrackDrumIcon.iconset"

mkdir -p "${WORK_DIR}" "$(dirname "${OUTPUT}")"
xcrun swiftc -O -sdk "${SDK}" \
    "${SCRIPT_DIR}/Sources/TrackDrumIcon.swift" \
    "${SCRIPT_DIR}/render-icon.swift" \
    -o "${RENDERER}"
"${RENDERER}" "${PNG}"
mkdir -p "${ICONSET}"
for spec in "16:icon_16x16.png" "32:icon_16x16@2x.png" \
            "32:icon_32x32.png" "64:icon_32x32@2x.png" \
            "128:icon_128x128.png" "256:icon_128x128@2x.png" \
            "256:icon_256x256.png" "512:icon_256x256@2x.png" \
            "512:icon_512x512.png" "1024:icon_512x512@2x.png"; do
    size="${spec%%:*}"
    name="${spec#*:}"
    sips -z "${size}" "${size}" "${PNG}" --out "${ICONSET}/${name}" >/dev/null
done
iconutil -c icns "${ICONSET}" -o "${OUTPUT}"
