#!/usr/bin/env bash
# Geometry checks for the phone edition, built for the HOST so they can run
# anywhere — the transform and the ring math are platform-free on purpose, and
# they are compiled here against the real engine so the drawn rings are checked
# against the heard zones rather than against a restatement of them.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MENUBAND="${SCRIPT_DIR}/../../slab/menuband/Sources/MenuBand"
BIN="${SCRIPT_DIR}/build/TrackDrumGeometryTests"

mkdir -p "${SCRIPT_DIR}/build"
xcrun swiftc \
    "${SCRIPT_DIR}/Sources/TrackDrumGeometry.swift" \
    "${MENUBAND}/MenuBandPercussion.swift" \
    "${SCRIPT_DIR}/Tests/main.swift" \
    -o "${BIN}"
"${BIN}"
