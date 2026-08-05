#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_BIN="${SCRIPT_DIR}/build/TrackDrumContactTests"
mkdir -p "${SCRIPT_DIR}/build"
xcrun swiftc \
    "${SCRIPT_DIR}/Sources/TrackDrumContact.swift" \
    "${SCRIPT_DIR}/Tests/main.swift" \
    -o "${TEST_BIN}"
"${TEST_BIN}"
