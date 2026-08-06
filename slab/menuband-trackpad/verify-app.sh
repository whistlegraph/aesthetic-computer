#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP="${1:-${SCRIPT_DIR}/build/TrackDrum for Menu Band.app}"
BIN="${APP}/Contents/MacOS/MenuBandTrackpad"

[[ -x "${BIN}" ]] || { echo "missing helper executable" >&2; exit 1; }
codesign --verify --deep --strict --verbose=2 "${APP}"

ENTITLEMENTS="$(codesign -d --entitlements :- "${APP}" 2>/dev/null || true)"
if grep -q 'com.apple.security.app-sandbox' <<<"${ENTITLEMENTS}"; then
    echo "helper must not be sandboxed" >&2
    exit 1
fi

[[ "$(plutil -extract LSUIElement raw -o - "${APP}/Contents/Info.plist")" == "true" ]] || {
    echo "helper must be headless (LSUIElement)" >&2; exit 1;
}

if ! strings "${BIN}" | grep -q 'MultitouchSupport.framework'; then
    echo "global trackpad implementation is missing" >&2
    exit 1
fi
if strings "${BIN}" | grep -Eq 'AVAudioEngine|TracktrampMetalView|NSWindow'; then
    echo "audio or standalone instrument UI leaked into helper" >&2
    exit 1
fi

ARCHS="$(lipo -archs "${BIN}")"
[[ " ${ARCHS} " == *" arm64 "* && " ${ARCHS} " == *" x86_64 "* ]] || {
    echo "helper must be universal: ${ARCHS}" >&2; exit 1;
}

echo "✓ headless Menu Band trackpad bridge (${ARCHS})"
