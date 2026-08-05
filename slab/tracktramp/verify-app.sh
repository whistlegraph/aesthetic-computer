#!/usr/bin/env bash
set -euo pipefail

APP="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/build/TrackDrum.app}"
BIN="${APP}/Contents/MacOS/TrackDrum"

[[ -x "${BIN}" ]] || { echo "missing TrackDrum executable" >&2; exit 1; }
if [[ ! -f "${APP}/Contents/Resources/TracktrampShaders.metallib" \
      && ! -f "${APP}/Contents/Resources/TracktrampShaders.metalsource" ]]; then
    echo "missing Metal shader" >&2
    exit 1
fi
[[ -f "${APP}/Contents/Resources/TrackDrumIcon.icns" ]] || {
    echo "missing app icon" >&2; exit 1;
}
[[ ! -e "${APP}/Contents/MacOS/TrackDrumLauncher" ]] || {
    echo "legacy launcher must not ship" >&2; exit 1;
}

codesign --verify --deep --strict --verbose=2 "${APP}"
SANDBOX="$(codesign -d --entitlements :- "${APP}" 2>/dev/null \
    | plutil -extract 'com\.apple\.security\.app-sandbox' raw -)"
[[ "${SANDBOX}" == "true" ]] || { echo "App Sandbox is missing" >&2; exit 1; }

if strings "${BIN}" | grep -Eq \
    'MultitouchSupport\.framework|MTDeviceCreateList|CGEventTapCreate|Library/LaunchAgents'; then
    echo "private/global input implementation leaked into TrackDrum" >&2
    exit 1
fi

ARCHS="$(lipo -archs "${BIN}")"
[[ " ${ARCHS} " == *" arm64 "* && " ${ARCHS} " == *" x86_64 "* ]] || {
    echo "TrackDrum must be universal: ${ARCHS}" >&2; exit 1;
}

echo "✓ sandboxed public-input TrackDrum (${ARCHS})"
