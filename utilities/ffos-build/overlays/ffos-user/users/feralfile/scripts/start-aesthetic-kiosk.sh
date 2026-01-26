#!/usr/bin/env bash
set -euo pipefail

AC_URL="${AC_URL:-https://aesthetic.computer?tv=true&nogap=true&nolabel=true}"

# Launch Chromium in kiosk mode for Aesthetic Computer
# Note: FFOS ships Chromium kiosk service; this script mirrors the same style.
exec chromium \
  --kiosk \
  --incognito \
  --no-first-run \
  --no-default-browser-check \
  --disable-session-crashed-bubble \
  --disable-translate \
  --disable-features=TranslateUI \
  --autoplay-policy=no-user-gesture-required \
  "$AC_URL"
