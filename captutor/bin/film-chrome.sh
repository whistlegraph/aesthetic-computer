#!/bin/bash
# film-chrome — start the browser we record in, looking like a stock browser.
#
#   bin/film-chrome.sh [url] [port]
#
# Prefs (bookmarks bar, theme) can only be patched while Chrome is CLOSED — it
# rewrites Preferences on exit and would silently undo us — so prep-chrome kills
# it first. The flags below cover what Chrome exposes as switches; the rest lives
# in the profile.
set -e
URL="${1:-https://app.fuser.studio/w/me}"
PORT="${2:-9333}"
DIR="$HOME/.chrome-cdp"
NODE="$(command -v node || echo /opt/homebrew/bin/node)"

"$NODE" "$(dirname "$0")/prep-chrome.mjs" --dir "$DIR"

open -na "Google Chrome" --args \
  --remote-debugging-port="$PORT" \
  --user-data-dir="$DIR" \
  --profile-directory=Default \
  --no-first-run \
  --no-default-browser-check \
  --hide-crash-restore-bubble \
  --disable-session-crashed-bubble \
  --propagate-iph-for-testing \
  --window-size=1512,945 \
  "$URL"
sleep 8
echo "✓ filming browser up on :$PORT"
