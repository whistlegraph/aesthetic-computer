#!/usr/bin/env bash
# Five screenshots of the iPhone Mirroring window, five seconds apart.
# Window position+size is re-queried every iteration so it tracks any
# move or resize between shots.
#
# Heads up: iPhone Mirroring may render as solid black in captures
# because of Continuity privacy. If that's what you see, take the
# screenshot from the iPhone itself instead (it's only used here to
# locate the Capture button visually for one-time calibration).
#
# Usage:  ./shoot.sh [output-dir]
# Default output: ~/Desktop/iphone-mirror-shots

set -e
OUT="${1:-$HOME/Desktop/iphone-mirror-shots}"
mkdir -p "$OUT"

bounds() {
  osascript <<'OSA'
tell application "System Events"
  if not (exists process "iPhone Mirroring") then return "missing"
  tell process "iPhone Mirroring"
    if (count of windows) is 0 then return "missing"
    set p to position of window 1
    set s to size of window 1
    set AppleScript's text item delimiters to ","
    return ((p & s) as text)
  end tell
end tell
OSA
}

for i in 1 2 3 4 5; do
  b=$(bounds)
  if [ "$b" = "missing" ]; then
    echo "[$i] iPhone Mirroring isn't running. Open it and rerun."
    exit 1
  fi
  ts=$(date +%H%M%S)
  file="$OUT/shot-$i-$ts.png"
  echo "[$i] bounds=$b -> $file"
  screencapture -R "$b" -x "$file"
  [ "$i" -lt 5 ] && sleep 5
done

echo
echo "Done. Open: $OUT"
open "$OUT"
