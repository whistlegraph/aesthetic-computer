#!/bin/bash
# gfn-window.sh — let the GeForce NOW window on a Mac get small.
#
#   gfn-window.sh shrink [W,H]   lower the window floor (default 480,300) and
#                                keep the stream windowed; relaunches GFN
#   gfn-window.sh restore        put NVIDIA's original config back
#   gfn-window.sh status         show the current floor and whether it's applied
#
# Why: GeForce NOW's Mac client refuses to go below 900×600 no matter how you
# drag it, and the floor is not in Settings. It lives in a switch list inside
# the app bundle, /Applications/GeForceNOW.app/Contents/Resources/GeForceNOW.json:
#
#   "nv-min-window-size=900,600"    the floor  (this script lowers it)
#   "nv-sdl-force-windowed=true"    added: the stream stays a window instead
#                                   of taking its own full-screen Space
#
# Passing --nv-min-window-size on the command line does nothing; the bundle
# JSON wins (measured 2026-09-18). Editing the file is the only route.
#
# Caveats:
#   * Editing the bundle breaks its code signature. Already-installed GFN
#     keeps launching fine (tested macOS 26/27); a fresh download would need
#     to be opened once before shrinking.
#   * GFN self-updates on launch ("nv-startup-autoupdate"). An update rewrites
#     this file — run `shrink` again if the floor comes back.
#   * The floor is a minimum, not a size: after shrinking, drag the window to
#     whatever you like. Both axes clamp independently; no aspect lock.
#
# Nothing else about the app is touched. The original file is saved once to
# ~/.config/gfn-window/GeForceNOW.json.stock and `restore` copies it back.

set -eu
APP=/Applications/GeForceNOW.app
JSON="$APP/Contents/Resources/GeForceNOW.json"
STOCK="$HOME/.config/gfn-window/GeForceNOW.json.stock"
DEFAULT_FLOOR=480,300

die() { echo "gfn-window: $*" >&2; exit 1; }
[ -f "$JSON" ] || die "GeForce NOW is not installed at $APP"

floor() { grep -o '"nv-min-window-size=[0-9]*,[0-9]*"' "$JSON" | tr -d '"' | cut -d= -f2; }
windowed() { grep -q '"nv-sdl-force-windowed=true"' "$JSON" && echo yes || echo no; }
running() { pgrep -x GeForceNOW >/dev/null; }

# The file is admin-writable on a normal install; fall back to sudo otherwise.
write() { # $1 = temp file with the new contents
  if [ -w "$JSON" ]; then cp "$1" "$JSON"; else sudo cp "$1" "$JSON"; fi
}

relaunch() {
  if running; then
    echo "quitting GeForce NOW (any active stream ends)"
    osascript -e 'tell application "GeForceNOW" to quit' 2>/dev/null || pkill -x GeForceNOW
    for _ in 1 2 3 4 5 6 7 8 9 10; do running || break; sleep 1; done
  fi
  open -a GeForceNOW
}

status() {
  echo "floor:     $(floor)  (stock 900,600)"
  echo "windowed:  $(windowed)"
  echo "backup:    $( [ -f "$STOCK" ] && echo "$STOCK" || echo none )"
  echo "running:   $( running && echo yes || echo no )"
}

case "${1:-status}" in
  shrink)
    NEW="${2:-$DEFAULT_FLOOR}"
    echo "$NEW" | grep -Eq '^[0-9]+,[0-9]+$' || die "size must look like W,H — got '$NEW'"
    if [ ! -f "$STOCK" ]; then
      mkdir -p "$(dirname "$STOCK")"; cp "$JSON" "$STOCK"; echo "saved original to $STOCK"
    fi
    TMP=$(mktemp)
    sed -e "s/\"nv-min-window-size=[0-9]*,[0-9]*\"/\"nv-min-window-size=$NEW\"/" "$JSON" > "$TMP"
    if ! grep -q '"nv-sdl-force-windowed=true"' "$TMP"; then
      # Slot it after the resizable switch; both lines carry a trailing comma.
      sed -i '' 's/^\( *\)"nv-sdl-resizable=true",$/&\
\1"nv-sdl-force-windowed=true",/' "$TMP"
    fi
    grep -q "nv-min-window-size=$NEW" "$TMP" || die "could not find the floor switch in $JSON — NVIDIA changed the file?"
    write "$TMP"; rm -f "$TMP"
    echo "floor is now $NEW, stream stays windowed"
    relaunch; status ;;
  restore)
    [ -f "$STOCK" ] || die "no backup at $STOCK — nothing to restore"
    write "$STOCK"; echo "original config restored"
    relaunch; status ;;
  status) status ;;
  *) sed -n '2,7p' "$0"; exit 1 ;;
esac
