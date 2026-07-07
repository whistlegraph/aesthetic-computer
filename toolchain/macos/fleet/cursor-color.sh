#!/bin/bash
# cursor-color.sh — set the macOS pointer fill color per fleet Mac. Idempotent.
#
#   bash cursor-color.sh <host> [<host> ...]   # each host gets its mapped color
#   bash cursor-color.sh all                   # neo chicken panda (blueberry: run directly)
#   bash cursor-color.sh blueberry red         # override: force a named color
#
# Pointer colors live in the com.apple.universalaccess defaults domain as the
# NSColor-style dicts `cursorFill` (the fill) and `cursorOutline` (always white
# here). The system Pointer settings write the same keys. universalaccessd is
# SIP-protected so it can't be hot-reloaded from the CLI — the new color shows on
# the next login or lock/unlock (⌃⌘Q), which is when accessibility prefs re-read.
#
# Per-machine mapping keeps each Mac visually identifiable by its pointer:
#   neo=green  blueberry=blue  (others fall through to the default black cursor).
set -euo pipefail

# named color -> "red green blue" (0..1), alpha is always 1
color_rgb() {
  case "$1" in
    green)  echo "0 0.5603182912 0" ;;   # neo's exact green
    blue)   echo "0 0 1" ;;
    red)    echo "1 0 0" ;;
    purple) echo "0.5 0 1" ;;
    orange) echo "1 0.5 0" ;;
    white)  echo "1 1 1" ;;
    black|default) echo "" ;;            # empty => clear the key (default cursor)
    *) echo "__unknown__" ;;
  esac
}

# host -> default color
host_color() {
  case "$1" in
    neo)       echo "green" ;;
    blueberry) echo "blue" ;;
    *)         echo "default" ;;
  esac
}

hosts=("$@")
[ "${hosts[0]:-}" = "all" ] && hosts=(neo chicken panda)
[ ${#hosts[@]} -eq 0 ] && { echo "usage: bash cursor-color.sh <host> [host...] | all   [color]"; exit 1; }

# optional trailing color override: bash cursor-color.sh <host> <color>
override=""
if [ ${#hosts[@]} -eq 2 ] && [ "$(color_rgb "${hosts[1]}")" != "__unknown__" ]; then
  override="${hosts[1]}"; hosts=("${hosts[0]}")
fi

for host in "${hosts[@]}"; do
  color="${override:-$(host_color "$host")}"
  rgb="$(color_rgb "$color")"
  [ "$rgb" = "__unknown__" ] && { echo "=== $host: unknown color '$color', skipping ==="; continue; }
  echo "=== $host -> $color ==="
  ssh -o ConnectTimeout=8 "$host" "bash -lc '
    if [ -z \"$rgb\" ]; then
      defaults delete com.apple.universalaccess cursorFill 2>/dev/null || true
      defaults delete com.apple.universalaccess cursorOutline 2>/dev/null || true
      echo \"cleared cursor color (default)\"
    else
      set -- $rgb
      defaults write com.apple.universalaccess cursorFill -dict alpha 1 red \$1 green \$2 blue \$3
      defaults write com.apple.universalaccess cursorOutline -dict alpha 1 red 1 green 1 blue 1
      echo \"set cursorFill r=\$1 g=\$2 b=\$3\"
    fi
  '"
  echo "  (applies on next login / lock-unlock — ⌃⌘Q)"
done
