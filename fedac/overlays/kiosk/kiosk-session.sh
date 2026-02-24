#!/bin/bash
# FedAC Kiosk Session — cage as Wayland compositor + Firefox
# No GNOME Shell, no GDM, no desktop. Cage: zero background flash.
export XDG_SESSION_TYPE=wayland
export XDG_RUNTIME_DIR="/run/user/$(id -u)"

exec > /tmp/kiosk.log 2>&1

# cage: purpose-built kiosk compositor, no background, no shell, 50ms start
# --remote-debugging-port=9222 allows SSH-tunnel console access for debugging
# Load bundle directly from disk — no HTTP server needed
exec cage -- \
  firefox --fullscreen --no-remote \
  --remote-debugging-port=9222 \
  --profile /home/liveuser/.mozilla/firefox/kiosk \
  file:///usr/local/share/kiosk/piece.html
