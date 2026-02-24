#!/bin/bash
# FedAC Kiosk Session — mutter as bare Wayland compositor + Firefox
# No GNOME Shell, no GDM, no desktop.
export XDG_SESSION_TYPE=wayland
export XDG_RUNTIME_DIR="/run/user/$(id -u)"

# Wait for piece server (up to 30s)
for i in $(seq 1 30); do
  curl -s -o /dev/null http://127.0.0.1:8080 && break
  sleep 1
done

exec mutter --wayland --no-x11 -- firefox --kiosk --no-remote http://127.0.0.1:8080
