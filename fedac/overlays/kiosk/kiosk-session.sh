#!/bin/bash
# FedAC Kiosk Session — mutter as bare Wayland compositor + Firefox
# No GNOME Shell, no GDM, no desktop.
export XDG_SESSION_TYPE=wayland
export XDG_RUNTIME_DIR="/run/user/$(id -u)"

# Paint framebuffer black immediately — eliminates mutter compositor flash
dd if=/dev/zero of=/dev/fb0 bs=4M 2>/dev/null || true

# Start piece server inline (don't rely on systemd service timing)
python3 /usr/local/bin/kiosk-piece-server.py &

# Wait up to 15s for piece server to be ready
for i in $(seq 1 15); do
  curl -s -o /dev/null http://127.0.0.1:8080 && break
  sleep 1
done

exec mutter --wayland --no-x11 -- \
  firefox --kiosk --no-remote \
  --profile /home/liveuser/.mozilla/firefox/kiosk \
  http://127.0.0.1:8080
