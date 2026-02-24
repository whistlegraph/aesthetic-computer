#!/bin/bash
# FedAC Kiosk Session — cage as Wayland compositor + Firefox
# No GNOME Shell, no GDM, no desktop. Cage: zero background flash.
export XDG_SESSION_TYPE=wayland
export XDG_RUNTIME_DIR="/run/user/$(id -u)"

# Paint framebuffer black — belt-and-suspenders with the initrd hook
dd if=/dev/zero of=/dev/fb0 bs=4M 2>/dev/null || true

# Start piece server inline (socket-backed, immediate listen)
python3 /usr/local/bin/kiosk-piece-server.py &

# Wait for piece server (socket activation means it's instant, but be safe)
for i in $(seq 1 15); do
  curl -s -o /dev/null http://127.0.0.1:8080 && break
  sleep 1
done

# cage: purpose-built kiosk compositor, no background, no shell, 50ms start
exec cage -- \
  firefox --kiosk --no-remote \
  --profile /home/liveuser/.mozilla/firefox/kiosk \
  http://127.0.0.1:8080
