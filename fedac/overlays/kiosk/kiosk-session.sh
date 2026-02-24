#!/bin/bash
# FedAC Kiosk Session — cage as Wayland compositor + Firefox
# No GNOME Shell, no GDM, no desktop. Cage: zero background flash.
export XDG_SESSION_TYPE=wayland
export XDG_RUNTIME_DIR="/run/user/$(id -u)"

# Silence tty1 output to log (Plymouth messages go via `plymouth` binary, not stdout)
exec > /tmp/kiosk.log 2>&1

# Start piece server inline
plymouth message --text="Starting piece server..." 2>/dev/null || true
python3 /usr/local/bin/kiosk-piece-server.py &

# Wait for piece server (up to 15s)
for i in $(seq 1 15); do
  curl -s -o /dev/null http://127.0.0.1:8080 && break
  sleep 1
done

# Hand off Plymouth — retain its last frame until cage/Firefox paints over it
plymouth message --text="Launching..." 2>/dev/null || true
sleep 0.1
plymouth quit --retain-splash 2>/dev/null || true

# Belt-and-suspenders fb0 blackout
dd if=/dev/zero of=/dev/fb0 bs=4M 2>/dev/null || true

# cage: purpose-built kiosk compositor, no background, no shell, 50ms start
# --remote-debugging-port=9222 allows SSH-tunnel console access for debugging
exec cage -- \
  firefox --fullscreen --no-remote \
  --remote-debugging-port=9222 \
  --profile /home/liveuser/.mozilla/firefox/kiosk \
  http://127.0.0.1:8080
