#!/bin/bash
# kiosk-start.sh — Launch Firefox kiosk
# If online → go straight to kidlisp.com
# Otherwise → open local WiFi picker at localhost:8080
# Runs as liveuser via XDG autostart

sleep 2  # Wait for desktop + wifi server to be ready

# Check if already connected
if nmcli -t -f STATE general 2>/dev/null | grep -q "^connected"; then
  exec firefox --kiosk https://kidlisp.com
fi

# Not connected — open WiFi picker (served by kiosk-wifi-server.py)
# The page auto-redirects to kidlisp.com after successful connection
exec firefox --kiosk http://127.0.0.1:8080
