#!/bin/bash
# kiosk-start.sh — Launch Firefox kiosk
# Two modes:
#   Online:  WiFi picker → kidlisp.com
#   Offline: local piece server at localhost:8080

sleep 1

# Wait for piece server
for i in $(seq 1 15); do
  curl -s -o /dev/null http://127.0.0.1:8080 && break
  sleep 1
done

# Check if online → go to kidlisp.com, otherwise use local piece server
if nmcli -t -f STATE general 2>/dev/null | grep -q "^connected"; then
  exec firefox --kiosk --no-remote https://kidlisp.com
fi

exec firefox --kiosk --no-remote http://127.0.0.1:8080
