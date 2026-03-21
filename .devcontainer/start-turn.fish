#!/usr/bin/env fish
# Start coturn TURN server for WebRTC relay
# This is needed because Docker containers can't expose their internal IPs

# Get the host's LAN IP (passed from host or detected)
set -l EXTERNAL_IP (cat /tmp/host-lan-ip 2>/dev/null; or echo "192.168.1.127")

echo "🔄 Starting TURN server with external IP: $EXTERNAL_IP"

# Kill any existing turnserver
pkill turnserver 2>/dev/null

# Start turnserver in background
turnserver -c /workspaces/aesthetic-computer/.devcontainer/turnserver.conf \
    --external-ip=$EXTERNAL_IP \
    --no-daemon &

echo "🔄 TURN server started on port 3478 (STUN) and 3478 (TURN)"
