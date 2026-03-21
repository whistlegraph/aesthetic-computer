#!/usr/bin/env fish
# Start Chromium with remote debugging for MCP
chromium-browser --remote-debugging-port=9222 --user-data-dir=/tmp/chrome-profile-mcp --no-sandbox --disable-setuid-sandbox --headless --disable-dev-shm-usage --disable-gpu &
echo "Chromium started with remote debugging on port 9222"
