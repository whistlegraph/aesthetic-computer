#!/bin/bash
# Wrapper to handle Host header requirement for Chrome DevTools Protocol
# VS Code requires Host: localhost even when connecting via host.docker.internal
export CHROME_REMOTE_DEBUGGING_HOST="host.docker.internal"
export PATH="/usr/sbin:$PATH"
exec npx -y chrome-devtools-mcp@latest --browserUrl=http://host.docker.internal:9222
