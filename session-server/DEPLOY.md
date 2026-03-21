# Session Server Deployment Guide

## Production Server
- **Host**: session-server.aesthetic.computer (157.245.134.225)
- **User**: root
- **Path**: `/home/aesthetic-computer/session-server`
- **Node**: Managed via `fnm` (Fast Node Manager)
- **Process**: `pm2` (name: `session`)

## Deploy from DevContainer

The SSH key for the session server is available in the dev container at `~/.ssh/session_server`:

```bash
# One-liner deployment
ssh -i ~/.ssh/session_server root@157.245.134.225 'export PATH="/root/.local/share/fnm:$PATH" && eval "$(fnm env)" && cd /home/aesthetic-computer && git pull origin main && cd session-server && npm install && pm2 restart session && pm2 logs session --lines 20 --nostream'
```

Or use the fish function (recommended):
```fish
ac-deploy-session
```

## What This Does

1. ✅ SSH into production server
2. ✅ Load fnm environment (for node/npm/pm2)
3. ✅ Pull latest code from GitHub main branch
4. ✅ Install any new npm dependencies
5. ✅ Restart session server process with pm2
6. ✅ Show last 20 log lines to verify deployment

## Verify Deployment

After deploying, test the endpoints:

```bash
# Test socklogs WebSocket endpoint
websocat "wss://session-server.aesthetic.computer/socklogs?role=viewer"

# Test build-stream endpoint
curl -X POST https://session-server.aesthetic.computer/build-stream \
  --header "Content-Type: application/json" \
  --data '{"line": "🔨 Test deployment"}'

# Expected response:
# {"status":"ok"}
```

## Monitor Production

```bash
# Set up fnm first for any pm2 commands
FNM='export PATH="/root/.local/share/fnm:$PATH" && eval "$(fnm env)"'

# View live logs
ssh -i ~/.ssh/session_server root@157.245.134.225 "$FNM && pm2 logs session"

# Check process status  
ssh -i ~/.ssh/session_server root@157.245.134.225 "$FNM && pm2 status"

# Restart if needed
ssh -i ~/.ssh/session_server root@157.245.134.225 "$FNM && pm2 restart session"
```

## Local Testing (Already Verified ✅)

The endpoints work locally at `https://localhost:8889`:

- `/build-stream` - Accepts log lines and broadcasts to WebSocket clients
- `/build-status` - Accepts status updates and broadcasts to WebSocket clients
- Both endpoints are dev-mode only for security

## Changes Deployed

- Added `/build-stream` POST endpoint (session.mjs line ~220)
- Added `/build-status` POST endpoint (session.mjs line ~225)
- Both broadcast to all connected WebSocket clients via `everyone()`
- Message types: `build:log` and `build:status`
- Dev-mode only (protected by `if (dev)` block)

## Next Steps After Deployment

1. Run a build with `./remote-update-and-build.fish`
2. Open https://builds.false.work in browser
3. Watch live build progress stream appear in real-time
4. Build logs will show with terminal-style formatting
