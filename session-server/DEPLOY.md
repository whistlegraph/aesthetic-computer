# Session Server Deployment Guide

## Production Server
- **Host**: session-server.aesthetic.computer (157.245.134.225)
- **User**: root
- **Path**: `/root/aesthetic-computer/session-server`

## Deploy from Local Machine

Since the production server requires specific SSH keys not in the dev container, deploy from your local machine where you have SSH access:

```bash
# One-liner deployment (run from your local terminal)
ssh root@157.245.134.225 "cd /root/aesthetic-computer && git pull origin main && cd session-server && npm install && pm2 restart session-server && pm2 logs session-server --lines 20 --nostream"
```

## What This Does

1. ✅ SSH into production server
2. ✅ Pull latest code from GitHub main branch
3. ✅ Install any new npm dependencies
4. ✅ Restart session server process with pm2
5. ✅ Show last 20 log lines to verify deployment

## Verify Deployment

After deploying, test the new endpoints:

```bash
# Test build-stream endpoint (from local machine)
curl -k -X POST https://session-server.aesthetic.computer/build-stream \
  --header "Content-Type: application/json" \
  --data '{"line": "🔨 Test deployment"}'

# Expected response:
# {"status":"ok"}
```

## Monitor Production

```bash
# View live logs
ssh root@157.245.134.225 "pm2 logs session-server"

# Check process status  
ssh root@157.245.134.225 "pm2 status"

# Restart if needed
ssh root@157.245.134.225 "pm2 restart session-server"
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
