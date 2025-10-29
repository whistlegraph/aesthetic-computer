# Oven Local Development Setup

## Quick Start

```fish
# Start the oven service
ac-oven
```

This will:
- Navigate to ~/aesthetic-computer/oven
- Clean up any stuck processes
- Kill port 3002
- Start the oven server with auto-reload on https://localhost:3002

## Architecture

### Production Flow
1. **track-media.mjs** (Netlify) → Creates tape record, POSTs to `https://oven.aesthetic.computer/bake`
2. **oven** → Downloads ZIP, processes video, uploads to Spaces
3. **oven** → Calls webhook `POST /api/tape-mp4-complete` on Netlify
4. **tape-mp4-complete.mjs** (Netlify) → Downloads MP4/thumbnail, syncs to ATProto

### Local Dev Flow
1. **track-media.mjs** (Netlify dev) → Creates tape record, POSTs to `https://localhost:3002/bake`
2. **oven** (local) → Downloads ZIP, processes video, uploads to Spaces
3. **oven** (local) → Calls webhook `POST localhost:8888/api/tape-mp4-complete`
4. **tape-mp4-complete.mjs** (Netlify dev) → Downloads MP4/thumbnail, syncs to ATProto

## Key URLs

- **Oven dashboard**: https://localhost:3002/
- **Health check**: https://localhost:3002/health
- **Status API**: https://localhost:3002/status
- **Bake endpoint**: https://localhost:3002/bake (POST)
- **WebSocket**: wss://localhost:3002/ws

## Environment Detection

The oven service is automatically selected based on environment:

```javascript
const isDev = process.env.CONTEXT === 'dev' || process.env.NODE_ENV === 'development';
const ovenUrl = isDev ? 'https://localhost:3002' : 'https://oven.aesthetic.computer';
```

## Testing Locally

1. Start the oven service:
   ```fish
   ac-oven
   ```

2. Start Netlify dev (in another terminal):
   ```fish
   ac-site
   ```

3. Upload a tape through the site - it will automatically use the local oven

4. Monitor the oven dashboard at https://localhost:3002/ to see live processing status

## Emacs Tab Configuration

Add to your Emacs aesthetic-backend configuration:

```elisp
;; Example tab configuration for oven
(add-to-list 'aesthetic-tabs
  '("oven" . (lambda ()
               (aesthetic-run-in-tab "oven" "ac-oven"))))
```

This allows you to switch to the oven tab using your Emacs aesthetic interface.

## Files Modified

- `/oven/*` - New oven service
- `system/netlify/functions/track-media.mjs` - Updated to call oven instead of inline processing
- `system/netlify/functions/tape-mp4-complete.mjs` - Updated webhook to handle oven callbacks
- `.devcontainer/config.fish` - Added `ac-oven` function

## Code-Based File Naming

Both guest and user tapes now use **code** for file identification:

- Guest ZIP: `art-aesthetic-computer/tapes/{code}.zip`
- User ZIP: `user-aesthetic-computer/tapes/{code}.zip` (future)
- Output MP4: `at-blobs-aesthetic-computer/tapes/{code}.mp4`
- Output thumbnail: `at-blobs-aesthetic-computer/tapes/{code}-thumb.jpg`

This provides consistency across the database and storage layer.

## Next Steps

1. Test full local flow (upload → oven → webhook → atproto)
2. Add oven tab to Emacs configuration
3. Deploy oven to DigitalOcean droplet
4. Set up production environment variables
5. Configure Caddy for HTTPS on oven.aesthetic.computer
