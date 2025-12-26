# Oven: Unified Capture & Bake Service

**Status**: In Progress  
**Last Updated**: December 26, 2025

## Overview

Oven (`oven.aesthetic.computer`) is a unified service for capturing screenshots/animations and processing video tapes. It replaces the previous fragmented grab.aesthetic.computer service.

## Architecture

- **Server**: Express.js on DigitalOcean droplet (137.184.237.166)
- **Storage**: MongoDB for persistence, DigitalOcean Spaces for CDN
- **Capture**: Puppeteer for headless browser screenshots
- **Processing**: FFmpeg for video/animation encoding

## Completed Features ✅

### Dashboard (`/`)
- [x] Unified "bakes" grid showing all capture types (tape, grab, icon, preview)
- [x] Type filter checkboxes to show/hide specific types
- [x] Letterboxed preview images with `object-fit: contain`
- [x] Live WebSocket updates with auto-reconnect
- [x] Version display with auto-reload on version change
- [x] Uptime display (⏱️) in header
- [x] Soft reboot banner when server recently restarted
- [x] **Manual capture form** - enter piece name, format (PNG/WebP/GIF), duration
- [x] **Back to prompt link** (← prompt)

### Capture Endpoints
- [x] `GET /grab/:format/:width/:height/:piece` - Direct capture URL
- [x] `POST /grab` - JSON body capture request
- [x] `POST /grab-ipfs` - Capture and upload to IPFS (for keeps)
- [x] `GET /icon/:size/:piece.png` - Square icon thumbnails
- [x] `GET /preview/:size/:piece.png` - Preview images

### Deduplication System
- [x] Capture keys: `{piece}_{width}x{height}_{format}_{animated}_{gitVersion}`
- [x] Git version tracking via `OVEN_VERSION` env var
- [x] Check existing captures before regenerating
- [x] Return cached CDN URL if capture exists

### Still Capture Improvements
- [x] **3 second settle time** for stills (vs 200ms for animations)
- [x] Wait for canvas content to render before capturing
- [x] Support for both 2D and WebGL canvases

### Metadata & Tracking
- [x] Source tracking: `source` field ('keep', 'manual', 'api')
- [x] Keep ID tracking: `keepId` field for Tezos NFT token IDs
- [x] Git version stored with each capture
- [x] Dimensions stored with captures

### Links in Dashboard
- [x] 🎫 Keep #N - Link to objkt.com token (when from keep minting)
- [x] 📌 IPFS - Link to IPFS gateway
- [x] ☁️ CDN - Link to Spaces CDN URL
- [x] 🏷️ abc1234 - Git commit link
- [x] 🦋 AT - ATProto record link (for tapes)

### Queue Persistence
- [x] MongoDB storage for completed bakes/grabs
- [x] Restore pending tapes on server restart
- [x] Load recent bakes from database on startup
- [x] Change stream watching for new tapes

### Integration
- [x] `tezos/keeps.mjs` passes `source: 'keep'` to grab-ipfs
- [x] `prompt.mjs` has `oven` command to jump to dashboard

## Pending / Future Work 🔄

### Taxonomy Unification
- [ ] Merge `oven-cache`, `oven-grabs` into unified `oven-captures` collection
- [ ] Unified capture schema: icons, previews, grabs are all "captures" with different params
- [ ] Single endpoint for all capture types

### Keep Integration
- [ ] Pass `keepId` from keeps.mjs (need to query next token ID before mint)
- [ ] Track which captures became NFT thumbnails
- [ ] Link captures to on-chain tokens

### Performance
- [ ] Browser pool for parallel captures
- [ ] Pre-warm browser on startup
- [ ] Priority queue for keep captures vs manual

### Dashboard Enhancements
- [ ] Search/filter by piece name
- [ ] Bulk actions (delete, re-capture)
- [ ] Capture queue visualization
- [ ] Storage usage stats

## Endpoints Reference

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/` | GET | Dashboard HTML |
| `/status` | GET | JSON status with all bakes/grabs |
| `/health` | GET | Health check |
| `/grab/:format/:width/:height/:piece` | GET | Capture piece |
| `/grab` | POST | Capture via JSON body |
| `/grab-ipfs` | POST | Capture and upload to IPFS |
| `/icon/:size/:piece.png` | GET | Icon thumbnail |
| `/preview/:size/:piece.png` | GET | Preview image |
| `/bake` | POST | Start video bake |
| `/bake-complete` | POST | Mark bake complete |
| `/bake-status` | POST | Update bake status |
| `/keeps/latest` | GET | Latest keep thumbnail |
| `/ws` | WebSocket | Live updates |

## Environment Variables

- `OVEN_VERSION` - Git commit hash for cache invalidation
- `ART_SPACES_KEY` - DigitalOcean Spaces access key
- `ART_SPACES_SECRET` - DigitalOcean Spaces secret
- `MONGODB_URI` - MongoDB connection string
- `PORT` - Server port (default 3002)

## Deployment

```bash
# From devcontainer
cd /workspaces/aesthetic-computer
rsync -avz --delete --exclude='node_modules' --exclude='.git' \
  -e "ssh -i aesthetic-computer-vault/oven/ssh/oven-deploy-key" \
  oven/ root@137.184.237.166:/opt/oven/

ssh -i aesthetic-computer-vault/oven/ssh/oven-deploy-key root@137.184.237.166 \
  'systemctl restart oven'
```

## Related Files

- `oven/server.mjs` - Main Express server and dashboard
- `oven/grabber.mjs` - Puppeteer capture logic
- `oven/baker.mjs` - Video bake processing
- `tezos/keeps.mjs` - NFT minting (calls oven for thumbnails)
- `system/public/aesthetic.computer/disks/prompt.mjs` - Has `oven` command
