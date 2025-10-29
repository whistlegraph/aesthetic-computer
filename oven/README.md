# Oven - Video Processing Service for Aesthetic Computer Tapes

## Overview
`oven.aesthetic.computer` is a dedicated DigitalOcean Droplet service for processing tape recordings into MP4 videos. This solves the Netlify Function 250MB size limit issue caused by bundling ffmpeg.

## Quick Deployment

### Deploy to Production

```bash
cd /workspaces/aesthetic-computer/oven
fish deploy.fish
```

This automated script will:
1. ✅ Create DigitalOcean droplet (2GB RAM, 2 vCPU, $18/month)
2. ✅ Install Node.js 20, ffmpeg, and Caddy
3. ✅ Deploy oven service code
4. ✅ Configure HTTPS with automatic certificates
5. ✅ Set up systemd service for auto-restart  
6. ✅ Configure DNS (oven.aesthetic.computer)

### Post-Deployment Steps

1. **Update Netlify Environment:**
   ```bash
   # Add to system/.env (for development)
   OVEN_URL=https://oven.aesthetic.computer
   
   # Add to Netlify dashboard (for production)
   # Site settings → Environment variables
   OVEN_URL=https://oven.aesthetic.computer
   ```

2. **Test the Service:**
   ```bash
   # Health check
   curl https://oven.aesthetic.computer/health
   # Expected: {"status":"healthy"}
   
   # View dashboard
   open https://oven.aesthetic.computer
   ```

3. **Monitor Logs:**
   ```bash
   ssh -i ~/.ssh/oven-deploy-key root@DROPLET_IP
   tail -f /var/log/oven/oven.log
   ```

## Architecture

### Current Problem
- Netlify Functions have a 250MB size limit
- `ffmpeg-static` (50MB) + `ffprobe-static` + dependencies exceed this limit
- Functions `track-media` and `tape-convert-background` fail to deploy

### Solution
External video processing service on a dedicated server:

```
User uploads tape
    ↓
aesthetic.computer (Netlify)
    ↓ POST job to oven
oven.aesthetic.computer (DigitalOcean Droplet)
    ↓ Download ZIP to /tmp/
    ↓ Process video with ffmpeg
    ↓ Upload MP4 + thumbnail to at-blobs-aesthetic-computer/tapes/
    ↓ POST callback with Spaces URLs
    ↓ Clean up /tmp/
aesthetic.computer receives callback
    ↓ Download MP4 from Spaces
    ↓ Upload blob to ATProto
    ↓ Update MongoDB
```

## Service Components

### 1. Oven Server (Express.js)
- **Location**: `/oven/server.mjs`
- **Port**: 3000 (behind Caddy reverse proxy on 443)
- **Endpoints**:
  - `POST /bake` - Accept tape conversion job
  - `GET /health` - Health check
  - `GET /status/:jobId` - Check job status (optional)

### 2. Processing Pipeline
Reuse existing code from `system/backend/tape-to-mp4.mjs`:
1. Download ZIP from `art-aesthetic-computer` to `/tmp/tape-${slug}/`
2. Extract frames and audio
3. Read timing.json
4. Generate thumbnail with Sharp
5. Probe audio duration with ffprobe
6. Convert to MP4 with ffmpeg
7. Upload MP4 to `at-blobs-aesthetic-computer/tapes/${slug}.mp4`
8. Upload thumbnail to `at-blobs-aesthetic-computer/tapes/${slug}-thumb.jpg`
9. POST callback with Spaces URLs
10. Clean up `/tmp/tape-${slug}/` directory

**Storage Strategy**:
- **Source ZIPs**: `art-aesthetic-computer/${slug}.zip` (already there)
- **Processed MP4s**: `at-blobs-aesthetic-computer/tapes/${slug}.mp4`
- **Thumbnails**: `at-blobs-aesthetic-computer/tapes/${slug}-thumb.jpg`
- **Temp processing**: `/tmp/tape-${slug}/` (deleted after upload)
- **Organization**: All ATProto-related blobs in dedicated Space

### 3. Netlify Integration
Update `system/netlify/functions/track-media.mjs`:
- Remove inline MP4 conversion
- Remove ffmpeg-static and ffprobe-static dependencies
- POST to `https://oven.aesthetic.computer/bake` with:
  ```json
  {
    "mongoId": "...",
    "slug": "...",
    "zipUrl": "https://...",
    "metadata": {...},
    "callbackUrl": "https://aesthetic.computer/api/tape-bake-complete",
    "callbackSecret": "shared-secret"
  }
  ```

### 4. Callback Webhook
New Netlify function: `system/netlify/functions/tape-bake-complete.mjs`:
- Receives JSON callback with Spaces URLs
- Verifies shared secret for authentication
- Downloads MP4 from `at-blobs-aesthetic-computer/tapes/${slug}.mp4`
- Uploads MP4 blob to ATProto PDS
- Updates MongoDB with completion status, rkey, and MP4 URL
- Returns 200 on success

**Callback Payload**:
```json
{
  "mongoId": "...",
  "slug": "...",
  "mp4Url": "https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com/tapes/${slug}.mp4",
  "thumbnailUrl": "https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com/tapes/${slug}-thumb.jpg",
  "secret": "shared-secret"
}
```

**Data Flow**:
- Oven uploads to permanent Spaces storage
- Webhook downloads from Spaces, uploads to ATProto
- MP4 remains in Spaces for backup/future use
- ATProto gets blob, MongoDB gets both URLs and rkey

## Deployment Strategy

### Following Existing Patterns
Model deployment after `/at` and `/grab`:

1. **Vault Configuration**: `/aesthetic-computer-vault/oven/`
   - `.env` - Environment variables
   - `deploy.env` - DigitalOcean deployment config

2. **Deployment Script**: `/oven/deploy.fish`
   - Loads vault credentials
   - Creates/updates DigitalOcean Droplet
   - Installs ffmpeg, Node.js, dependencies
   - Sets up systemd service
   - Configures Caddy for HTTPS

3. **DNS Configuration**: Automatic via Cloudflare API
   - Similar to `/grab/scripts/deploy-with-dns.fish`
   - Creates A record: `oven.aesthetic.computer` → Droplet IP
   - Proxied through Cloudflare for DDoS protection

### Infrastructure Requirements

#### DigitalOcean Droplet
- **Size**: Basic Droplet ($6-12/month)
  - 1-2 GB RAM (sufficient for 8-second tapes)
  - 1 vCPU
  - 25-50 GB SSD
- **Image**: Ubuntu 24.04 LTS
- **Region**: SFO3 (same as Spaces)
- **Firewall**: 
  - Allow 80, 443 (HTTP/HTTPS)
  - Allow 22 (SSH for deployment)

#### Software Stack
- **Node.js**: v20+ (via nvm)
- **ffmpeg**: Latest via apt
- **ffprobe**: Included with ffmpeg
- **Caddy**: Automatic HTTPS
- **PM2** or **systemd**: Process management

### Environment Variables (Vault)

#### `/aesthetic-computer-vault/oven/.env`
```bash
# DigitalOcean Spaces - Source ZIPs
ART_SPACES_KEY=...
ART_SPACES_SECRET=...
ART_SPACES_ENDPOINT=https://sfo3.digitaloceanspaces.com
ART_SPACES_BUCKET=art-aesthetic-computer

# DigitalOcean Spaces - Processed Videos
AT_BLOBS_SPACES_KEY=...
AT_BLOBS_SPACES_SECRET=...
AT_BLOBS_SPACES_ENDPOINT=https://sfo3.digitaloceanspaces.com
AT_BLOBS_SPACES_BUCKET=at-blobs-aesthetic-computer

# Optional: Custom CDN domain (requires manual Cloudflare CNAME setup)
# AT_BLOBS_CDN=at-blobs.aesthetic.computer
# CNAME: at-blobs.aesthetic.computer → at-blobs-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com
# Enable Cloudflare proxy for CDN caching and DDoS protection

# Webhook callback
CALLBACK_SECRET=... # Shared secret for webhook authentication

# Service config
PORT=3000
NODE_ENV=production
TEMP_DIR=/tmp  # Where to store working files during processing
```

#### `/aesthetic-computer-vault/oven/deploy.env`
```bash
# DigitalOcean API
DO_TOKEN=...

# Droplet configuration
DROPLET_NAME=oven-aesthetic-computer
DROPLET_SIZE=s-1vcpu-1gb
DROPLET_IMAGE=ubuntu-24-04-x64
DROPLET_REGION=sfo3

# Cloudflare DNS
CLOUDFLARE_EMAIL=...
CLOUDFLARE_API_TOKEN=...
CLOUDFLARE_ZONE_ID=a23b54e8877a833a1cf8db7765bce3ca
```

## File Structure

```
/oven/
├── README.md                 # This file (architecture & deployment docs)
├── package.json              # Dependencies
├── server.mjs                # Express server
├── baker.mjs                 # Tape processing logic (from tape-to-mp4.mjs)
├── deploy.fish               # Main deployment script
├── setup.sh                  # Server provisioning script
├── oven.service              # Systemd service file
├── Caddyfile                 # Caddy reverse proxy config
└── scripts/
    └── setup-droplet.sh      # Initial droplet setup

/system/netlify/functions/
├── track-media.mjs           # Updated: POST to oven instead of inline
└── tape-bake-complete.mjs    # New: Webhook callback handler

/aesthetic-computer-vault/oven/
├── .env                      # Service environment variables
└── deploy.env                # Deployment configuration

DigitalOcean Spaces:
├── art-aesthetic-computer/
│   └── ${slug}.zip           # Source tape recordings
└── at-blobs-aesthetic-computer/
    └── tapes/
        ├── ${slug}.mp4       # Processed videos
        └── ${slug}-thumb.jpg # Thumbnails
```

## Implementation Steps

### Phase 1: Oven Service Setup
1. ✅ Create `/oven` directory structure
2. Create `server.mjs` with Express endpoints
3. Extract processing logic to `baker.mjs`
4. Add health check and status endpoints
5. Test locally with Docker

### Phase 2: Deployment Automation
1. Create vault configuration files
2. Write `deploy.fish` following `/grab` pattern
3. Create `setup.sh` for server provisioning
4. Configure Caddy for HTTPS
5. Set up systemd service

### Phase 3: Netlify Integration
1. Create `tape-bake-complete.mjs` webhook
2. Update `track-media.mjs` to POST to oven
3. Remove ffmpeg-static and ffprobe-static deps
4. Add oven URL to environment variables
5. Test end-to-end flow

### Phase 4: Testing & Monitoring
1. Test anonymous tape uploads
2. Test authenticated tape uploads
3. Monitor oven server logs
4. Set up health check alerts
5. Document troubleshooting

## Security Considerations

### Authentication
- Webhook callback uses shared secret
- Validate callback signature
- Only accept jobs from known origins

### Rate Limiting
- Limit concurrent jobs (start with 2-3)
- Queue additional requests
- Reject jobs over size/duration limits

### Firewall
- Restrict SSH to known IPs
- Only expose 80/443 for web traffic
- Use Cloudflare proxy for DDoS protection

## Cost Estimation

### Monthly Costs
- **Droplet**: $6-12/month (Basic/Regular)
- **Bandwidth**: Included (1TB transfer)
- **Spaces traffic**: Minimal (already using Spaces)
- **DNS**: Free (existing Cloudflare account)

**Total**: ~$6-12/month

### Scaling Considerations
- Single droplet handles ~10-20 concurrent conversions
- Can add load balancer + multiple droplets if needed
- Current traffic: <100 tapes/day = easily handled

## Monitoring & Maintenance

### Health Checks
- `/health` endpoint for uptime monitoring
- Cloudflare health checks
- Alert on 5xx errors or timeouts

### Logs
- PM2/systemd logs for debugging
- Rotate logs daily
- Monitor disk space usage

### Updates
- Automatic security updates (unattended-upgrades)
- Manual ffmpeg updates as needed
- Node.js updates via nvm

## Rollback Plan

If oven service fails:
1. Revert Netlify functions to inline processing
2. Use smaller droplet sizes temporarily
3. Fall back to background function (if <250MB somehow)

## Future Enhancements

### Potential Additions
- Job queue with Redis (for high traffic)
- Multiple worker droplets with load balancer
- Separate thumbnail generation endpoint
- Progress callbacks (for UI feedback)
- Video preview generation
- Format conversion (WebM, different resolutions)

## References

### Similar Services in Repo
- `/at` - ATProto PDS server (DigitalOcean + Cloudflare DNS)
- `/grab` - Screenshot worker (Cloudflare Worker + DNS)
- `/session-server` - WebSocket server example

### External Documentation
- [DigitalOcean API](https://docs.digitalocean.com/reference/api/)
- [Cloudflare API](https://developers.cloudflare.com/api/)
- [ffmpeg Documentation](https://ffmpeg.org/documentation.html)
- [Express.js](https://expressjs.com/)

## Timeline Estimate

- **Phase 1**: 4-6 hours (service implementation)
- **Phase 2**: 2-3 hours (deployment automation)
- **Phase 3**: 2-3 hours (Netlify integration)
- **Phase 4**: 1-2 hours (testing)

**Total**: ~9-14 hours of development

## Success Criteria

✅ Anonymous tape uploads work in production
✅ Authenticated tape uploads work in production
✅ MP4 conversion completes in <30 seconds for 8-second tapes
✅ ATProto sync happens automatically after conversion
✅ Netlify functions deploy successfully (<250MB)
✅ Service has 99% uptime
✅ Automatic deployment via `deploy.fish`
✅ DNS automatically configured on deployment
