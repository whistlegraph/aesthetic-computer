# Tape Video Processing Setup

## Overview
Tapes are converted from ZIP (frames + audio) to MP4 using Netlify Background Functions.

## Duration Limit
- **Maximum tape duration: 30 seconds**
- Client should check `metadata.totalDuration` before allowing POST
- Server validates and returns error if exceeded

## Architecture

### Upload Flow
1. Client uploads ZIP to DigitalOcean Spaces
2. Client POSTs to `/api/track-tape` with metadata
3. Server validates duration (max 30s)
4. Server creates MongoDB record
5. Server invokes background function (fire-and-forget)
6. Client receives response immediately

### Background Processing
1. Background function runs (up to 15 minutes)
2. Downloads ZIP from Spaces
3. Converts to MP4 using ffmpeg
4. Uploads MP4 back to Spaces
5. Syncs to ATProto (if user has account)
6. Updates MongoDB with status

## Installation

```bash
cd system
npm install @ffmpeg-installer/ffmpeg
```

## Configuration

### netlify.toml
Background function is configured with:
- External modules: `@aws-sdk/client-s3`, `@atproto/api`, `adm-zip`, `@ffmpeg-installer/ffmpeg`
- Included backend files
- Required environment variables

### Environment Variables
- `ART_KEY` / `DO_SPACES_KEY` - DigitalOcean Spaces access key
- `ART_SECRET` / `DO_SPACES_SECRET` - DigitalOcean Spaces secret
- `MONGODB_CONNECTION_STRING` - MongoDB connection
- `PDS_URL` - ATProto PDS URL

## Client-Side Duration Check

Add to tape upload UI:

```javascript
// Before showing "Post to ATProto" button
const MAX_DURATION = 30; // seconds
const duration = tapeMetadata.totalDuration;

if (duration > MAX_DURATION) {
  // Hide Post button or show warning
  console.log(`Tape too long: ${duration}s (max ${MAX_DURATION}s)`);
  return;
}

// Show Post button
```

## Retry Failed Conversions

If background function fails, tapes can be reprocessed:

```bash
cd system/backend
node sync-atproto.mjs live --tapes-only --anonymous-only
```

## Monitoring

Check tape conversion status in MongoDB:
- `mp4Status`: "pending" | "processing" | "complete" | "failed"
- `mp4StartedAt`: When conversion started
- `mp4CompletedAt`: When conversion completed
- `mp4Error`: Error message if failed
- `mp4`: URL to MP4 file
- `atproto.rkey`: ATProto record key (if synced)
