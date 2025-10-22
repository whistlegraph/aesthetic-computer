# Tape MP4 Conversion Service

## Overview
External microservice that converts ZIP archives of frames into MP4 videos using FFmpeg.

## Architecture

### Input
POST request to `/convert` with JSON body:
```json
{
  "mongoId": "507f1f77bcf86cd799439011",
  "bucket": "user-aesthetic-computer",
  "slug": "wand-1729177200000",
  "zipUrl": "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/auth0|123/wand-1729177200000.zip",
  "callbackUrl": "https://aesthetic.computer/.netlify/functions/tape-mp4-complete",
  "metadata": {
    "frameCount": 120,
    "totalDuration": 4000,
    "piece": "wand",
    "scale": 6,
    "originalSize": [640, 480],
    "scaledSize": [3840, 2880]
  }
}
```

### Process
1. Download ZIP from `zipUrl`
2. Extract frames (PNG files)
3. Read `timing.json` to get frame durations
4. Generate FFmpeg concat file with frame timings
5. Run FFmpeg to create MP4:
   - Input: Individual frames with duration metadata
   - Output: H.264 encoded MP4, 30fps interpolated
   - Quality: CRF 23 (high quality)
   - Resolution: Use scaledSize from metadata (6x upscaled)
6. Upload MP4 to same bucket: `{bucket}/{userId}/{slug}.mp4`
7. POST callback to `callbackUrl`

### Output (Callback)
POST to callback URL with:
```json
{
  "mongoId": "507f1f77bcf86cd799439011",
  "mp4Url": "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/auth0|123/wand-1729177200000.mp4",
  "status": "complete",
  "duration": 4.0,
  "size": 2048576,
  "resolution": [3840, 2880]
}
```

Or on error:
```json
{
  "mongoId": "507f1f77bcf86cd799439011",
  "status": "error",
  "error": "FFmpeg encoding failed: ..."
}
```

## Implementation Options

### Option 1: Standalone Node.js Service (Recommended)
- Separate repository/deployment
- Express.js server with job queue (Bull/BullMQ)
- Redis for queue management
- FFmpeg binary in Docker container
- Deployed on DigitalOcean App Platform or Render.com
- Scales horizontally

**Pros**: 
- Isolated from main app
- Can handle long-running jobs
- Easy to scale and monitor
- No Netlify function timeout limits

**Cons**: 
- Additional infrastructure
- Separate deployment process

### Option 2: Netlify Background Function
- Uses Netlify's background function feature
- 15-minute timeout (should be enough for most tapes)
- FFmpeg via Lambda layer or bundled binary

**Pros**: 
- Same deployment pipeline
- No additional infrastructure

**Cons**: 
- 15-minute hard limit
- More complex FFmpeg setup on Lambda
- Limited scaling control

## FFmpeg Command Reference

```bash
# Create concat file from timing.json
# Format: file 'frame-0.png'
#         duration 0.033
#         file 'frame-1.png'
#         duration 0.050
# ...

ffmpeg \
  -f concat \
  -safe 0 \
  -i concat.txt \
  -vf "fps=30,scale=3840:2880:flags=neighbor" \
  -c:v libx264 \
  -preset medium \
  -crf 23 \
  -pix_fmt yuv420p \
  -movflags +faststart \
  output.mp4
```

## S3/DO Spaces Upload

Upload MP4 using presigned URL or AWS SDK:
```javascript
import { S3Client, PutObjectCommand } from "@aws-sdk/client-s3";

const s3 = new S3Client({
  endpoint: "https://sfo3.digitaloceanspaces.com",
  region: "us-east-1", // DO Spaces uses this region
  credentials: {
    accessKeyId: process.env.DO_SPACES_KEY,
    secretAccessKey: process.env.DO_SPACES_SECRET,
  },
});

await s3.send(new PutObjectCommand({
  Bucket: bucket,
  Key: `${userId}/${slug}.mp4`,
  Body: mp4Buffer,
  ContentType: "video/mp4",
  ACL: "public-read",
}));
```

## Environment Variables

Required for conversion service:
- `DO_SPACES_KEY` - DigitalOcean Spaces access key
- `DO_SPACES_SECRET` - DigitalOcean Spaces secret key
- `CALLBACK_AUTH_TOKEN` - Token to verify callback authenticity
- `REDIS_URL` - Redis connection string (for queue)

Required for main app:
- `MP4_SERVICE_URL` - URL of conversion service
- `MP4_SERVICE_TOKEN` - Authorization token for conversion service

## Retry Strategy

If conversion fails:
1. Service retries 3 times with exponential backoff
2. After 3 failures, sends error callback
3. Main app can manually trigger retry via admin endpoint
4. Cron job checks for tapes stuck in "processing" status

## Monitoring

Metrics to track:
- Conversion queue length
- Average conversion time
- Success/failure rate
- Storage usage

Alerts:
- Queue length > 100 (scaling needed)
- Failure rate > 10% (investigate)
- Tape stuck in "processing" > 1 hour (manual review)

## Future Enhancements

- [ ] Thumbnail generation (first frame as poster)
- [ ] Multiple quality/resolution outputs
- [ ] WebM/AV1 encoding for better compression
- [ ] Progress updates via websocket
- [ ] Batch processing for multiple tapes
- [ ] GPU-accelerated encoding (NVENC)
