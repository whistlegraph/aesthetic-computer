# Painting Sharing Tools

Automated tools for sharing aesthetic.computer paintings to Bluesky with proper attribution and image embedding.

## Tools

### 1. `share-latest-painting.mjs`
Share a specific user's latest painting to Bluesky.

**Usage:**
```bash
# Preview the painting first (saves to preview-painting.png)
node share-latest-painting.mjs @jeffrey --preview

# Post to Bluesky with default message
node share-latest-painting.mjs @jeffrey

# Post with custom message
node share-latest-painting.mjs @jeffrey --message "Amazing artwork! 🎨"
```

**Features:**
- ✅ Fetches latest painting from user's gallery
- ✅ Downloads and uploads image as Bluesky blob
- ✅ Includes proper AC URL: `https://prompt.ac/@handle/painting/{slug}`
- ✅ Preview mode for testing
- ✅ Custom message support

### 2. `bulk-share-paintings.mjs`
Bulk share multiple paintings from the TV feed to Bluesky.

**Usage:**
```bash
# Post 10 paintings with default 2s delay
node bulk-share-paintings.mjs 10

# Post 20 paintings with 3s delay between each
node bulk-share-paintings.mjs 20 --delay 3000

# Post 5 paintings quickly (1s delay)
node bulk-share-paintings.mjs 5 --delay 1000
```

**Features:**
- ✅ Posts multiple paintings in one run
- ✅ Configurable delay between posts
- ✅ Full summary report (success/failure counts)
- ✅ Proper error handling
- ✅ Reusable login session

## Post Format

All posts follow this format:

```
New painting by @{handle} 🎨✨

https://prompt.ac/@{handle}/painting/{slug}

[embedded image]
```

## Example Output

```bash
$ node bulk-share-paintings.mjs 10

🚀 Bulk Share Paintings to Bluesky

   Count: 10
   Delay: 2000ms between posts

🔐 Logging in as @aesthetic.computer...
✅ Logged in successfully

📡 Fetching 20 paintings from TV feed...
✅ Found 20 paintings with handles

[1/10] 🎨 Posting painting by @fifi
    Slug: 2025.10.09.09.51.18.882
    URL: https://prompt.ac/@fifi/painting/2025.10.09.09.51.18.882
    ✅ Downloaded 29692 bytes
    ✅ Uploaded to Bluesky
    ✅ Posted! https://bsky.app/profile/aesthetic.computer/post/3m2rrdqdk722z
    ⏳ Waiting 2000ms...

...

✨ Summary

✅ Successful: 10
❌ Failed: 0
📊 Total: 10

🔗 Posted paintings:
   1. @fifi - https://bsky.app/profile/aesthetic.computer/post/3m2rrdqdk722z
   2. @mangoghoul - https://bsky.app/profile/aesthetic.computer/post/3m2rrdsz2bb2u
   ...
```

## Configuration

Uses credentials from `.env`:

```bash
BSKY_SERVICE=https://bsky.social
BSKY_IDENTIFIER=aesthetic.computer
BSKY_APP_PASSWORD=your-app-password
```

## API Integration

### Data Flow

1. **Fetch** → Query AC's TV API or media-collection API
2. **Download** → Fetch PNG from aesthetic.computer CDN
3. **Upload** → Upload as blob to Bluesky PDS
4. **Post** → Create post with embedded image and AC URL

### TV API
```
GET https://aesthetic.computer/api/tv?limit=500
```

Returns recent paintings with metadata:
- Owner handle and user ID
- Painting slug and timestamp
- Media URL

### Media Collection API
```
GET https://aesthetic.computer/media-collection?for={handle}/painting
```

Returns all paintings for a specific user.

## Test Results

**Date:** October 9, 2025  
**Test:** Bulk posted 10 paintings

**Results:**
- ✅ 10/10 successful
- ⏱️ ~20 seconds total (2s delay between)
- 🎨 Paintings from @fifi and @mangoghoul
- 📊 Total data uploaded: ~380KB

**Sample Posts:**
- https://bsky.app/profile/aesthetic.computer/post/3m2rrdqdk722z
- https://bsky.app/profile/aesthetic.computer/post/3m2rrdsz2bb2u
- https://bsky.app/profile/aesthetic.computer/post/3m2rrdwhwvh2l

## Future Enhancements

- [ ] Filter by date range
- [ ] Filter by specific users
- [ ] Add hashtags support
- [ ] Track posted paintings to avoid duplicates
- [ ] Support for piece sharing (not just paintings)
- [ ] Scheduled posting via cron
- [ ] Integration with AC's notification system
- [ ] Custom templates for post text
- [ ] Support for threads (multiple images per post)

## Notes

- Images are stored in Bluesky as blobs (permanent storage)
- Original paintings remain on AC's CDN
- Posts include clickable AC URLs for traffic back to platform
- Rate limiting: 2-3 second delay recommended between posts
- Bluesky has a 300 character limit for post text
- Image alt text is auto-generated with handle attribution
