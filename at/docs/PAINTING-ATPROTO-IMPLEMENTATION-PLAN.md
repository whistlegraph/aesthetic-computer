# Painting ATProto Implementation Plan
**Date:** 2025-10-15  
**Status:** Planning → Implementation

## Overview
Migrate 3,903 paintings from MongoDB to ATProto with thumbnail blobs and external URLs to full images in Digital Ocean Spaces.

---

## Architecture Decision

### ✅ Chosen Approach: Thumbnails + External URLs
- **Thumbnail blob** in ATProto (~50KB, 512x512px)
- **Full image URL** pointing to DO Spaces
- **Metadata** stored in ATProto record
- **MongoDB reference** for bidirectional linking

### Why This Approach?
- Storage efficient: ~200MB vs 20-40GB for full images
- Fast sync/replication across ATProto network
- Leverages existing DO Spaces infrastructure
- Standard pattern used by Twitter/Bluesky/social platforms
- Scalable to 100,000+ paintings without PDS strain

---

## Lexicon Definition

### `computer.aesthetic.painting`

```typescript
{
  lexicon: 1,
  id: "computer.aesthetic.painting",
  defs: {
    main: {
      type: "record",
      description: "A digital painting created on aesthetic.computer",
      key: "tid",
      record: {
        type: "object",
        required: ["slug", "code", "thumbnail", "imageUrl", "when", "ref"],
        properties: {
          slug: {
            type: "string",
            description: "Timestamp slug (e.g., '2023.8.24.16.21.09.123')",
            maxLength: 64
          },
          code: {
            type: "string", 
            description: "Short code for lookups (e.g., 'a3b', '4Xz')",
            maxLength: 10
          },
          thumbnail: {
            type: "blob",
            accept: ["image/png", "image/jpeg"],
            maxSize: 102400, // 100KB max
            description: "512x512px thumbnail preview"
          },
          imageUrl: {
            type: "string",
            format: "uri",
            description: "URL to full resolution PNG in DO Spaces",
            maxLength: 512
          },
          recordingUrl: {
            type: "string",
            format: "uri",
            description: "URL to .zip recording file (if available)",
            maxLength: 512
          },
          when: {
            type: "string",
            format: "datetime",
            description: "Original creation timestamp (matches MongoDB)"
          },
          ref: {
            type: "string",
            "description": "MongoDB ObjectId reference for bidirectional linking",
            maxLength: 24
          }
        }
      }
    }
  }
}
```

**Key Features (Matches Mood Lexicon Pattern):**
- `slug` - Timestamp only (e.g., '2023.8.24.16.21.09.123')
- `code` - Short code for `#abc` lookups (already in MongoDB!)
- `thumbnail` - Blob for fast timeline display
- `imageUrl` - Link to full .png (https://aesthetic.computer/media/@user/painting/slug.png)
- `recordingUrl` - Link to .zip recording (https://aesthetic.computer/media/@user/painting/slug.zip) - **optional**
- `when` - Timestamp (matches MongoDB `when` field, like moods)
- `ref` - MongoDB `_id` for bidirectional linking (EXACTLY like moods)

---

## Implementation Phases

### Phase 1: Infrastructure Setup ✅ (Partially Done)

**Components:**
1. ✅ SSH access to PDS (`ac-at` command working)
2. ✅ ATProto account system (978 users migrated)
3. ✅ MongoDB connection utilities
4. ⏳ Define `computer.aesthetic.painting` lexicon on PDS
5. ⏳ Create shared thumbnail generation module

**Tasks:**
- [ ] Deploy lexicon to PDS at `at.aesthetic.computer`
- [ ] Create `/workspaces/aesthetic-computer/system/backend/thumbnail.mjs`
- [ ] Test thumbnail generation with Sharp (reuse pixel.js logic)

---

### Phase 2: Thumbnail Generation Module

**File:** `/system/backend/thumbnail.mjs`

```javascript
// Shared thumbnail generator for paintings
// Uses Sharp (like pixel.js) to create 512x512 thumbnails

import sharp from "sharp";

export async function generateThumbnail(imageUrl, options = {}) {
  const size = options.size || 512;
  const format = options.format || "png";
  
  // Fetch image from DO Spaces
  const { got } = await import("got");
  const response = await got(imageUrl, {
    responseType: "buffer",
    https: { rejectUnauthorized: process.env.CONTEXT !== "dev" }
  });
  
  // Generate thumbnail with Sharp
  const thumbnail = await sharp(response.body)
    .resize({
      width: size,
      height: size,
      fit: "cover", // Crop to square
      kernel: sharp.kernel.lanczos3 // High quality downscale
    })
    .png() // or .jpeg({ quality: 90 })
    .toBuffer();
    
  return thumbnail;
}

export async function getThumbnailFromSlug(slug) {
  const imageUrl = `https://aesthetic.computer/media/${slug}`;
  return generateThumbnail(imageUrl);
}
```

**Benefits:**
- Reuses Sharp infrastructure (already in package.json)
- Works for both backfill and live uploads
- 512x512 "cover" mode ensures consistent aspect ratio
- ~30-50KB per thumbnail (PNG compression)

---

### Phase 3: Create Anonymous Account

**Purpose:** Host all 1,064 anonymous paintings (no user attribution)

**Handle Options:**
- `anon.aesthetic.computer` (preferred)
- `guest.aesthetic.computer`
- `public.aesthetic.computer`

**Creation Script:** `/system/backend/create-anon-account.mjs`

```javascript
#!/usr/bin/env node
// Create anonymous/guest account for paintings without user attribution

import { AtpAgent } from "@atproto/api";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";
const PDS_ADMIN_PASSWORD = process.env.PDS_ADMIN_PASSWORD;

async function createAnonAccount() {
  const agent = new AtpAgent({ service: PDS_URL });
  
  // Login as admin
  await agent.com.atproto.server.createSession({
    identifier: "admin",
    password: PDS_ADMIN_PASSWORD
  });
  
  // Create account
  const handle = "anon.aesthetic.computer";
  const email = "anon@aesthetic.computer";
  const password = generateSecurePassword(); // Store in vault!
  
  const account = await agent.com.atproto.server.createAccount({
    handle,
    email,
    password
  });
  
  console.log(`✅ Created anonymous account: ${handle}`);
  console.log(`   DID: ${account.data.did}`);
  console.log(`   Password: ${password} (SAVE THIS IN VAULT!)`);
  
  return account.data.did;
}
```

---

### Phase 4: Backfill Script (Existing Paintings)

**File:** `/system/backend/backfill-paintings.mjs`

**Logic:**
```javascript
1. Query MongoDB for paintings
   - Start with anonymous (1,064) for testing
   - Then user paintings (2,839) for 978 users with ATProto
   
2. For each painting:
   a. Check if already migrated (skip if atproto.rkey exists)
   b. Generate thumbnail from DO Spaces URL
   c. Upload thumbnail as blob to ATProto
   d. Create painting record with lexicon
   e. Update MongoDB with atproto.rkey reference
   
3. Error handling:
   - Skip if thumbnail generation fails
   - Retry logic for network issues
   - Log all failures for manual review
   - Non-destructive (doesn't delete MongoDB records)
```

**Features:**
- Dry-run mode (`--dry-run`)
- Limit flag (`--limit 10` for testing)
- User filter (`--user @username`)
- Anonymous flag (`--anonymous` for guest paintings)
- Progress tracking (X/Y paintings migrated)

---

### Phase 5: Live Upload Integration

**File:** `/system/netlify/functions/track-media.mjs`

**Add after MongoDB insert:**
```javascript
// After successful insertOne()...
if (user?.atproto?.did) {
  try {
    // Generate thumbnail
    const thumbnail = await getThumbnailFromSlug(slug);
    
    // Upload to ATProto
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.resumeSession(user.atproto.session);
    
    // Upload blob
    const blobUpload = await agent.uploadBlob(thumbnail, {
      encoding: "image/png"
    });
    
    // Create painting record
    const paintingRecord = await agent.com.atproto.repo.createRecord({
      repo: user.atproto.did,
      collection: "computer.aesthetic.painting",
      record: {
        slug,
        code,
        thumbnail: blobUpload.data.blob,
        url: `https://aesthetic.computer/media/${slug}`,
        createdAt: new Date().toISOString(),
        ref: record._id.toString(),
        piece: body.piece || null,
        nuked: false
      }
    });
    
    // Update MongoDB with rkey
    await collection.updateOne(
      { _id: record._id },
      { $set: { "atproto.rkey": paintingRecord.data.uri.split("/").pop() } }
    );
    
    console.log(`✅ Synced painting to ATProto: ${paintingRecord.data.uri}`);
  } catch (error) {
    // Non-blocking: painting still works even if ATProto fails
    console.error(`⚠️  ATProto sync failed (non-critical):`, error);
  }
}
```

**Benefits:**
- Automatic sync for new paintings
- Non-blocking (doesn't break uploads if ATProto is down)
- Reuses same thumbnail generation
- Immediate availability in ATProto feeds

---

## Data Statistics

### MongoDB Paintings Collection
- **Total:** 3,903 paintings
- **User paintings:** 2,839 (with user attribution)
- **Anonymous paintings:** 1,064 (no user)
- **Top user:** @ac25namuc with 384 paintings

### Storage Estimates
- **Thumbnails:** ~200MB (50KB × 4,000)
- **PDS capacity:** 19GB free (plenty of room)
- **Full images:** Stay in DO Spaces (20-40GB)

### Migration Priority
1. ✅ **Test:** 10 anonymous paintings first
2. ✅ **Anonymous batch:** All 1,064 guest paintings
3. ✅ **User paintings:** 2,839 for 978 ATProto users
4. ⏸️  **Skip:** 6 blocked users (no ATProto accounts)

---

## Testing Strategy

### 1. Lexicon Deployment Test
```bash
# SSH to PDS and verify lexicon
ac-at "cat /pds/lexicons/computer/aesthetic/painting.json"
```

### 2. Thumbnail Generation Test
```bash
# Test with a single painting
node system/backend/thumbnail.mjs --slug "@jeffrey/painting/2023.8.24.16.21.09.123"
# Should output: thumbnail-test.png (512x512, ~50KB)
```

### 3. Anonymous Account Test
```bash
# Create anonymous account
PDS_ADMIN_PASSWORD=xxx node system/backend/create-anon-account.mjs
# Verify: Should return DID for anon.aesthetic.computer
```

### 4. Backfill Dry Run
```bash
# Test 10 anonymous paintings (no actual upload)
node system/backend/backfill-paintings.mjs --anonymous --limit 10 --dry-run
# Should show: 10 paintings would be migrated, no changes made
```

### 5. Backfill Test Run
```bash
# Migrate 10 real anonymous paintings
node system/backend/backfill-paintings.mjs --anonymous --limit 10
# Verify in MongoDB: atproto.rkey field added
# Verify in ATProto: paintings visible at anon.aesthetic.computer
```

### 6. Full Anonymous Migration
```bash
# Migrate all 1,064 anonymous paintings
node system/backend/backfill-paintings.mjs --anonymous
# Monitor progress, check for errors
```

### 7. User Painting Test
```bash
# Test with a single user (your account)
node system/backend/backfill-paintings.mjs --user @jeffrey --limit 5
# Verify paintings appear in your ATProto feed
```

### 8. Full User Migration
```bash
# Migrate all 2,839 user paintings
node system/backend/backfill-paintings.mjs
# Will take ~30-60 minutes (thumbnail generation is the bottleneck)
```

---

## Success Criteria

### Phase 1: Infrastructure ✅
- [x] SSH access working (`ac-at` command)
- [ ] Lexicon deployed to PDS
- [ ] Thumbnail module created and tested

### Phase 2: Anonymous Paintings ✅
- [ ] Anonymous account created
- [ ] 10 test paintings migrated successfully
- [ ] All 1,064 anonymous paintings migrated
- [ ] Bidirectional MongoDB ↔ ATProto links verified

### Phase 3: User Paintings ✅
- [ ] 10 user paintings migrated (test)
- [ ] All 2,839 user paintings migrated
- [ ] Top users verified (ac25namuc with 384 paintings)
- [ ] No errors for 978 users with ATProto accounts

### Phase 4: Live Integration ✅
- [ ] track-media.mjs updated with ATProto sync
- [ ] New painting uploads automatically sync to ATProto
- [ ] Error handling tested (graceful degradation)

### Phase 5: Documentation ✅
- [ ] Migration status document (like ATPROTO-MIGRATION-STATUS.md)
- [ ] Comparison tool (like compare-mood-records.mjs)
- [ ] Edge cases documented

---

## Edge Cases & Considerations

### 1. Large Paintings
- Some paintings may be >10MB
- Thumbnail generation might timeout
- **Solution:** Set timeout, skip and log failures

### 2. Missing Images
- Some slugs might not resolve (deleted from DO)
- **Solution:** Skip paintings where URL returns 404

### 3. Anonymous Paintings
- Need special account (can't attribute to random user)
- **Solution:** Create `anon.aesthetic.computer` account

### 4. Duplicate Codes
- MongoDB code field should be unique
- **Solution:** Already handled in track-media.mjs with collision detection

### 5. Nuked Paintings
- Some paintings marked `nuked: true` in MongoDB
- **Solution:** Still migrate to ATProto with `nuked: true` flag (maintains parity)

### 6. ATProto Session Management
- Sessions expire after 2 hours
- **Solution:** Refresh session tokens during long backfill runs

### 7. Rate Limiting
- PDS might throttle blob uploads
- **Solution:** Add delays between uploads (100ms), respect 429 responses

---

## File Structure

```
/workspaces/aesthetic-computer/
├── system/
│   ├── backend/
│   │   ├── thumbnail.mjs                    # NEW: Shared thumbnail generator
│   │   ├── create-anon-account.mjs          # NEW: Create anonymous account
│   │   ├── backfill-paintings.mjs           # NEW: Migrate existing paintings
│   │   ├── compare-painting-records.mjs     # NEW: Comparison tool (like moods)
│   │   └── database.mjs                     # EXISTING: MongoDB connection
│   └── netlify/
│       └── functions/
│           ├── track-media.mjs              # MODIFY: Add ATProto sync
│           └── pixel.js                     # EXISTING: Sharp/resize reference
├── PAINTING-ATPROTO-IMPLEMENTATION-PLAN.md  # THIS FILE
└── PAINTING-MIGRATION-STATUS.md             # NEW: Post-migration report
```

---

## Next Steps (Ordered)

1. **Deploy Lexicon** → Define `computer.aesthetic.painting` on PDS
2. **Create Thumbnail Module** → Build `/system/backend/thumbnail.mjs`
3. **Test Thumbnail Generation** → Verify Sharp produces 512x512 PNGs
4. **Create Anonymous Account** → Set up `anon.aesthetic.computer`
5. **Build Backfill Script** → Create `/system/backend/backfill-paintings.mjs`
6. **Dry Run Test** → Test with 10 paintings (no actual upload)
7. **Test Migration** → Migrate 10 real anonymous paintings
8. **Full Anonymous Migration** → All 1,064 guest paintings
9. **User Migration Test** → 10 paintings from test user
10. **Full User Migration** → All 2,839 user paintings
11. **Live Integration** → Update track-media.mjs for auto-sync
12. **Create Comparison Tool** → Build compare-painting-records.mjs
13. **Document Results** → Create PAINTING-MIGRATION-STATUS.md

---

## Timeline Estimate

- **Phase 1 (Infrastructure):** 30 minutes
- **Phase 2 (Anonymous):** 1-2 hours (1,064 paintings)
- **Phase 3 (Users):** 2-3 hours (2,839 paintings)
- **Phase 4 (Live Integration):** 30 minutes
- **Phase 5 (Documentation):** 30 minutes

**Total:** ~5-7 hours including testing

---

## Ready to Start! 🚀

Let's begin with **Phase 1: Deploy the Lexicon**!
