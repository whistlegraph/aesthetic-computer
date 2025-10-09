# Painting Short Code System Implementation Plan

**Date:** October 9, 2025  
**Goal:** Implement a global short identifier system for paintings similar to KidLisp's `$code` system, using `#code` format (hashtag prefix for bitmaps)

**Implementation:** Single comprehensive commit with all components

**Tools:** CLI suite in `/paintings` directory for inspection, migration, and orchestration

---

## CLI Tools & Inspection Suite

**Location:** `/paintings` directory

### Available Tools
- **`inspect-spaces.mjs`** - Browse Digital Ocean Spaces, list paintings, check sizes
- **`inspect-mongodb.mjs`** - Query MongoDB, check for codes, get statistics
- **`inspect-api.mjs`** - Test API endpoints (local or live server)

### Quick Start
```bash
cd paintings
./setup.fish  # Install dependencies and setup .env
npm run inspect:mongodb -- --stats
npm run inspect:spaces -- --list
npm run inspect:api -- --tv
```

### Environment Configuration
Tools support both local development and live production:
```bash
# Local development
AC_API=http://localhost:8888
MONGODB_URI=mongodb://localhost:27017/aesthetic

# Live production
AC_API=https://aesthetic.computer
MONGODB_URI=mongodb+srv://...
```

See [`/paintings/README.md`](/paintings/README.md) for full documentation.

---

## Current State Analysis

### KidLisp Code System (`$code`)
**Location:** `/system/public/aesthetic.computer/disks/store-kidlisp.mjs`

**What Works:**
- ✅ Short, memorable 3-12 character codes (e.g., `$beli`, `$waf`)
- ✅ Smart inference from source content (function names, meaningful combinations)
- ✅ Phonetically-balanced generation (vowel-consonant patterns)
- ✅ Collision detection via SHA-256 hash
- ✅ Automatic deduplication (same source = same code)
- ✅ MongoDB collection with indexes: `code` (unique), `hash` (unique), `when`, `user`
- ✅ nanoid-based generation with custom alphabets
- ✅ Progressive length growth (3→12 chars) if collisions occur
- ✅ Hit tracking and analytics

**Database Structure:**
```javascript
// kidlisp collection
{
  code: "beli",                    // 3-12 char unique identifier
  source: "(wipe 'blue')...",      // Original KidLisp source
  hash: "sha256hash",              // For deduplication
  user: "auth0|...",               // Optional user attribution
  when: Date,                      // Creation timestamp
  hits: 42,                        // Usage counter
  lastAccessed: Date               // Analytics
}
```

### Painting Upload/Creation Flow (Current)

**Step 1: Client-Side Upload Initiation**
- Location: `/system/public/aesthetic.computer/disks/prompt.mjs` (lines 987-1030)
- User paints, types "done" or "upload"
- System generates filename: `painting-${num.timestamp()}.png`
- Calls `upload()` function with PNG pixel data

**Step 2: Presigned URL Generation**
- Endpoint: `/system/netlify/functions/presigned-url.js`
- Client requests: `GET /presigned-upload-url/png/painting-{slug}/user`
- Server logic:
  - Checks user authentication via `authorize(event.headers)`
  - Uses nanoid (8 chars, alphabet: `0-9A-Za-z`) for collision prevention
  - S3 bucket selection:
    - `art` bucket: Guest uploads (temporary, expiring)
    - `user` bucket: Authenticated uploads (permanent, user/{auth0_id}/ prefix)
  - Returns presigned S3 URL valid for 1 hour (3600s)

**Step 3: S3 Upload**
- Client uploads PNG directly to Digital Ocean Spaces via presigned URL
- PUT request with `image/png` mime type, `public-read` ACL
- Storage path: `{user_id}/painting/{slug}.png`
- File becomes accessible at CDN URL

**Step 4: Database Record Creation**
- Endpoint: `/system/netlify/functions/track-media.js` (POST handler)
- Location: Lines 38-57
- Client sends: `{ slug, ext: "png" }`
- Server creates MongoDB record:
  ```javascript
  {
    slug: "2025.10.09.09.51.18.882",  // Timestamp from upload
    user: "auth0|...",                 // From JWT token
    when: new Date()                   // Server timestamp
  }
  ```
- Indexes created: `user`, `when`, `slug`, `slug+user` (unique)

**Step 5: Navigation**
- Client jumps to: `painting~@handle/{slug}` or `painting~{slug}`
- Painting viewer loads PNG from Digital Ocean CDN

### Existing Admin/Migration Tools

**Migration Script:** `/system/scripts/admin-migrate.mjs`
```bash
# Usage
node scripts/admin-migrate.mjs painting  # Migrate paintings
node scripts/admin-migrate.mjs piece     # Migrate pieces
```

**Migration Function:** `/system/backend/database.mjs` (`listAndSaveMedia()`)
- Scans Digital Ocean Spaces buckets for orphaned files
- Iterates through all `auth0|*/painting/*.png` files
- Creates missing database records with `{slug, user, when}`
- Logs: `✅ Added painting entry for: {slug}` or `⚠️ painting already exists`

**Prompt Command:** `admin:migrate-painting` or `admin:migrate-piece`
- Location: `/system/public/aesthetic.computer/disks/prompt.mjs` (lines 1312-1318)
- Calls: `/api/admin?migrate=painting`

### Painting System (Current Database)

**Collection:** `paintings` in MongoDB

**Existing Schema:**
```javascript
{
  slug: "2025.10.09.09.51.18.882",  // Timestamp-based (REQUIRED)
  user: "auth0|...",                 // Auth0 user ID
  when: Date,                        // Creation timestamp
  nuked: false                       // Soft delete flag (optional)
}
```

**Indexes:**
- `user` (ascending)
- `when` (ascending)
- `slug` (ascending)
- `slug + user` (unique composite)

**Known Issues:**
- ❌ NO short codes currently
- ⚠️ Some paintings may not be tracked in MongoDB (orphans in S3)
- ❌ No metadata (dimensions, colors, etc.)
- ❌ No deduplication mechanism
- ❌ No hit tracking/analytics

**Current URL Patterns:**
```
Long:  https://aesthetic.computer/painting~@handle/2025.10.09.09.51.18.882
Short: (doesn't exist yet)
Goal:  https://aesthetic.computer/#waf or aesthetic.computer/painting~#waf
```

---

## Proposed Solution

### 1. **Hashtag Prefix Convention**
- **Format:** `#waf`, `#lor`, `#pix` (3-4 characters preferred)
- **Prefix:** `#` for bitmaps (paintings), `$` for code (KidLisp)
- **Benefits:**
  - Clear semantic distinction (# = bitmap, $ = code)
  - Short, memorable, typeable
  - Works in URLs (# gets encoded as %23 or used as fragment)
  - Social media friendly

### 2. **Database Schema Enhancement**

Add to existing `paintings` collection:
```javascript
{
  // Existing fields
  slug: "2025.10.09.09.51.18.882",
  user: "auth0|...",
  when: Date,
  nuked: false,
  
  // NEW fields
  code: "waf",              // Short unique identifier (3-12 chars)
  hash: "sha256...",        // Hash of pixel data for deduplication
  hits: 0,                  // Usage tracking
  lastAccessed: Date,       // Analytics
  metadata: {               // Optional enrichment
    width: 195,
    height: 372,
    colors: 8,              // Palette size
    tags: ["pixel-art"],    // Auto-generated or manual
    title: "Sunset Scene"   // Optional user title
  }
}
```

**New Indexes Needed:**
```javascript
- { code: 1 } - unique, for #waf lookups
- { hash: 1 } - unique, for deduplication
- { user: 1, code: 1 } - for user galleries by code
```

### 3. **Code Generation Strategy**

**Option A: Smart Inference (Like KidLisp)**
- Extract visual features from painting
- Generate codes based on:
  - Dominant colors ("red" → `#rad`, `#rox`)
  - Dimensions ("16x16" → `#pix`, `#dot`)
  - User handle initials (@fifi → `#fif`, `#fie`)
  - Random pronounceable patterns

**Option B: Pure Random (Simpler)**
- Use nanoid with vowel-consonant balanced alphabet
- Start at 3 chars, grow to 4, 5 if collisions
- Alphabet: `abcdefghijklmnopqrstuvwxyz0123456789`
- Prefer CVC patterns (consonant-vowel-consonant)

**Option C: Hybrid (Recommended)**
- Try smart inference first (5-10 attempts)
- Fall back to random generation
- Ensure pronounceability with vowel injection

---

## Implementation Components

**Approach:** Single-commit implementation with all components integrated into existing upload flow

### Component 1: Code Generator Module
**File:** `system/backend/painting-code-generator.mjs`

**Purpose:** Reusable module for generating short codes for paintings

**Implementation:**
- Standalone pure function: `generatePaintingCode(imageBuffer, user, existingCodes)`
- Smart inference: Extract visual features (colors, dimensions, user handle)
- Fallback: Random pronounceable codes using nanoid with CVC patterns
- Collision detection: Check MongoDB before returning
- Hash generation: SHA-256 of pixel data for deduplication
- Progressive length: Start at 3 chars, grow to 4, 5, etc. if collisions

**Dependencies:**
- nanoid (for random generation)
- sharp (for image analysis)
- crypto (for SHA-256 hashing)

**Testing:**
- Unit tests with sample images
- Collision rate testing (should be < 0.01%)
- Pronounceability validation

---

### Component 2: Upload Flow Integration
**File:** `system/netlify/functions/track-media.js` (POST handler)

**Current Flow:**
1. Client uploads PNG to S3 via presigned URL
2. Client POSTs `{ slug, ext }` to track-media
3. Server creates MongoDB record: `{ slug, user, when }`

**Enhanced Flow:**
1. Client uploads PNG to S3 via presigned URL
2. **[NEW]** Server downloads PNG from S3 (or receives hash from client)
3. **[NEW]** Server generates code using `painting-code-generator.mjs`
4. **[NEW]** Server calculates SHA-256 hash of image data
5. Server creates MongoDB record: `{ slug, user, when, code, hash, hits: 0 }`
6. **[NEW]** Server returns `{ slug, code }` to client
7. **[NEW]** Client can navigate to `painting~#code` or `painting~@handle/slug`

**Implementation Details:**
- Import code generator module
- Add collision retry logic (max 10 attempts)
- Handle hash-based deduplication (same image = reuse code)
- Update indexes: add `code` (unique), `hash` (unique)
- Maintain backward compatibility (slug-only still works)

---

### Component 3: Migration Script
**File:** `system/scripts/migrate-paintings-add-codes.mjs`

**Purpose:** Add codes and hashes to existing paintings

**Implementation:**
```bash
# Dry run (preview only)
node scripts/migrate-paintings-add-codes.mjs --dry-run

# Execute migration
node scripts/migrate-paintings-add-codes.mjs --execute

# Migrate specific user
node scripts/migrate-paintings-add-codes.mjs --user auth0|123 --execute
```

**Algorithm:**
1. Query MongoDB for paintings without `code` field
2. For each painting:
   - Download PNG from Digital Ocean CDN
   - Generate code using `painting-code-generator.mjs`
   - Calculate SHA-256 hash
   - Check for hash duplicates (deduplication)
   - Update document with `{ code, hash, hits: 0, lastAccessed: Date }`
3. Log progress: `✅ slug → #code` or `⚠️ Duplicate hash, reusing #code`
4. Summary: Total migrated, duplicates found, failures

**Error Handling:**
- Skip paintings that fail to download (404, network error)
- Retry up to 3 times on collision
- Log failures to separate file for manual review
- Rate limit: 100ms delay between requests to avoid S3 throttling

**Reuse Admin Infrastructure:**
- Extend existing `admin-migrate.mjs` with `--add-codes` flag
- Use same S3 client and MongoDB connection
- Add to prompt commands: `admin:migrate-painting-codes`

---

### Component 4: Lookup API
**File:** `system/netlify/functions/painting-code-lookup.mjs`

**Endpoint:** `GET /api/painting-code/{code}`

**Input:** 
- `code` parameter: `waf` or `#waf` (strip # if present)

**Output:**
```json
{
  "code": "waf",
  "slug": "2025.10.09.09.51.18.882",
  "user": "auth0|...",
  "handle": "fifi",
  "when": "2025-10-09T09:51:18.882Z",
  "url": "https://aesthetic.computer/painting~@fifi/2025.10.09.09.51.18.882",
  "shortUrl": "https://aesthetic.computer/#waf",
  "cdnUrl": "https://aesthetic.computer/media/@fifi/painting/2025.10.09.09.51.18.882.png"
}
```

**Features:**
- Strip `#` prefix if present
- Case-insensitive lookup (convert to lowercase)
- Increment `hits` counter
- Update `lastAccessed` timestamp
- Return 404 if code not found
- Include user handle via join/lookup

**Authorization:** Public endpoint (no auth required)

---

### Component 5: URL Routing
**File:** `system/public/aesthetic.computer/disks/prompt.mjs`

**Current Routes:**
- `painting~@handle/slug` → User's specific painting
- `painting~slug` → Painting by current user

**New Routes:**
- `painting~#waf` → Painting by code (any user)
- `#waf` → Direct shortcut to painting by code

**Implementation:**
1. Update URL parser to detect `#code` pattern
2. Call `/api/painting-code/waf` to resolve code → slug/user
3. Load painting with resolved slug
4. Display code prominently in UI
5. Generate QR code for `aesthetic.computer/#waf`

**Fragment Routing:**
- Handle `/#waf` at top level
- Check if fragment matches `#[a-z0-9]{3,12}` pattern
- If match, treat as painting code and route to `painting~#waf`
- Otherwise, handle as normal prompt command

---

### Component 6: TV API Enhancement
**File:** `system/netlify/functions/tv.mjs`

**Current Response:**
```json
{
  "media": {
    "paintings": [
      { "slug": "2025...", "user": "auth0|...", "owner": { "handle": "@fifi" }, ... }
    ]
  }
}
```

**Enhanced Response:**
```json
{
  "media": {
    "paintings": [
      { 
        "slug": "2025...", 
        "code": "waf",
        "shortUrl": "https://aesthetic.computer/#waf",
        "user": "auth0|...", 
        "owner": { "handle": "@fifi" }, 
        ...
      }
    ]
  }
}
```

**Implementation:**
- Add `code` field to projection
- Generate `shortUrl` in response
- Filter by code: `GET /api/tv?code=waf`
- Order by hits: `GET /api/tv?sort=popular`

---

### Component 7: Orphan Discovery
**File:** `system/scripts/audit-digital-ocean-paintings.mjs`

**Purpose:** Find paintings in S3 that aren't tracked in MongoDB

**Implementation:**
1. List all files in Digital Ocean Spaces: `auth0|*/painting/*.png`
2. Query MongoDB for all painting slugs
3. Compare lists to find orphans
4. Generate report: `orphaned-paintings.json`
5. Optionally auto-import with generated codes

**Integration:**
- Reuse `listAndSaveMedia()` from `database.mjs`
- Extend with code generation
- Add `--import` flag to create records for orphans

---

### Component 8: Frontend Display
**File:** `system/public/aesthetic.computer/disks/painting.mjs`

**Updates:**
1. **Display Code:** Show `#waf` prominently on painting page
2. **Copy Button:** Click to copy short URL to clipboard
3. **QR Code:** Generate QR for `aesthetic.computer/#waf`
4. **Share Menu:** Update to use short URL by default
5. **Legacy Support:** Still show timestamp slug as fallback

**UI Mockup:**
```
┌─────────────────────────┐
│   🎨 Painting #waf      │
│   by @fifi              │
│                         │
│   [📋 Copy Link]        │
│   [🔗 QR Code]          │
│   [↗️  Share]           │
│                         │
│   aesthetic.computer/#waf
└─────────────────────────┘
```

---

## Technical Details

### Code Generation Algorithm

```javascript
async function generatePaintingCode(imageBuffer, user, existingCodes = new Set()) {
  const { customAlphabet } = await import('nanoid');
  const lowercaseAlphabet = 'abcdefghijklmnopqrstuvwxyz0123456789';
  
  // Try smart inference first
  const inferredCodes = await inferCodesFromImage(imageBuffer, user);
  
  for (const code of inferredCodes) {
    if (!existingCodes.has(code) && !await codeExists(code)) {
      return code;
    }
  }
  
  // Fall back to random CVC pattern generation
  const generator = customAlphabet(lowercaseAlphabet, 3);
  let attempts = 0;
  let code;
  
  do {
    code = generator();
    // Ensure pronounceability (consonant-vowel-consonant pattern)
    if (!/[aeiou]/.test(code)) {
      code = injectVowel(code);
    }
    attempts++;
  } while (existingCodes.has(code) || await codeExists(code));
  
  return code;
}

async function inferCodesFromImage(imageBuffer, user) {
  const codes = [];
  const metadata = await sharp(imageBuffer).metadata();
  const { width, height } = metadata;
  
  // Size-based codes
  if (width <= 16 || height <= 16) codes.push('pix', 'dot', 'sml');
  if (width >= 1024 || height >= 1024) codes.push('big', 'hiq');
  
  // Aspect ratio codes
  if (Math.abs(width - height) < 10) codes.push('sqr');
  if (height > width * 1.5) codes.push('tal', 'ver');
  if (width > height * 1.5) codes.push('wid', 'hor');
  
  // User-based codes
  if (user && user.handle) {
    const handle = user.handle.replace('@', '');
    codes.push(
      handle.substring(0, 3),
      handle.charAt(0) + handle.charAt(1) + handle.charAt(handle.length - 1)
    );
  }
  
  // Make all codes pronounceable
  return codes.map(c => ensurePronounceable(c)).filter(c => c.length >= 3);
}
```

### Migration Script Structure

```javascript
// migrate-paintings-add-codes.mjs
import { connect } from '../backend/database.mjs';
import { generatePaintingCode } from '../backend/painting-code-generator.mjs';
import fetch from 'node-fetch';

async function migratePaintings(dryRun = true) {
  const { db, disconnect } = await connect();
  const paintings = db.collection('paintings');
  
  // Find paintings without codes
  const query = { code: { $exists: false }, nuked: { $ne: true } };
  const toMigrate = await paintings.find(query).toArray();
  
  console.log(`Found ${toMigrate.length} paintings to migrate`);
  
  const existingCodes = new Set(
    (await paintings.find({ code: { $exists: true } }).toArray())
      .map(p => p.code)
  );
  
  let migrated = 0;
  let failed = 0;
  
  for (const painting of toMigrate) {
    try {
      // Download painting
      const url = `https://aesthetic.computer/media/${painting.user}/painting/${painting.slug}.png`;
      const response = await fetch(url);
      const imageBuffer = Buffer.from(await response.arrayBuffer());
      
      // Generate code
      const code = await generatePaintingCode(imageBuffer, { user: painting.user }, existingCodes);
      const hash = crypto.createHash('sha256').update(imageBuffer).digest('hex');
      
      if (!dryRun) {
        await paintings.updateOne(
          { _id: painting._id },
          { $set: { code, hash, hits: 0, lastAccessed: new Date() } }
        );
      }
      
      console.log(`✅ ${painting.slug} → #${code}`);
      existingCodes.add(code);
      migrated++;
      
      // Rate limit
      await sleep(100);
    } catch (error) {
      console.error(`❌ Failed: ${painting.slug}`, error.message);
      failed++;
    }
  }
  
  console.log(`\n✨ Migration complete: ${migrated} migrated, ${failed} failed`);
  await disconnect();
}

// Run with: node migrate-paintings-add-codes.mjs --dry-run
// Or: node migrate-paintings-add-codes.mjs --execute
```

---

## URL Structure Comparison

### Current System
```
View:   aesthetic.computer/painting~@fifi/2025.10.09.09.51.18.882
Share:  (same - very long URL)
QR:     (impractical - too long)
```

### New System  
```
View:   aesthetic.computer/#waf  (fragment-based routing)
Or:     aesthetic.computer/painting~#waf
Legacy: aesthetic.computer/painting~@fifi/2025.10.09.09.51.18.882  (still works)
QR:     aesthetic.computer/#waf  (perfect for QR codes!)
Social: @aesthetic.computer just posted #waf 🎨
```

---

## Benefits

1. **Short URLs** - Perfect for QR codes, social media, typing
2. **Memorable** - `#waf` is easier to remember than a timestamp
3. **Consistent** - Matches KidLisp's `$code` pattern
4. **Semantic** - `#` clearly denotes bitmap/image content
5. **Deduplication** - Same painting = same code (via hash)
6. **Analytics** - Track painting popularity via hits
7. **Discovery** - Enable "random painting" via random code
8. **Social** - Shareable hashtags (#waf) that work as URLs

---

## Risks & Mitigation

### Risk 1: Code Collisions
**Mitigation:**
- Unique index on `code` field
- Progressive length growth (3→12 chars)
- 36^3 = 46,656 combinations at 3 chars
- 36^4 = 1,679,616 combinations at 4 chars
- Check for collisions before inserting

### Risk 2: Untracked Paintings in DO Spaces
**Mitigation:**
- Phase 3 audit script finds orphans
- Bulk import with generated codes
- Gradual migration (non-blocking)

### Risk 3: Hash Collisions
**Mitigation:**
- SHA-256 is cryptographically secure
- Paranoid verification: compare actual pixels on collision
- Fall back to separate code if true collision

### Risk 4: Existing URLs Breaking
**Mitigation:**
- Maintain backward compatibility
- Keep timestamp-based slugs working
- Add codes as alternative access method
- No existing URLs change

### Risk 5: Code Generation Bias
**Mitigation:**
- Test smart inference with diverse paintings
- Monitor code distribution
- Fall back to random if inference fails
- Allow manual code assignment for special cases

---

## Success Metrics

1. **Code Coverage:** 100% of paintings have unique codes within 2 weeks
2. **Collision Rate:** < 0.01% during generation
3. **Pronounceability:** > 80% of codes pass phonetic test
4. **Adoption:** Short URLs used in 50%+ of shares within 1 month
5. **Performance:** Code lookup < 50ms (with indexes)
6. **QR Usability:** 90%+ of QR codes successfully scan and resolve

---

## Next Steps

1. **Review this plan** with team
2. **Prioritize phases** based on urgency
3. **Prototype code generator** in `/at` directory first
4. **Test on subset** of paintings (100-1000)
5. **Full migration** once validated
6. **Update docs** and announce new feature

---

## Open Questions

1. Should codes be case-sensitive? (Recommend: no, lowercase only)
2. Allow users to request custom codes? (vanity codes like #jeffrey)
3. Reserve certain codes? (e.g., #test, #admin, #new)
4. Integrate with existing "nuke" functionality?
5. Should codes be transferable between paintings? (Recommend: no)
6. Create separate namespace for featured/curated? (e.g., #featured:waf)

---

## References

**Existing Systems:**
- KidLisp: `/system/netlify/functions/store-kidlisp.mjs`
- Paintings: `/system/netlify/functions/track-media.js`
- Database: `/system/backend/database.mjs`

**Similar Services:**
- Imgur: 7-char alphanumeric (e.g., `a1b2c3d`)
- TinyURL: Variable length short codes
- Bitly: Custom short links
- Instagram: 11-char base64 post IDs

---

**Status:** 📋 Planning - Ready for Review  
**Next Action:** Prototype code generator in `/at` directory
