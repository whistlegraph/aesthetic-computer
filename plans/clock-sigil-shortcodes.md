# Clock Sigil Shortcodes (`*`) Implementation Plan

**STATUS: ✅ IMPLEMENTED** - 2025.06.14

## Summary

The `*` sigil for clock.mjs melodies has been implemented. You can now:
- Type `*bako` in the prompt to load a cached melody
- Visit URLs like `prompt.ac/*bako` or `aesthetic.computer/*bako`
- Share melodies via QR codes that appear in the corner (same as `$` for KidLisp)

### Code Changes Made

1. **store-clock.mjs** (NEW) - API for storing/retrieving melodies
2. **index.mjs** - Added `*xxx` URL routing and `clock:xxx` redirects  
3. **parse.mjs** - Added `*` sigil detection to route to clock piece
4. **clock.mjs** - Added async boot, API fetch for `*code`, melody caching
5. **disk.mjs** - Extended QR corner display for clock pieces with `*` sigil

### Pronounceable Codes

Uses CVCV pattern (consonant-vowel-consonant-vowel) with:
- 70% pure vowels (aeiou)
- 30% single-syllable digits (1,2,3,4,5,6,8,9)

Examples: `bako`, `milu`, `de4a`, `to2i`

---

## Overview

Add a new `*` sigil for clock.mjs melodies, similar to existing sigils:
- `$` → KidLisp pieces (e.g., `$roz`, `$cow`)
- `#` → Paintings (e.g., `#waf`, `#abc`)
- `!` → Video/Tapes (e.g., `!xyz`, video piece prefix)

**New**: `*` → Clock melodies (e.g., `*bah`, `*cdefg`)

## Current Sigil Architecture

### KidLisp (`$`) - Reference Implementation

The `$` sigil is the most complete implementation and serves as our reference:

1. **URL Routing** ([index.mjs#L210-225](../system/netlify/functions/index.mjs#L210-225))
   - Handles `kidlisp:code` → redirects to `$code` format
   - Direct `/$xxx` paths load the KidLisp piece

2. **Storage API** ([store-kidlisp.mjs](../system/netlify/functions/store-kidlisp.mjs))
   - POST: Stores source code, generates unique short code (nanoid)
   - GET: Retrieves source by code
   - Supports authenticated (user-linked) and anonymous storage
   - Hash-based deduplication
   - MongoDB collection: `kidlisp`

3. **Parser Support** ([parse.mjs](../system/public/aesthetic.computer/lib/parse.mjs))
   - Detects KidLisp source patterns
   - Decodes URL-encoded source
   - Routes to appropriate piece

4. **Corner QR Code** ([disk.mjs](../system/public/aesthetic.computer/lib/disk.mjs))
   - Displays QR code linking to `prompt.ac/$code`
   - Tappable to toggle fullscreen QR
   - Auto-generated when piece is cached

5. **Authorship & Uniqueness**
   - Hash-based deduplication (identical code → same shortcode)
   - User ID stored when authenticated
   - Anonymous pieces allowed (no user field)

### Painting (`#`) - Simpler Implementation

- **Storage**: [painting-code.mjs](../system/netlify/functions/painting-code.mjs)
- **Collection**: `paintings` (links code → slug/user)
- **URL Pattern**: `/#abc` or `painting~#abc`

### Video/Tape (`!`)

- **`!`**: Video piece prefix (triggers video.mjs loading for tapes)

## Proposed Clock (`*`) Implementation

### 1. Storage API: `store-clock.mjs`

Create new Netlify function at `system/netlify/functions/store-clock.mjs`:

```javascript
// Store Clock Melody, 2026.01.02
// Caches clock melody strings and generates pronounceable short codes for QR codes

// Pronounceable code generation (CVCV pattern for singability)
// Examples: bako, milu, dena, tofi, raku
const CONSONANTS = 'bdfghjklmnprstvwz'.split(''); // 17 consonants (no hard-to-pronounce)
const VOWELS = 'aeiou'.split(''); // 5 vowels

function generatePronounceable(length = 4) {
  let code = '';
  for (let i = 0; i < length; i++) {
    const chars = i % 2 === 0 ? CONSONANTS : VOWELS;
    code += chars[Math.floor(Math.random() * chars.length)];
  }
  return code;
}

// Collection: 'clocks' in MongoDB
// Schema:
{
  code: string,        // Short unique code (e.g., "bah", "xyz")
  melody: string,      // The melody string (e.g., "cdefgab", "{square}ceg dfa")
  hash: string,        // SHA256 of melody for deduplication
  user: string,        // User ID (optional, for authorship)
  handle: string,      // User handle at time of save (optional)
  when: Date,          // Creation timestamp
  hits: number,        // View count
  kept: object         // NFT/Tezos integration (future)
}
```

**Endpoints**:
- `POST /api/store-clock` - Store melody, return code
- `GET /api/store-clock?code=xxx` - Retrieve melody by code
- `GET /api/store-clock?recent=true&limit=N` - List recent clocks

### 2. URL Routing Updates

**In [index.mjs](../system/netlify/functions/index.mjs)**:

Add handling for `*` prefix similar to `$`:

```javascript
// Handle clock:code URL pattern and convert to *code format
if (slug.startsWith("clock:") && slug.length > 6) {
  const code = slug.slice(6);
  const newSlug = `*${code}`;
  return respond(302, `Redirecting to /${newSlug}`, {
    "Content-Type": "text/html",
    Location: `/${newSlug}`,
  });
}

// Handle *xxx clock codes
if (slug.startsWith("*") && slug.length > 1) {
  const code = slug.slice(1);
  // Fetch clock melody from API
  const clockData = await fetch(`/api/store-clock?code=${code}`);
  if (clockData.melody) {
    // Redirect to clock piece with melody parameter
    return respond(302, `Redirecting to /clock~${clockData.melody}`, {
      Location: `/clock~${encodeURIComponent(clockData.melody)}`,
    });
  }
}
```

### 3. Parser Updates

**In [parse.mjs](../system/public/aesthetic.computer/lib/parse.mjs)**:

Add clock sigil detection:

```javascript
// Check for clock shortcode (*xxx)
if (tokens[0] && tokens[0].startsWith("*") && tokens[0].length > 1) {
  tokens.unshift("clock"); // Route to clock piece
}
```

### 4. Clock Piece Updates

**In [clock.mjs](../system/public/aesthetic.computer/disks/clock.mjs)**:

Add auto-caching and QR display support:

```javascript
// On boot, if melody provided:
// 1. Cache the melody via store-clock API
// 2. Store the returned code for QR display
// 3. Display corner QR linking to prompt.ac/*code

let cachedCode = null;

async function cacheMelody(melody, api) {
  try {
    const response = await fetch('/api/store-clock', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ melody })
    });
    const data = await response.json();
    cachedCode = data.code;
    return data.code;
  } catch (e) {
    console.warn('Failed to cache clock melody:', e);
  }
}
```

### 5. HUD/QR Corner Display

**In [disk.mjs](../system/public/aesthetic.computer/lib/disk.mjs)**:

Extend QR corner logic to support clock pieces:

```javascript
// Detect clock piece with cached code
const isClockPiece = currentPath === "clock" || currentPath?.startsWith("clock~");
const isClockWithCode = isClockPiece && cachedClockCode;

// Generate QR for clock pieces
if (isClockWithCode) {
  const url = `https://prompt.ac/*${cachedClockCode}`;
  // Use same QR rendering as KidLisp pieces
}
```

### 6. Prompt Piece Integration

**In [prompt.mjs](../system/public/aesthetic.computer/disks/prompt.mjs)**:

Add `*` handling in halt function:

```javascript
// Handle *xxx clock shortcode
if (text.startsWith("*") && text.length > 1) {
  const code = text.slice(1);
  // Fetch and jump to clock melody
  const response = await fetch(`/api/store-clock?code=${code}`);
  if (response.melody) {
    jump(`clock~${response.melody}`);
    return true;
  }
}
```

## Database Schema

### Collection: `clocks`

```javascript
{
  _id: ObjectId,
  code: String,          // Unique short code (indexed, unique)
  melody: String,        // Full melody string
  hash: String,          // SHA256 hash (indexed, unique for dedup)
  user: String,          // Auth0 user ID (optional, sparse index)
  handle: String,        // User handle at save time
  when: Date,            // Creation timestamp (indexed)
  hits: Number,          // View count
  kept: {                // NFT integration (future)
    network: String,
    contract: String,
    tokenId: String
  }
}
```

### Indexes

```javascript
await collection.createIndex({ code: 1 }, { unique: true });
await collection.createIndex({ hash: 1 }, { unique: true });
await collection.createIndex({ user: 1 }, { sparse: true });
await collection.createIndex({ when: -1 });
await collection.createIndex({ hits: -1 });
```

## Implementation Order

### Phase 1: Core Storage
1. [ ] Create `store-clock.mjs` function
2. [ ] Add MongoDB collection and indexes
3. [ ] Implement POST (store) endpoint
4. [ ] Implement GET (retrieve) endpoint
5. [ ] Add hash-based deduplication

### Phase 2: URL Routing
6. [ ] Update `index.mjs` for `*xxx` routing
7. [ ] Update `parse.mjs` for `*` sigil detection
8. [ ] Handle `clock:xxx` → `*xxx` redirects

### Phase 3: Clock Piece Integration
9. [ ] Add melody auto-caching to clock.mjs boot
10. [ ] Store cached code for QR display
11. [ ] Pass cached code to HUD system

### Phase 4: QR Display
12. [ ] Extend disk.mjs QR corner logic for clock pieces
13. [ ] Add clock detection alongside KidLisp detection
14. [ ] Test QR generation with `prompt.ac/*code` URLs

### Phase 5: Prompt Integration
15. [ ] Add `*` handling in prompt.mjs halt()
16. [ ] Test `*bah` command input
17. [ ] Test URL bar navigation to `/*bah`

### Phase 6: Polish
18. [ ] Add authorship display (corner label)
19. [ ] Add anonymous vs authenticated distinction
20. [ ] Add hit counting
21. [ ] Test edge cases (empty melody, special chars, etc.)

## Files to Modify

| File | Changes |
|------|---------|
| `system/netlify/functions/store-clock.mjs` | **NEW** - Clock storage API |
| `system/netlify/functions/index.mjs` | Add `*` routing, `clock:` redirect |
| `system/public/aesthetic.computer/lib/parse.mjs` | Add `*` sigil detection |
| `system/public/aesthetic.computer/lib/disk.mjs` | Extend QR corner for clock |
| `system/public/aesthetic.computer/disks/clock.mjs` | Add caching, QR support |
| `system/public/aesthetic.computer/disks/prompt.mjs` | Add `*` command handling |

## URL Patterns

| Pattern | Description |
|---------|-------------|
| `/*bah` | Direct URL to clock shortcode |
| `/clock:bah` | Legacy format, redirects to `/*bah` |
| `/clock~cdefgab` | Direct melody (no caching) |
| `prompt.ac/*bah` | QR code destination |

## Edge Cases

1. **Empty melody** - Reject with 400 error
2. **Very long melody** - Limit to 10,000 chars (like KidLisp)
3. **Special characters** - URL encode in redirects
4. **Duplicate melody** - Return existing code (hash match)
5. **Invalid code** - Return 404 with helpful message
6. **Rate limiting** - Consider for POST endpoint

## Testing Checklist

- [ ] `*bah` in prompt navigates to clock piece
- [ ] `/*bah` URL loads correct melody
- [ ] QR code appears in corner during clock playback
- [ ] QR links to `prompt.ac/*code`
- [ ] Anonymous storage works (no auth)
- [ ] Authenticated storage links to user
- [ ] Duplicate melodies return same code
- [ ] Hit counter increments on GET
- [ ] Long melodies are handled properly
- [ ] Special chars in melody work correctly

## Future Considerations

1. **Tezos "Keep" Integration** - Like KidLisp, allow clock melodies to be "kept" as NFTs
2. **Feed/Discovery** - Add clocks to the TV feed system
3. **Embed in KidLisp** - Allow `(*code)` syntax to play clocks within KidLisp
4. **Collaboration** - Multiple users contributing to same clock
5. **Versioning** - Track edits to saved clocks
6. **Disambiguation Page** - When accessing `/bako` (no sigil), show a disambiguation page listing all matching codes across sigil types:
   - 🎵 `*bako` - Clock melody
   - 🎨 `$bako` - KidLisp piece  
   - 🖼️ `#bako` - Painting
   - Auto-redirect if only one type exists
