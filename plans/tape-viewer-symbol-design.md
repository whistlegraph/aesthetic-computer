# Tape Viewer: Design Decisions & Integration

## Symbol Choice: % for Tapes

### Why `%` instead of `#`?

The `%` symbol was chosen as the visual signifier for tape codes because:

1. **Visual Similarity to Film Reels**
   - The `%` resembles a film reel or movie symbol
   - Two circular shapes connected by a line = classic film reel icon
   - Instantly recognizable as video/motion content

2. **Distinct from Paintings**
   - Paintings use `#` (hash/number sign)
   - Tapes use `%` (percent sign)
   - Clear visual distinction between static and animated content
   - Prevents confusion when sharing codes

3. **Semantic Meaning**
   - `#` suggests hashtag/static reference (paintings)
   - `%` suggests progression/percentage (video playback)
   - Aligns with the temporal nature of tape recordings

4. **URL Compatibility**
   - Both `#` and `%` work in URLs
   - `#abc` → fragment identifier (client-side routing)
   - `%abc` → also works with fragment routing
   - Can differentiate content types in analytics

### Symbol Usage Patterns

**Paintings:**
```
painting #abc          → View painting with code "abc"
https://aesthetic.computer/#abc
```

**Tapes:**
```
tape %abc             → View tape with code "abc"
https://aesthetic.computer/#%abc
```

Note: In URLs, we still use `#` for fragment routing, but include the `%` as part of the fragment value.

## Integration with Existing Systems

### Code Generation

Both paintings and tapes use the same code generation system:

**Algorithm:**
- Custom alphabet: consonants (3x weight) + vowels (2x weight) + numbers
- Default length: 3 characters
- Collision detection with retry
- Stored in MongoDB with unique index

**Example Codes:**
```
Paintings: #k3d, #abc, #xyz
Tapes:     %k3d, %abc, %xyz
```

The code itself is identical (e.g., "k3d"), but the prefix symbol indicates the content type.

### Database Schema

**Paintings Collection:**
```javascript
{
  _id: ObjectId,
  code: "k3d",              // 3-char short code
  slug: "1704147600000",    // Timestamp or nanoid
  user: ObjectId,           // Owner (undefined for guests)
  when: Date,
  bucket: "user-aesthetic-computer" | "art-aesthetic-computer",
  nuked: false
}
```

**Tapes Collection:**
```javascript
{
  _id: ObjectId,
  code: "abc",              // 3-char short code
  slug: "mlENmD8Q",         // S3 filename (nanoid)
  user: ObjectId,           // Owner (undefined for guests)
  when: Date,
  bucket: "user-aesthetic-computer" | "art-aesthetic-computer",
  mp4Status: "pending" | "processing" | "complete",
  mp4: "https://...",       // MP4 URL (when ready)
  nuked: false
}
```

### API Endpoints

**Paintings:**
```
GET /api/painting-code?code=k3d
GET /api/get-painting?code=k3d
```

**Tapes:**
```
GET /api/get-tape?code=abc
GET /api/get-tape-status?code=abc
```

### Storage Buckets

**User Content:**
```
user-aesthetic-computer.sfo3.digitaloceanspaces.com/
├── @handle/painting/slug.png      (paintings)
└── slug.zip                         (tapes)
```

**Guest Content:**
```
art-aesthetic-computer.sfo3.digitaloceanspaces.com/
├── slug.png                         (paintings)
└── slug.zip                         (tapes)
```

### Routing System

The `tape.mjs` piece handles multiple routing patterns:

**Hash-based:**
```
https://aesthetic.computer/#%abc    → loads tape %abc
https://aesthetic.computer/#abc     → loads painting #abc (fallback)
```

**Parameter-based:**
```
aesthetic.computer/tape %abc        → loads tape %abc
aesthetic.computer/painting #abc    → loads painting #abc
```

**Show mode:**
```
aesthetic.computer/tape %abc:show   → lightbox mode
aesthetic.computer/painting #abc:show → lightbox mode
```

## User Experience Flow

### Creating a Tape
```
User Input:  tape 3 notepat
↓
System:      Record 3 seconds of notepat piece
↓            Capture frames + audio
↓            Create ZIP with frames, timing, metadata, soundtrack
↓            Upload to S3 (user or guest bucket)
↓            Generate short code (e.g., "abc")
↓            Store in MongoDB with code
↓
Output:      📼 Tape code: #abc
             ✔️ Tape uploaded and posted: https://...
```

### Viewing a Tape
```
User Input:  tape %abc
↓
System:      Parse code "abc"
↓            Fetch metadata from MongoDB
↓            Download ZIP from S3
↓            Extract frames, timing, audio
↓            Load into tape viewer
↓
Output:      Playable tape with controls
```

### Sharing a Tape
```
User shares: "Check out my tape: %abc"
↓
Recipient:   tape %abc
             OR
             https://aesthetic.computer/#%abc
↓
System:      Loads and plays tape
```

## Content Type Differentiation

### Visual Indicators

**Paintings:**
- Static image display
- No playback controls
- Title: `#code • Aesthetic Computer`
- Download button → PNG file

**Tapes:**
- Animated frame sequence
- Play/pause controls
- Title: `%code • Aesthetic Computer`
- Download button → ZIP file

### Console Messages

**Paintings:**
```
🎨 painting.mjs boot
🎨 Loading painting by code from hash: #abc
```

**Tapes:**
```
📼 tape.mjs boot
📼 Loading tape by code from hash: %abc
```

### URL Structure

**Direct Links:**
```
Painting: https://aesthetic.computer/#k3d
Tape:     https://aesthetic.computer/#%abc
```

**Embedded:**
```
Painting: <iframe src="https://aesthetic.computer/painting #k3d:show">
Tape:     <iframe src="https://aesthetic.computer/tape %abc:show">
```

## Code Collision Prevention

Since both paintings and tapes use the same code alphabet and length, there's potential for code overlap. However:

**Current Solution:**
- Different MongoDB collections (paintings vs tapes)
- Same code can exist in both (e.g., painting #abc AND tape %abc)
- Symbol prefix (`#` vs `%`) provides context
- Routing determines which piece to load

**Example Scenario:**
```
painting #abc exists → static image
tape %abc exists     → video playback

Both are valid and coexist peacefully!
```

**Future Consideration:**
If code uniqueness across content types becomes important:
- Add namespace prefix to codes in DB
- Filter available codes when generating new ones
- Or increase code length to reduce collisions

## Cross-Piece Integration

### From Other Pieces

**Jump to tape:**
```javascript
// In any piece
jump("tape %abc");
```

**Jump to painting:**
```javascript
jump("painting #abc");
```

### From Prompts

**Direct commands:**
```
> tape %abc
> painting #abc
```

**URL navigation:**
```
> /#%abc
> /#abc
```

## Analytics & Tracking

### Differentiation in Logs

**Server-side:**
```javascript
if (collection === 'paintings') {
  console.log(`📊 Painting #${code} viewed`);
} else if (collection === 'tapes') {
  console.log(`📊 Tape %${code} viewed`);
}
```

**Client-side:**
```javascript
// In tape.mjs
console.log(`📼 Loading tape %${code}`);

// In painting.mjs
console.log(`🎨 Loading painting #${code}`);
```

### Usage Patterns

Track symbol usage to understand user preferences:
- How often users share `#codes` vs `%codes`
- Which symbol has better recall/memorability
- Error rates in typing symbols

## Accessibility

### Screen Readers

**Painting codes:**
```
#abc → "hashtag A B C" or "number A B C"
```

**Tape codes:**
```
%abc → "percent A B C"
```

Both are pronounceable and distinguishable for screen reader users.

### Keyboard Navigation

Both viewers support:
- Arrow keys for navigation
- Space for play/pause (tapes only)
- Enter for activating buttons
- Escape for closing (if in modal)

## Migration Path

If we want to change the symbol system later:

1. **Backwards compatibility:**
   - Keep `%` working forever
   - Add alternative symbol (e.g., `@tape:abc`)
   - Redirect old URLs to new format

2. **Database migration:**
   - Add `type` field to codes collection
   - Unified code lookup across types
   - Deprecate symbol-based routing

3. **URL structure evolution:**
   ```
   Current:  /#%abc
   Future:   /tape/abc  (cleaner URLs)
   Legacy:   /#%abc     (still works)
   ```

## Summary

The `%` symbol for tapes:
- ✅ Visually distinct from paintings (`#`)
- ✅ Semantically meaningful (film reel / progression)
- ✅ URL-compatible
- ✅ Easy to type and remember
- ✅ Screen reader friendly
- ✅ Integrates seamlessly with existing code system

This creates a cohesive content ecosystem where users can easily share both static (`#`) and animated (`%`) creations! 🎨📼
