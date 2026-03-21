# Attribution Strategy for Painting Short Codes

## Overview
This document outlines how we'll handle attribution for ALL paintings (guest and user) and how this aligns with the existing KidLisp code system.

## Current State

### KidLisp Codes (`$code`)
**Collection:** `kidlisp`  
**Attribution Model:** Optional user field

```javascript
{
  _id: ObjectId,
  code: "abc123",              // Short code ($abc123)
  source: "(wipe 'blue')...",  // Original KidLisp source
  hash: "sha256hash",          // For deduplication
  user: "auth0|...",           // OPTIONAL - can be null/undefined
  when: Date,                  // Creation timestamp
  hits: 42,                    // Usage counter
  lastAccessed: Date           // Analytics
}
```

**Key Insights from `store-kidlisp.mjs`:**
- User authorization is optional (wrapped in try/catch with 3s timeout)
- Anonymous kidlisp codes work perfectly: `user` field is undefined
- Handles "anonymous cache" gracefully: logs `🔓 No user authorization (anonymous cache)`
- Deduplication via hash still works without user
- Codes are reusable by anyone via `$code` regardless of who created them

### Paintings (Current)
**Collection:** `paintings`  
**Attribution Model:** Currently user-only (guest paintings NOT in database)

```javascript
// USER PAINTINGS (existing):
{
  _id: ObjectId,
  slug: "1704147600000",       // Timestamp
  user: "auth0|...",           // Always present
  when: Date,
  nuked: false
  // NO CODE FIELD YET
}

// GUEST PAINTINGS (not in DB):
// - Stored in "art-aesthetic-computer" S3 bucket
// - Filename is random code: "Lw2OYs0H.png"
// - NO database record at all
// - Referenced as "anon" in URLs
```

## Proposed Strategy

### Unified Painting Model
**Goal:** Every painting gets a database record AND a short code, regardless of user attribution.

```javascript
{
  _id: ObjectId,
  code: "abc",                 // NEW: Short code for #abc lookups
  slug: "1704147600000",       // Original identifier
  user: "auth0|...",           // OPTIONAL - undefined for guest paintings
  when: Date,
  nuked: false,
  bucket: "art-aesthetic-computer" | "user-aesthetic-computer",
  type: "guest" | "user"       // Convenience field
}
```

### Attribution Rules

#### Rule 1: Optional User Field
- **Match KidLisp model:** `user` field is optional
- **Guest paintings:** `user` is `undefined` (not "anon" string, not null)
- **User paintings:** `user` is Auth0 subject ID

#### Rule 2: Every Painting Gets a Code
- **Guest paintings:** Generate code on upload (Component 2)
- **User paintings:** Generate code on upload (Component 2)
- **Existing paintings:** Backfill codes via migration (Component 3)

#### Rule 3: Handle Lookup
- **With user:** Join with `@handles` collection (already done in tv.mjs)
- **Without user:** Display as "anonymous" or "guest" in UI
- **Code lookup:** Works regardless of attribution

### Database Queries

#### TV Feed (Update Existing)
```javascript
// Already handles missing handles gracefully:
const handle = record.handle ? `@${record.handle}` : null;
const ownerSegment = handle ?? record.user;

// For guest paintings with no user:
const ownerSegment = handle ?? record.user ?? "anon";
```

#### Code Lookup (New)
```javascript
// Find painting by short code - works for both:
const painting = await paintings.findOne({ code: "abc" });

// Attribution logic:
if (painting.user) {
  // Look up handle
  const handleDoc = await handles.findOne({ _id: painting.user });
  const owner = handleDoc ? `@${handleDoc.handle}` : painting.user;
} else {
  // Anonymous/guest painting
  const owner = "anonymous";
}
```

## Migration Strategy

### Phase 1: Add Code Field (Component 3)
```javascript
// For existing USER paintings (with user field):
for (const painting of userPaintings) {
  const code = generateCode();
  await paintings.updateOne(
    { _id: painting._id },
    { $set: { 
      code,
      bucket: "user-aesthetic-computer",
      type: "user"
    }}
  );
}
```

### Phase 2: Import Guest Paintings
```javascript
// For existing GUEST paintings (S3 only, no DB record):
const s3Files = await listAllPaintings("art-aesthetic-computer");

for (const file of s3Files) {
  const code = generateCode();
  const slug = file.Key.replace('.png', ''); // Use S3 filename as slug
  
  await paintings.insertOne({
    code,
    slug,
    // user: undefined,  // Don't set user field at all
    when: file.LastModified,
    nuked: false,
    bucket: "art-aesthetic-computer",
    type: "guest"
  });
}
```

## URL Routing

### Current URLs
```
/@user/painting/timestamp.png        → User painting
/Lw2OYs0H.png                        → Guest painting (no DB record)
```

### New URLs (Adding, Not Replacing)
```
/#abc                                → Painting by code (guest OR user)
/$xyz                                → KidLisp code (guest OR user)
/@user/painting/timestamp.png        → Still works (legacy)
/anon/painting/Lw2OYs0H.png         → Guest painting (new, with DB)
```

### Route Handler Logic
```javascript
// In painting.mjs or new route:
if (params[0].startsWith('#')) {
  const code = params[0].slice(1);
  const painting = await fetch(`/api/painting-code?code=${code}`);
  
  if (painting.user) {
    // Load from user bucket with handle
    const url = `/@${painting.handle}/painting/${painting.slug}.png`;
  } else {
    // Load from guest bucket
    const url = `/media/anon/painting/${painting.slug}.png`;
  }
}
```

## API Endpoints

### Component 4: `/api/painting-code`
```javascript
// GET /api/painting-code?code=abc
{
  code: "abc",
  slug: "1704147600000" | "Lw2OYs0H",
  owner: {
    handle: "@alice",     // If user painting with handle
    userId: "auth0|..."   // If user painting without handle
    // OR both undefined for guest paintings
  },
  url: "/@alice/painting/1704147600000.png",
  when: "2025-01-01T12:00:00Z",
  type: "guest" | "user"
}
```

## UI Display Rules

### Attribution Display
```javascript
// In painting viewer/TV feed:
function getAttribution(painting) {
  if (painting.handle) {
    return `@${painting.handle}`;
  } else if (painting.user) {
    return `User ${painting.user.slice(0, 8)}...`;
  } else {
    return "anonymous";  // or "guest" or "🎨"
  }
}
```

### Short Code Display
```javascript
// Always show code for easy sharing:
function getShareCode(painting) {
  return `#${painting.code}`;  // Works for both guest and user
}
```

## Benefits

### 1. Consistency with KidLisp
- Same optional attribution model
- Same code generation approach
- Same lookup semantics (`$code` and `#code`)

### 2. Preserves Guest Privacy
- No user field required
- Still trackable for analytics
- Still shareable via codes

### 3. Unified Codebase
- Same painting collection for all
- Same API endpoints
- Same TV feed logic (minor updates)

### 4. Migration Path
- Backfill existing user paintings ✅
- Import existing guest paintings ✅
- New uploads get codes automatically ✅

## Next Steps

1. **Component 1:** Create code generator module
2. **Component 2:** Update track-media/upload to generate codes
3. **Component 3:** Run migration script for existing paintings
4. **Component 4:** Create painting-code lookup API
5. **Component 5:** Update routing to support `#code`
6. **Component 6:** Update TV API to return codes
7. **Component 7:** Update frontend to display/use codes
8. **Component 8:** Add orphan discovery tools

## Open Questions

### Q1: Should we track guest paintings differently?
**Answer:** No. Use the same collection with optional `user` field, just like KidLisp.

### Q2: What about guest painting slug format?
**Answer:** Use the original S3 filename (e.g., "Lw2OYs0H") as the slug. The new `code` field is the short code.

### Q3: Do we need the `type` field?
**Answer:** Optional convenience field. Can derive from `user === undefined` vs `user !== undefined`.

### Q4: How to handle collisions during migration?
**Answer:** Use the same approach as KidLisp - generate unique codes with collision detection (test-generator.mjs shows 0 collisions in 50K codes).

### Q5: Should anonymous users see their upload codes?
**Answer:** YES! Show the `#code` immediately after upload so they can save it. This is the only way they can find their painting again.
