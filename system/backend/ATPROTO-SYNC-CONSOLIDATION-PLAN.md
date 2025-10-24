# ATProto Sync Consolidation Plan

## Problem

Currently, ATProto record creation logic is duplicated and inconsistent across:

1. **`painting-atproto.mjs`** - Specialized painting creation with blob uploads
2. **`sync-atproto.mjs`** - Simplified record creation for bulk sync
3. **Netlify functions** (`track-media.mjs`, `store-kidlisp.mjs`) - Should sync but don't always
4. **Future needs** - Moods, pieces, kidlisp all need proper lexicon implementation

### Issues:
- ❌ Duplicate logic in multiple places
- ❌ `sync-atproto.mjs` doesn't create proper painting records (missing blobs, imageUrl, code)
- ❌ Not using full lexicon schemas consistently
- ❌ Hard to maintain - changes need to be made in multiple places
- ❌ Some content types (moods) don't have ATProto creation modules yet

## Solution: Unified Media Sync Module

Create **`media-atproto.mjs`** - a single source of truth for all ATProto media operations.

### Module Structure

```javascript
// media-atproto.mjs
// Unified ATProto sync for all AC media types

export const MediaTypes = {
  PAINTING: 'painting',
  MOOD: 'mood',
  PIECE: 'piece',
  KIDLISP: 'kidlisp'
};

// Main API - works for all types
export async function createMediaRecord(database, type, item, options = {})
export async function deleteMediaRecord(database, type, userSub, rkey)
export async function updateMediaRecord(database, type, userSub, rkey, updates)

// Type-specific helpers (internal)
async function createPaintingRecord(...)  // handles blobs, thumbnails
async function createMoodRecord(...)      // simple text record
async function createPieceRecord(...)     // may need blob for source
async function createKidlispRecord(...)   // code + source
```

### Lexicon-Driven Design

Each media type knows its own schema:

```javascript
const LEXICONS = {
  [MediaTypes.PAINTING]: {
    collection: "computer.aesthetic.painting",
    requiredFields: ['slug', 'code', 'imageUrl', 'when', 'ref'],
    optionalFields: ['thumbnail', 'recordingUrl'],
    hasBlob: true,
    blobField: 'thumbnail',
    buildRecord: async (item, user, database) => {
      // Generate thumbnail blob
      // Build full painting record
      return { record, blob };
    }
  },
  
  [MediaTypes.MOOD]: {
    collection: "computer.aesthetic.mood",
    requiredFields: ['text', 'when', 'ref'],
    optionalFields: [],
    hasBlob: false,
    buildRecord: async (item, user, database) => {
      return {
        record: {
          $type: "computer.aesthetic.mood",
          text: item.text,
          when: item.when.toISOString(),
          ref: item._id.toString()
        }
      };
    }
  },
  
  // ... piece, kidlisp
};
```

### Usage in Different Contexts

#### 1. Netlify Functions (Real-time Sync)
```javascript
// In track-media.mjs after saving to MongoDB
import { createMediaRecord, MediaTypes } from '../../backend/media-atproto.mjs';

const paintingId = insertResult.insertedId;
const painting = await collection.findOne({ _id: paintingId });

// Single line to sync!
const result = await createMediaRecord(
  database, 
  MediaTypes.PAINTING, 
  painting,
  { userSub: user?.sub } // undefined for guest
);

if (result.rkey) {
  // Update MongoDB with rkey
  await collection.updateOne(
    { _id: paintingId },
    { $set: { 'atproto.rkey': result.rkey } }
  );
}
```

#### 2. Bulk Sync Script
```javascript
// In sync-atproto.mjs
import { createMediaRecord, MediaTypes } from './media-atproto.mjs';

for (const painting of missingPaintings) {
  const result = await createMediaRecord(
    database,
    MediaTypes.PAINTING,
    painting,
    { userSub: user._id }
  );
  // ... handle result
}
```

#### 3. User-facing tools (wipe/restore)
```javascript
// Already works! Uses the same unified API
await createMediaRecord(database, type, item, { userSub });
```

## Implementation Plan

### Phase 1: Create Core Module ✅
- [ ] Create `media-atproto.mjs` with unified API
- [ ] Move painting logic from `painting-atproto.mjs` into new module
- [ ] Add mood creation logic (currently missing)
- [ ] Add piece creation logic
- [ ] Add kidlisp creation logic
- [ ] All use proper lexicon schemas

### Phase 2: Update Sync Script
- [ ] Update `sync-atproto.mjs` to use `createMediaRecord`
- [ ] Remove simplified `buildRecord` functions
- [ ] Test wipe/restore with new module

### Phase 3: Update Netlify Functions
- [ ] Update `track-media.mjs` to call `createMediaRecord` after save
- [ ] Update `store-kidlisp.mjs` to call `createMediaRecord` after save
- [ ] Remove any partial ATProto logic from these functions

### Phase 4: Deprecate Old Files
- [ ] Mark `painting-atproto.mjs` as deprecated (or remove)
- [ ] Update all imports to use `media-atproto.mjs`
- [ ] Update documentation

## Benefits

1. ✅ **Single source of truth** - one place to maintain ATProto logic
2. ✅ **Consistent** - all media types use proper lexicons
3. ✅ **Reusable** - works in sync scripts, Netlify functions, admin tools
4. ✅ **Type-safe** - clear API with type constants
5. ✅ **Testable** - easier to write tests for unified module
6. ✅ **Extensible** - easy to add new media types

## Files Affected

### New
- `system/backend/media-atproto.mjs` (new unified module)

### Modified
- `system/backend/sync-atproto.mjs` (use new module)
- `system/netlify/functions/track-media.mjs` (call createMediaRecord)
- `system/netlify/functions/store-kidlisp.mjs` (call createMediaRecord)

### Deprecated/Removed
- `system/backend/painting-atproto.mjs` (logic moves to media-atproto.mjs)
- Any other type-specific modules

## Migration Strategy

1. Create new module with full lexicon support
2. Update sync script first (easier to test)
3. Update Netlify functions (test with real uploads)
4. Verify everything works
5. Remove old code

## Notes

- Must handle both authenticated and anonymous/guest content
- Blob generation (thumbnails) is expensive - consider caching
- Error handling should be consistent across all types
- Need to handle partial failures gracefully (MongoDB saved but ATProto failed)
