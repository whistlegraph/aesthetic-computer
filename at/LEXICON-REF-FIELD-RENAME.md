# Lexicon Field Rename: mongoId → ref

**Date:** 2025-10-14  
**Status:** ✅ Complete - Code Updated, Backward Compatible

## Why the Change?

The field name `mongoId` exposed implementation details (MongoDB database). Better naming:
- **`ref`** - Generic reference to source database record
- Doesn't expose what database we use
- More professional and flexible

## What Was Changed

### 1. Lexicon Updated
**File:** `/at/lexicons/computer/aesthetic/mood.json`

```json
{
  "mongoId": {  // OLD
    "type": "string",
    "description": "Reference to MongoDB _id (ObjectId as string) for bidirectional sync"
  }
}
```

↓

```json
{
  "ref": {  // NEW
    "type": "string",
    "description": "Reference to source database record for bidirectional sync"
  }
}
```

### 2. Code Updated

**Files Modified:**
- `/system/backend/mood-atproto.mjs` - All functions now use `refId` parameter
- `/system/netlify/functions/mood.mjs` - Passes `refId` to ATProto sync
- `/at/scripts/check-atproto-moods.mjs` - Shows "Database Ref" instead of "MongoDB ID"
- `/at/scripts/check-atproto-duplicates.mjs` - Checks for duplicate `ref` values
- `/system/backend/sync-mongodb-from-atproto.mjs` - Syncs using `ref` field
- `/system/backend/delete-atproto-duplicates.mjs` - Identifies duplicates by `ref`

### 3. Backward Compatibility

**All scripts support BOTH old and new fields:**

```javascript
const ref = record.value.ref || record.value.mongoId;  // Fallback to old field
```

This means:
- ✅ Old @jeffrey moods with `mongoId` still work
- ✅ New moods use `ref`
- ✅ Scripts work with both
- ✅ Gradual migration possible

## Current State

### Existing Data (@jeffrey)
- 222 moods on ATProto with old `mongoId` field
- 1 test mood with new `ref` field
- All still functional due to fallback logic

### New Data
- All new moods created after this change use `ref`
- Clean, implementation-agnostic naming

## Migration Options

### Option 1: Leave As-Is (Recommended)
- Fallback logic handles both fields
- No breaking changes
- Old data still works
- New data uses better naming
- **Status:** ✅ This is what we're doing

### Option 2: Re-migrate All Data
- Would require deleting all ATProto moods
- Re-creating them with `ref` instead of `mongoId`
- More work, same end result
- **Status:** ❌ Unnecessary due to fallback support

## Code Examples

### Creating a Mood (New Code)
```javascript
await createMoodOnAtproto(
  database,
  user.sub,
  moodText,
  new Date(),
  refId  // ← Clean, generic name
);
```

### Reading ATProto Records (Backward Compatible)
```javascript
const ref = record.value.ref || record.value.mongoId;  // Works with both!
```

## Testing

Test the new `ref` field:
```bash
# Create a test mood (will use 'ref')
cd /workspaces/aesthetic-computer/system
node backend/test-create-mood.mjs jeffrey "Test with ref field"

# Check it (supports both ref and mongoId)
cd /workspaces/aesthetic-computer/at
node scripts/check-atproto-moods.mjs jeffrey
```

## Summary

✅ **Better naming** - `ref` instead of `mongoId`  
✅ **Backward compatible** - Old moods still work  
✅ **Future-proof** - Not tied to specific database  
✅ **All scripts updated** - Handle both fields  
✅ **No breaking changes** - Smooth transition  

---

**Result:** Professional naming without breaking existing data! 🎉
