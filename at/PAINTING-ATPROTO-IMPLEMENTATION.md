# Painting ATProto Auto-Sync Implementation

## Summary
Implemented automatic syncing of paintings to ATProto Personal Data Server (PDS) when users create new paintings on aesthetic.computer.

## Files Created

### 1. `/workspaces/aesthetic-computer/system/backend/painting-atproto.mjs`
New backend module for ATProto painting operations.

**Functions:**
- `createPaintingOnAtproto()` - Syncs a new painting to ATProto
  - Checks if user has ATProto credentials
  - Logs into PDS
  - Downloads painting image from S3/Spaces
  - Uploads image as blob to ATProto
  - Creates record in `computer.aesthetic.painting` collection
  - Returns rkey and uri

- `deletePaintingFromAtproto()` - Removes painting from ATProto
  - Used when user nukes a painting
  - Deletes the ATProto record by rkey

## Files Modified

### 2. `/workspaces/aesthetic-computer/system/netlify/functions/track-media.mjs`
Updated the painting tracking function to sync with ATProto.

**Changes:**
- Added imports for `createPaintingOnAtproto`, `deletePaintingFromAtproto`, and `getHandleOrEmail`
- After inserting painting into MongoDB:
  - Gets user's handle via `getHandleOrEmail()`
  - Constructs image URL
  - Calls `createPaintingOnAtproto()`
  - Stores `atproto.rkey` in MongoDB
- When nuking a painting:
  - Checks if painting has `atproto.rkey`
  - Calls `deletePaintingFromAtproto()` to remove from PDS

**Error Handling:**
- ATProto failures are logged but don't block painting creation
- Guest paintings skip ATProto sync (no user account)
- Users without ATProto accounts skip sync gracefully

### 3. `/workspaces/aesthetic-computer/system/netlify.toml`
Updated configuration for the `track-media` function.

**Changes:**
```toml
[functions.track-media]
external_node_modules = ["@atproto/api", "nanoid"]
included_env_vars = [
  "CONTEXT", 
  "ADMIN_SUB", 
  "AUTH0_M2M_CLIENT_ID", 
  "AUTH0_M2M_SECRET", 
  "MONGODB_CONNECTION_STRING", 
  "MONGODB_NAME", 
  "NETLIFY_DEV", 
  "PDS_URL",           # NEW
  "PDS_ADMIN_PASSWORD" # NEW
]
```

## How It Works

### Creating a Painting

1. User creates painting in aesthetic.computer
2. Frontend uploads PNG to S3/Digital Ocean Spaces
3. Frontend calls `POST /api/track-media` with `{ slug, ext: "png" }`
4. `track-media.mjs`:
   - Authorizes user (or allows guest)
   - Generates unique short code
   - Inserts into MongoDB `paintings` collection
   - **NEW:** Calls `createPaintingOnAtproto()`
     - Downloads painting image
     - Uploads as blob to ATProto
     - Creates `computer.aesthetic.painting` record
     - Returns rkey
   - Updates MongoDB with `atproto.rkey`
   - Returns `{ slug, code }` to client

### Nuking a Painting

1. User nukes painting
2. Frontend calls `PUT /api/track-media` with `{ slug, nuke: true }`
3. `track-media.mjs`:
   - Finds painting in MongoDB
   - Sets `nuked: true` in MongoDB
   - **NEW:** If `painting.atproto.rkey` exists:
     - Calls `deletePaintingFromAtproto()`
     - Removes record from ATProto PDS
   - Returns success

## ATProto Record Structure

```json
{
  "$type": "computer.aesthetic.painting",
  "slug": "2025.10.19.12.34.56.789",
  "code": "a3b",
  "imageUrl": "https://aesthetic.computer/media/@handle/painting/slug.png",
  "thumbnail": {
    "$type": "blob",
    "ref": { "$link": "bafyreiblahlahblah..." },
    "mimeType": "image/png",
    "size": 29692
  },
  "when": "2025-10-19T12:34:56.789Z",
  "ref": "67138a2f5e8c1a0012345678"
}
```

## MongoDB Changes

Paintings now have an optional `atproto` field:

```json
{
  "_id": ObjectId("..."),
  "code": "a3b",
  "slug": "2025.10.19.12.34.56.789",
  "when": ISODate("2025-10-19T12:34:56.789Z"),
  "bucket": "user-aesthetic-computer",
  "user": "auth0|1234567890",
  "atproto": {
    "rkey": "3m3irrx4aac2z"
  }
}
```

## Testing After Deployment

1. **Create a test painting** while logged in
2. **Check Netlify function logs** for:
   - ✅ `🔄 Syncing painting to ATProto...`
   - ✅ `📸 Uploaded painting thumbnail blob: ...`
   - ✅ `🎨 Created ATProto painting record: [rkey]`
   - ✅ `✅ Painting synced to ATProto with rkey: [rkey]`

3. **Check MongoDB** `paintings` collection for `atproto.rkey` field

4. **Query ATProto PDS** for `computer.aesthetic.painting` records

5. **Test nuking**:
   - Nuke a painting with an ATProto record
   - Check logs for `🗑️ Deleted ATProto painting record: [rkey]`
   - Verify record is gone from PDS

## Edge Cases Handled

- ✅ Guest paintings (no user) - skip ATProto sync
- ✅ Users without ATProto accounts - skip sync gracefully
- ✅ ATProto login failures - log error, continue
- ✅ Image download failures - log error, create record without thumbnail
- ✅ ATProto API failures - log error, painting still saved to MongoDB
- ✅ Nuking paintings without ATProto records - normal MongoDB update
- ✅ Pieces (`.mjs` files) - skip ATProto sync (only paintings)

## Compatibility

- **Existing paintings**: Will NOT be retroactively synced to ATProto
- **New paintings**: Will automatically sync if user has ATProto account
- **Manual sharing scripts**: Still work independently (`share-latest-painting.mjs`, etc.)
- **Guest paintings**: Continue to work exactly as before

## Dependencies

- `@atproto/api` - ATProto client library
- `nanoid` - Short code generation (already used)
- MongoDB connection
- User ATProto credentials stored in `users` collection

## Deployment Checklist

- [x] Create `painting-atproto.mjs` backend module
- [x] Update `track-media.mjs` function
- [x] Update `netlify.toml` configuration
- [ ] Deploy to Netlify
- [ ] Test painting creation
- [ ] Test painting deletion
- [ ] Monitor function logs
- [ ] Verify ATProto records in PDS

## Date
Implemented: 2025-10-19
