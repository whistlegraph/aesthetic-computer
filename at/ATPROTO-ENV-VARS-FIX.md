# ATProto Environment Variables Fix

## Issue Found
The `mood` Netlify function was missing critical environment variables needed to sync moods to ATProto.

## Problem
In `/workspaces/aesthetic-computer/system/netlify.toml`, the `[functions.mood]` section had:
```toml
[functions.mood]
external_node_modules = ["got", "firebase-admin", "obscenity"]
included_env_vars = []  # ❌ EMPTY!
```

This meant the mood function couldn't access:
- `MONGODB_CONNECTION_STRING` / `MONGODB_NAME` - to connect to the database
- `PDS_URL` / `PDS_ADMIN_PASSWORD` - to sync moods to ATProto  
- `GCM_FIREBASE_CONFIG_URL` - for Firebase notifications
- `AUTH0_M2M_CLIENT_ID` / `AUTH0_M2M_SECRET` - for authorization

## Fix Applied
Updated the mood function configuration to include all necessary environment variables and the `@atproto/api` module:

```toml
[functions.mood]
external_node_modules = ["got", "firebase-admin", "obscenity", "@atproto/api"]
included_env_vars = ["CONTEXT", "ADMIN_SUB", "AUTH0_M2M_CLIENT_ID", "AUTH0_M2M_SECRET", "MONGODB_CONNECTION_STRING", "MONGODB_NAME", "REDIS_CONNECTION_STRING", "NETLIFY_DEV", "GCM_FIREBASE_CONFIG_URL", "PDS_URL", "PDS_ADMIN_PASSWORD"]
```

## Other Functions Checked ✅
All other functions that use ATProto already have the correct configuration:

1. **`delete-erase-and-forget-me`** ✅
   - Uses: `deleteAtprotoAccount` from `backend/at.mjs`
   - Has: `@atproto/api`, `PDS_URL`, `PDS_ADMIN_PASSWORD`

2. **`handle`** ✅
   - Uses: `updateAtprotoHandle` from `backend/at.mjs`
   - Has: `@atproto/api`, `PDS_URL`, `PDS_ADMIN_PASSWORD`

3. **`auth0-events`** ✅
   - Uses: `createAtprotoAccount` from `backend/at.mjs`
   - Has: `PDS_URL`, `PDS_ADMIN_PASSWORD` (doesn't need `@atproto/api` in external_node_modules because it's imported via backend file)

## Next Steps
**Deploy to Netlify** to apply the fix. After deployment:
- New moods should properly sync to ATProto
- Check Netlify function logs for:
  - ✅ "🔄 Syncing mood to ATProto..."
  - ✅ "✅ Mood synced to ATProto with rkey: XXX"
  - ⚠️  "⚠️  Failed to sync mood to ATProto: [error]" (should not appear anymore)

## Testing
Created test scripts:
- `test-mood-api.mjs` - Tests the `/api/mood/all` endpoint (working, returns 2152 moods)
- `test-mood-create.mjs` - Documents the mood creation flow and potential issues

## Paintings - NEW IMPLEMENTATION ✨

**Paintings NOW automatically sync to ATProto** when created!

### What Was Implemented:

1. **Created `/workspaces/aesthetic-computer/system/backend/painting-atproto.mjs`**
   - `createPaintingOnAtproto()` - Syncs new paintings to ATProto
   - `deletePaintingFromAtproto()` - Removes paintings from ATProto when nuked
   - Downloads painting image and uploads as blob to ATProto
   - Creates record in `computer.aesthetic.painting` collection

2. **Updated `/workspaces/aesthetic-computer/system/netlify/functions/track-media.mjs`**
   - Imports painting-atproto functions
   - After creating painting in MongoDB, calls `createPaintingOnAtproto()`
   - Stores ATProto `rkey` in MongoDB record
   - When nuking a painting, calls `deletePaintingFromAtproto()`
   - Error handling: logs failures but doesn't block painting creation

3. **Updated `/workspaces/aesthetic-computer/system/netlify.toml`**
   - Added `@atproto/api` to external_node_modules
   - Added ATProto env vars: `PDS_URL`, `PDS_ADMIN_PASSWORD`
   - Already had MongoDB and Auth vars

### Painting Flow Now:

**Creating a painting:**
1. User creates painting → `track-media.mjs` receives it
2. Saves to MongoDB `paintings` collection with `code` and `slug`
3. 🆕 **Syncs to ATProto:**
   - Downloads painting image from S3/Spaces
   - Uploads as blob to ATProto PDS
   - Creates `computer.aesthetic.painting` record
   - Stores `atproto.rkey` back in MongoDB
4. Returns `{ slug, code }` to client

**Nuking a painting:**
1. User nukes painting → `track-media.mjs` receives PUT request
2. Updates MongoDB to set `nuked: true`
3. 🆕 **Removes from ATProto:**
   - Finds the `atproto.rkey` from MongoDB
   - Calls `deleteRecord` on ATProto PDS
4. Returns success

### Guest Paintings:
- Guest paintings (no auth) are **NOT** synced to ATProto
- They only go to MongoDB (same as before)
- No user account = no ATProto record

### Manual Sharing Scripts:
The existing manual sharing tools in `at/` still work:
- `share-latest-painting.mjs` - Share one painting as Bluesky post
- `bulk-share-paintings.mjs` - Share multiple paintings as Bluesky posts
- These create social media **posts**, separate from the ATProto **records**

## Date
Fixed: 2025-10-19
Paintings Added: 2025-10-19
