# Guest Bucket Expiration Policy Investigation

**Date:** October 9, 2025  
**Question:** Does the `art` bucket (guest bucket) have an expiration policy?

---

## Background

The `art` bucket on Digital Ocean Spaces is used for guest/anonymous uploads. Previously, there was discussion about a 24-hour expiration policy, but it's unclear if this was ever implemented.

## Current Code Analysis

### Presigned URL Expiration

From `/system/netlify/functions/presigned-url.js` (line 215):

```javascript
if (expiring) uploadOptions.expiresIn = 3600; 
// Set to expiring if we are in an anonymous bucket / not a logged in user.
```

**Important:** This only controls how long the **presigned upload URL** is valid (1 hour), NOT how long the actual file stays in storage.

### Bucket Selection Logic

1. **`art` bucket** (guest/anonymous):
   - Used for unauthenticated uploads
   - Presigned URL expires in 1 hour
   - Files themselves: **expiration policy unknown**

2. **`aesthetic-computer` bucket** (user):
   - Used for authenticated uploads  
   - Files stored under `{user_id}/painting/{slug}.png`
   - Files are permanent (no expiration)

3. **`wand` bucket**:
   - Legacy system
   - Unknown expiration policy

---

## How to Check

Use the new CLI tool to inspect lifecycle policies:

```bash
cd paintings

# Check all buckets
node check-lifecycle.mjs

# Or via npm script
npm run check:lifecycle
```

### What to Look For

The tool will show if any bucket has lifecycle rules like:

```
Rule 1: guest-expiration
  Status: Enabled
  ⏰ Expires after: 1 days
```

If it shows:

```
✅ No lifecycle rules configured
   → Files are permanent (never expire)
```

Then guest uploads **do not expire** and are permanent.

---

## Recommendation

Based on your comment: *"i dont mind if we just make that permanent now"*

### Keep Guest Uploads Permanent

**Pros:**
- Simpler architecture
- No data loss from guest uploads
- Consistent with user uploads
- Enables painting short codes for all paintings

**Cons:**
- Increased storage costs (minimal - PNGs are small)
- Old abandoned guest uploads stay forever

### If Permanent is Desired

1. Run `npm run check:lifecycle` to verify current state
2. If lifecycle rules exist, remove them via Digital Ocean console
3. Update documentation to reflect permanent storage
4. Consider adding DB tracking for guest uploads too

---

## Proposed Changes

If we make guest uploads permanent:

### Code Updates

1. **Remove expiration logic** (optional cleanup):
   ```javascript
   // In presigned-url.js
   // Remove or simplify:
   let expiring = true;
   if (expiring) uploadOptions.expiresIn = 3600;
   ```

2. **Track guest uploads in MongoDB** (optional):
   ```javascript
   // In track-media.js
   // Allow POST from guest bucket
   // Create painting records for guest uploads
   ```

3. **Add short codes to guest uploads**:
   - Generate codes for guest paintings
   - Allow `#code` lookups for guest art
   - Example: `aesthetic.computer/#abc` → guest painting

### Documentation Updates

- Update comments in presigned-url.js
- Add note about permanent storage
- Document guest upload workflow

---

## Next Steps

1. **Check current state:**
   ```bash
   npm run check:lifecycle
   ```

2. **If lifecycle rules exist:**
   - Decide if we want to keep or remove them
   - Document the decision
   - Update Digital Ocean if needed

3. **If no rules (already permanent):**
   - Document this in code comments
   - Consider tracking guest uploads
   - Enable short codes for guest art

---

## Environment Variables Needed

To run `check-lifecycle.mjs`, set in `.env`:

```env
DO_SPACES_KEY=your-key
DO_SPACES_SECRET=your-secret
DO_SPACES_ENDPOINT=nyc3.digitaloceanspaces.com
ART_SPACE_NAME=art
USER_SPACE_NAME=aesthetic-computer
WAND_SPACE_NAME=wand
```

These can be found in your Netlify environment variables or Digital Ocean console.

---

## Test Results

Run the lifecycle check and document results here:

```bash
$ npm run check:lifecycle

# Results will show:
# - art bucket: [?] days expiration OR permanent
# - aesthetic-computer bucket: [?] days expiration OR permanent  
# - wand bucket: [?] days expiration OR permanent
```

---

**Status:** 🔍 Investigation tool ready, awaiting test results
