# Painting Tools - Status & Next Steps

**Date:** October 9, 2025  
**Status:** 🛠️ Tools ready, feature not yet implemented

---

## ✅ Completed

### 1. Planning
- [x] Comprehensive implementation plan in `/plans/painting-short-codes-plan.md`
- [x] Component-based architecture (single-commit ready)
- [x] Analyzed KidLisp `$code` system
- [x] Documented current painting upload flow

### 2. CLI Tools Suite (`/paintings`)
- [x] `inspect-spaces.mjs` - Digital Ocean Spaces browser
- [x] `inspect-mongodb.mjs` - MongoDB paintings collection inspector
- [x] `inspect-api.mjs` - API endpoint tester
- [x] `setup.mjs` - Environment configuration helper
- [x] Package.json with all dependencies

### 3. Bluesky Integration (`/at`)
- [x] `share-latest-painting.mjs` - Share specific user's paintings
- [x] `share-random-painting.mjs` - Share random paintings with upscaling
- [x] `bulk-share-paintings.mjs` - Batch posting tool
- [x] `delete-all-posts.mjs` - Cleanup utility
- [x] Image upscaling with sharp (nearest-neighbor for pixel art)
- [x] RichText facets for clickable links

---

## 🧪 Tested

### API Inspector Results (Live)
```bash
$ AC_API=https://aesthetic.computer node inspect-api.mjs --tv

✅ Status: 200 OK
✅ Duration: 698ms
📊 Found 10 paintings
❌ With codes: 0 (not implemented yet)
```

**Conclusion:** APIs work, no paintings have codes yet (expected)

---

## 🚧 Next Steps

### Component 1: Code Generator Module
**File:** `system/backend/painting-code-generator.mjs`

**Tasks:**
- [ ] Create standalone generator function
- [ ] Implement smart inference (colors, dimensions, handle)
- [ ] Implement random fallback with CVC patterns
- [ ] Add collision detection
- [ ] Generate SHA-256 hashes
- [ ] Unit tests

**Test with:**
```bash
node test-code-generator.mjs --image sample.png
```

### Component 2: Track Media API Enhancement
**File:** `system/netlify/functions/track-media.js`

**Tasks:**
- [ ] Import code generator module
- [ ] Add code generation on POST
- [ ] Calculate hash of uploaded image
- [ ] Check for duplicate hashes (deduplication)
- [ ] Return `{ slug, code }` to client
- [ ] Add MongoDB indexes: `code` (unique), `hash` (unique)

**Test with:**
```bash
# Upload a painting via web interface
# Check response includes code
node paintings/inspect-mongodb.mjs --recent 1
```

### Component 3: Migration Script
**File:** `paintings/migrate-codes.mjs`

**Tasks:**
- [ ] Query paintings without codes
- [ ] Download PNG from Digital Ocean
- [ ] Generate code using generator module
- [ ] Calculate hash
- [ ] Update MongoDB records
- [ ] Dry-run and execute modes
- [ ] Progress logging

**Test with:**
```bash
node migrate-codes.mjs --dry-run --limit 10
node migrate-codes.mjs --execute --limit 100
```

### Component 4: Lookup API
**File:** `system/netlify/functions/painting-code-lookup.mjs`

**Tasks:**
- [ ] Accept code parameter (strip # if present)
- [ ] Query MongoDB for painting
- [ ] Increment hits counter
- [ ] Update lastAccessed timestamp
- [ ] Return painting data with URLs

**Test with:**
```bash
node paintings/inspect-api.mjs --painting-code waf
curl https://aesthetic.computer/api/painting-code/waf
```

### Component 5: URL Routing
**File:** `system/public/aesthetic.computer/disks/prompt.mjs`

**Tasks:**
- [ ] Add route parser for `#code` pattern
- [ ] Call lookup API to resolve code
- [ ] Navigate to resolved painting
- [ ] Handle fragment routing `/#waf`

**Test with:**
```bash
# Visit: https://aesthetic.computer/#waf
# Visit: https://aesthetic.computer/painting~#waf
```

### Component 6: TV API Enhancement
**File:** `system/netlify/functions/tv.mjs`

**Tasks:**
- [ ] Include `code` field in projection
- [ ] Add `shortUrl` to response
- [ ] Support filtering: `?code=waf`
- [ ] Support sorting: `?sort=popular` (by hits)

**Test with:**
```bash
node paintings/inspect-api.mjs --tv
# Check if paintings now have codes in response
```

### Component 7: Frontend Display
**File:** `system/public/aesthetic.computer/disks/painting.mjs`

**Tasks:**
- [ ] Display `#code` on painting page
- [ ] Add copy-to-clipboard button
- [ ] Generate QR code for short URL
- [ ] Update share menu to use short URL

**Test manually:**
- Upload a painting
- See code displayed
- Copy and share short URL

### Component 8: Orphan Discovery
**File:** `paintings/audit-orphans.mjs`

**Tasks:**
- [ ] List all S3 paintings
- [ ] Query MongoDB for all slugs
- [ ] Find S3 files not in MongoDB
- [ ] Generate report
- [ ] Auto-import option

**Test with:**
```bash
node audit-orphans.mjs --report
node audit-orphans.mjs --import --dry-run
```

---

## 🔧 Development Workflow

### 1. Start Local Server
```bash
cd /workspaces/aesthetic-computer
npm run dev  # or whatever starts the local server
```

### 2. Configure Tools
```bash
cd paintings
node setup.mjs

# Set environment variables
export MONGODB_CONNECTION_STRING="mongodb://localhost:27017"
export MONGODB_NAME="aesthetic"
export AC_API="http://localhost:8888"
```

### 3. Develop & Test Iteratively
```bash
# Build component (e.g., code generator)
# Test it
node test-code-generator.mjs

# Inspect MongoDB before
node inspect-mongodb.mjs --stats

# Run migration
node migrate-codes.mjs --dry-run --limit 10

# Inspect MongoDB after
node inspect-mongodb.mjs --stats

# Test APIs
node inspect-api.mjs --tv
node inspect-api.mjs --painting-code waf
```

### 4. Verify Everything Works
```bash
# Upload a new painting via web interface
# Check it gets a code automatically
node inspect-mongodb.mjs --recent 1

# Visit short URL
open https://aesthetic.computer/#waf

# Check TV feed includes codes
curl https://aesthetic.computer/api/tv?limit=5 | jq '.media.paintings[].code'
```

### 5. Final Commit
```bash
git add .
git commit -m "Implement painting short codes system

- Code generator with smart inference and random fallback
- Track-media API generates codes on upload
- Migration script for existing paintings
- Lookup API for #code resolution
- URL routing for /#waf and /painting~#waf
- TV API includes codes and short URLs
- Frontend displays codes with QR and copy
- Orphan discovery and import tools
- Complete CLI inspection suite"

git push
```

---

## 📊 Success Criteria

- [ ] All existing paintings have unique codes
- [ ] New uploads automatically get codes
- [ ] Short URLs work: `aesthetic.computer/#waf`
- [ ] TV API returns codes in response
- [ ] Painting pages display codes
- [ ] QR codes work and scan correctly
- [ ] Collision rate < 0.01%
- [ ] No duplicate codes in database
- [ ] Migration completes without errors

---

## 🎯 Priority Order

1. **Code Generator** (foundation for everything)
2. **Track Media API** (new uploads get codes)
3. **Migration Script** (backfill existing paintings)
4. **Lookup API** (enable short URLs)
5. **URL Routing** (make short URLs work)
6. **TV API** (include codes in feed)
7. **Frontend Display** (show codes to users)
8. **Orphan Discovery** (cleanup any missing data)

---

## 📝 Notes

- All tools can target local or live environment via `AC_API`
- MongoDB inspection works with any connection string
- Digital Ocean inspection requires credentials
- API inspection works against live site (public APIs)
- Feature is non-breaking: old URLs still work
- Codes are additive: `{ slug, user, when, code, hash }`

---

**Ready to implement!** 🚀
