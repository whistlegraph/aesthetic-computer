# Stow - File Upload & Sharing Feature

**Date:** 2024-11-24  
**Status:** Planning  
**Priority:** Medium

## Overview

Implement a `stow` piece that allows **authenticated users with handles** to drag and drop arbitrary files into aesthetic.computer, store them in S3, and share them via unique URLs. Following the chat upload pattern, this feature **requires registration and authentication** - no anonymous uploads are supported.

## Goal

Enable authenticated AC users to easily upload and share files (up to 1GB) through the AC interface using the existing BIOS drag-and-drop handler, with files stored in user-specific S3 buckets and tracked in MongoDB.

## User Flow

1. User types `stow` to enter the stow piece
2. **If not logged in:** Prompt to sign in (similar to chat authentication requirement)
3. **If logged in:** User drags a file into the browser window
4. BIOS drag-and-drop handler intercepts the file
5. Stow piece displays:
   - Filename
   - File size (formatted: bytes/KB/MB/GB)
   - File type/MIME type
   - "Stow" button to confirm upload
6. User clicks "Stow" button
7. File uploads to S3 with progress indicator
8. Upon completion, user receives a shareable URL
9. File is accessible via URL for downloading/sharing

## Technical Requirements

### Frontend (Stow Piece)

**Location:** `system/public/aesthetic.computer/disks/stow.mjs`

**Features:**
- Accept drag-and-drop events from BIOS
- Display file metadata:
  - Filename (truncated if too long)
  - File size with appropriate units
  - File type/extension
- Validate file size (max 1GB = 1,073,741,824 bytes)
- Show upload button
- Display upload progress (percentage, progress bar)
- Show success message with shareable URL
- Handle errors gracefully (file too large, network errors, etc.)
- Copy URL to clipboard functionality

**UI Elements:**
```
┌─────────────────────────────────┐
│  Drop file to stow              │
│                                 │
│  [File icon]                    │
│  filename.ext                   │
│  Size: 42.3 MB                  │
│                                 │
│  [ Stow ]                       │
└─────────────────────────────────┘

After upload:
┌─────────────────────────────────┐
│  Stowed!                        │
│                                 │
│  filename.ext                   │
│  https://ac.net/stow/abc123     │
│                                 │
│  [ Copy URL ] [ Stow Another ]  │
└─────────────────────────────────┘
```

### Backend (API Endpoints)

**Location:** `system/netlify/functions/` (following existing patterns)

**Endpoints needed:**

1. **GET `/presigned-upload-url/:ext/:filename/user`** (✅ Already exists)
   - **Already implemented** in `system/netlify/functions/presigned-url.js`
   - Already supports any file extension with proper MIME type mapping
   - Just needs to add MIME types for common file formats (pdf, doc, zip, etc.)
   - Requires authentication (Authorization header with JWT token)
   - Returns presigned S3 upload URL for user bucket
   - Files stored in: `{user.sub}/stow-{filename}` → becomes `{user.sub}/stow/{filename}`

2. **POST `/api/track-media`** (✅ Extend existing, no new endpoint needed!)
   - **Already handles:** paintings (.png), pieces (.mjs), tapes (.zip)
   - **Add support for:** stows (any other extension)
   - Input: `{ slug, ext, filename, fileSize, contentType }`
   - Requires authentication (must have valid token)
   - Generates unique short code (3-char, using existing code generator)
   - Creates MongoDB record in `stows` collection
   - Response: `{ slug, code, url }`
   - **Logic:** If ext not in ['png', 'mjs', 'zip'], treat as stow

3. **GET `/media/stows/:code`** (New media endpoint route)
   - Resolves stow code to actual file
   - Returns presigned download URL from S3
   - Logs download metrics (optional)
   - Similar to `/media/paintings/:code` and `/media/tapes/:code`
   - URL pattern: `^abc` (caret prefix for stow codes)

### S3 Storage Structure

**Bucket:** `user-aesthetic-computer` (existing authenticated user bucket)  
**Path structure:** `{user.sub}/stow/{slug}.{ext}`

Example: `auth0|abc123xyz/stow/2025.11.24.12.30.45.678.pdf`

**Following existing patterns:**
- Paintings: `{user.sub}/painting/{slug}.png`
- Pieces: `{user.sub}/piece/{slug}.mjs`
- Tapes: `{user.sub}/video/{slug}.zip`
- **Stows: `{user.sub}/stow/{slug}.{ext}`** (NEW)

**Storage configuration:**
- ACL: `public-read` (files accessible via presigned URLs)
- Bucket lifecycle: Permanent storage (no auto-expiration)
- CORS: Already configured for `*.aesthetic.computer`

### Database Schema

**Collection:** `stows` in MongoDB (aesthetic-computer database)

**Connection:** Via `aesthetic-computer-vault` environment variables
- `MONGODB_CONNECTION_STRING` - MongoDB Atlas connection string
- `MONGODB_NAME` - Database name (typically `aesthetic-computer`)

**Schema structure** (following paintings/tapes pattern):

```javascript
{
  _id: ObjectId("..."),
  code: "xyz",                           // 3-char short code (unique, indexed) - same as paintings/tapes
  slug: "2025.11.24.12.30.45.678",      // Timestamp-based slug (unique per user)
  user: "auth0|abc123xyz",              // Auth0 subject ID (REQUIRED - no anonymous)
  when: ISODate("2025-11-24T12:30:45.678Z"),
  bucket: "user-aesthetic-computer",
  
  // Stow-specific fields
  filename: "document.pdf",             // Original filename
  fileSize: 1234567,                    // Size in bytes
  contentType: "application/pdf",       // MIME type
  extension: "pdf",                     // File extension
  
  // Optional tracking
  downloadCount: 0,                     // Track downloads (future)
  nuked: false,                         // Soft delete flag
  
  // Future: ATProto sync (if we want stows in ATProto)
  // atproto: {
  //   rkey: "...",
  //   uri: "...",
  //   cid: "..."
  // }
}
```

**Indexes:**
```javascript
{ code: 1 }                    // Unique, for code lookup
{ user: 1 }                    // For user's stow list
{ when: 1 }                    // For chronological sorting
{ slug: 1 }                    // For slug lookup
{ slug: 1, user: 1 }          // Unique composite (slug unique per user)
```

### BIOS Integration

**Location:** `system/public/aesthetic.computer/bios.mjs` - `receivedUpload()` function

**Existing upload flow to follow:**
1. Request presigned URL from `/presigned-upload-url/{ext}/{filename}/user`
2. Requires authentication token in Authorization header
3. Upload file directly to S3 via presigned URL (XMLHttpRequest with PUT)
4. Track progress via `xhr.upload.addEventListener("progress")`
5. On success, call tracking endpoint (`/api/track-stow`)
6. Return code and URL to piece

**Drag & Drop:**
- BIOS already has drag-and-drop event handlers
- Check if current piece is `stow`
- Pass file object to stow piece via `disk` API
- Validate user is authenticated before allowing upload
- Prevent default browser file opening behavior

## Implementation Steps

### Phase 1: Database & Backend Setup
1. **Extend `track-media.mjs` to handle stows** (✅ Reuse existing endpoint!)
   - Current logic: `if (ext === "png")` → paintings, `else if (ext === "mjs")` → pieces, `else if (ext === "zip")` → tapes
   - **Add:** `else` → stows (any other extension)
   - Parse request body: `{ slug, ext, filename, fileSize, contentType }`
   - Require authentication (no anonymous uploads - already enforced)
   - Generate unique 3-char code using existing `generate-short-code.mjs`
   - Create MongoDB record in `stows` collection
   - Set up indexes (code, user, when, slug) - same as other media types
   - Return `{ code, slug, url }`

2. **Extend `presigned-url.js` MIME type mapping**
   - Already handles: png, zip, mjs, lisp, mp4, json, gltf, glb, obj
   - **Add common file types:**
     - Documents: pdf, doc, docx, txt, md
     - Images: jpg, jpeg, gif, webp, svg
     - Archives: tar, gz, 7z, rar
     - Audio: mp3, wav, ogg, m4a
     - Other: csv, xml, yaml, toml
   - Default fallback: `application/octet-stream`
   - Already enforces user authentication for `/user` bucket

3. **Create media retrieval endpoint** `/media/stows/:code`
   - Look up code in MongoDB `stows` collection
   - Generate presigned download URL from S3
   - Redirect to download URL
   - Optional: increment download counter
   - Pattern: Follow existing `/media/paintings/:code` and `/media/tapes/:code`
   - Accessible via: `https://aesthetic.computer/^abc` (caret prefix)

### Phase 2: Frontend Piece (`stow.mjs`)
1. **Create stow piece** at `system/public/aesthetic.computer/disks/stow.mjs`
2. **Authentication check:**
   - Check if user is logged in (via `user` parameter in boot/act)
   - If not logged in, show login prompt (like chat does)
   - Prevent drag-drop if not authenticated

3. **Implement UI states:**
   - Not logged in state (show login button)
   - Empty state (waiting for file, show drag target)
   - File preview state (showing metadata)
   - Uploading state (progress indicator)
   - Success state (shareable URL with copy button)
   - Error state (file too large, upload failed, etc.)

4. **Drag & drop integration:**
   - Listen for file drop events
   - Validate file size < 1GB before upload
   - Extract filename, size, type
   - Show preview and confirmation

5. **Upload implementation:**
   - Use existing `$commonApi.upload()` function
   - Pass file data, filename, progress callback
   - Handle upload:progress events
   - On success, receive code and create shareable URL

### Phase 3: URL Routing & Access
1. **Add route for stow codes** (in router or media endpoint)
   - `/stow/:code` → redirects to media retrieval
   - `/^:code` or `^code` → load stow viewer/downloader
   - Pattern: Similar to `!code` for tapes, `#code` for paintings, `^code` for stows

2. **File viewer/downloader** (optional future enhancement)
   - Direct download for all file types initially
   - Future: Preview for images, videos, PDFs, text files

### Phase 4: Testing & Polish
1. **Test authentication:**
   - Verify logged-out users cannot upload
   - Verify logged-in users can upload successfully
   
2. **Test file types:**
   - Images (jpg, png, gif, webp)
   - Videos (mp4, webm, mov)
   - Documents (pdf, doc, txt, md)
   - Archives (zip, tar, gz)
   - Code files (js, mjs, py, etc.)

3. **Test file sizes:**
   - Small files (< 1MB)
   - Medium files (10-100MB)
   - Large files (near 1GB limit)
   - Reject files > 1GB

4. **Test error scenarios:**
   - Network interruption during upload
   - Authentication token expiration
   - Duplicate filename handling
   - Invalid file types (if we want to restrict)

5. **Polish:**
   - Add smooth animations and transitions
   - Implement copy-to-clipboard for URLs
   - Add QR code generation for URLs (optional)
   - Sound feedback for upload completion

## File Size Limit

**Maximum:** 1GB (1,073,741,824 bytes)

**Validation:**
- Frontend: Check before upload
- Backend: Validate in upload URL generation
- S3: Configure bucket policy limits

## Security Considerations

1. **Authentication:** ✅ **REQUIRED** - User must be logged in with valid Auth0 token (no anonymous uploads)
2. **Authorization:** Files stored in user's directory (`{user.sub}/stow/`)
3. **Rate Limiting:** 
   - Consider limiting uploads per user per hour (e.g., 10 files/hour)
   - Consider storage quota per user (e.g., 10GB total)
4. **File Type Validation:**
   - Client-side: Check MIME type and extension
   - Server-side: Validate MIME type matches extension
   - Consider blocklist for executable file types (.exe, .sh, .bat, etc.)
5. **Presigned URL Expiration:** 
   - Upload URLs: 1 hour (3600s) - already implemented in `presigned-url.js`
   - Download URLs: Generate on-demand with short expiration (15 mins)
6. **File Name Sanitization:** 
   - Remove special characters
   - Use slug-based storage (timestamp) instead of original filename
   - Store original filename in MongoDB metadata only
7. **CORS:** ✅ Already configured in S3 bucket for `*.aesthetic.computer`
8. **Size Enforcement:** 
   - Frontend: Block files > 1GB before upload
   - Backend: Validate in presigned URL generation (could add max size check)
   - S3: Bucket policies can enforce max object size
9. **Content Scanning:** Future consideration for malware/virus scanning
10. **Soft Delete:** Use `nuked` flag instead of permanent deletion (recovery possible)

## Future Enhancements

- File expiration (auto-delete after X days)
- File organization/folders
- File search/listing
- Sharing permissions (private vs public)
- File previews for images/videos
- Batch upload (multiple files)
- Drag files out to download
- File versioning
- Storage quota per user
- Analytics dashboard (storage used, downloads, etc.)

## Dependencies

✅ **Already Available:**
- Auth0 authentication system (user login required)
- S3/Digital Ocean Spaces with presigned URL generation (`presigned-url.js`)
- MongoDB connection via `aesthetic-computer-vault` credentials
- BIOS upload infrastructure (`receivedUpload()` in `bios.mjs`)
- Short code generation (`generate-short-code.mjs`)
- Media routing system (paintings, tapes patterns to follow)

🆕 **To Implement:**
- Extend `track-media.mjs` to handle stows (add `else` case for non-png/mjs/zip extensions)
- Extend `presigned-url.js` MIME type mapping for common file types
- `stow.mjs` piece (frontend)
- `/media/stows/:code` route handler
- MongoDB `stows` collection with indexes

## Success Metrics

- Users can successfully upload files up to 1GB
- Files are retrievable via shareable URLs
- Upload success rate > 95%
- Average upload time acceptable for file size
- No security incidents related to file storage

## Open Questions

1. ✅ **Should files be publicly accessible or require authentication to download?**
   - Decision: Publicly accessible via presigned URLs (like paintings)
   - Anyone with the link/code can download
   - Files stored with `public-read` ACL in user bucket

2. **Do we want to implement file expiration from day one?**
   - Recommendation: No expiration initially (like paintings)
   - Future: Could add TTL or user-controlled expiration

3. **Should we compress files automatically?**
   - Recommendation: No automatic compression
   - User can upload .zip if they want compression
   - Preserve original file integrity

4. **What's the storage quota per user?**
   - Initial: No hard quota (monitor usage)
   - Future: Consider 10GB per user limit
   - Track total storage used per user in MongoDB aggregation

5. **Do we want to show a list of all stowed files for a user?**
   - Yes, good feature for future enhancement
   - Query: `db.stows.find({ user: "auth0|xyz", nuked: false })`
   - Show in a `/stow/list` or `/stow/@handle` page

6. **Should we allow file deletion?**
   - Yes, via soft delete (`nuked: true` flag)
   - Similar to painting nuke functionality
   - Keep file in S3 but hide from listings
   - Future: Hard delete after X days

7. **What analytics do we want to track?**
   - Upload count per user
   - Total storage used per user
   - Download count per file (optional)
   - Popular file types
   - Failed upload rate

8. 🆕 **Should we restrict certain file types?**
   - Recommendation: Allow all initially, blocklist executables
   - Blocklist: .exe, .bat, .sh, .cmd, .scr, .com
   - Monitor for abuse and adjust policy

9. 🆕 **URL format for stows?**
   - ✅ **Decision: Use `^abc` pattern (caret prefix)**
   - Caret works in URLs (encoded as %5E, handled transparently)
   - Distinct from existing prefixes: `!` (tapes), `#` (paintings), `$` (kidlisp)
   - Clean 3-character codes, same generation as paintings/tapes
   - Example: `https://aesthetic.computer/^abc` → stowed file

## Architecture Reference

**Following existing AC patterns:**

### Upload Flow (Unified Media Pattern)
```javascript
// 1. Client requests presigned URL
GET /presigned-upload-url/{ext}/stow-{timestamp}/user
Headers: { Authorization: "Bearer {token}" }
Response: { uploadURL: "https://..." }

// 2. Client uploads to S3
PUT {presignedURL}
Body: file blob
Headers: { Content-Type: "{mimeType}", x-amz-acl: "public-read" }

// 3. Client tracks in MongoDB (SAME endpoint as paintings/tapes!)
POST /api/track-media
Body: { slug, ext, filename, fileSize, contentType }
Headers: { Authorization: "Bearer {token}" }
Response: { code: "abc", slug: "...", url: "https://aesthetic.computer/^abc" }

// track-media.mjs logic:
if (ext === "png") → type = "paintings"    → URL: #abc
else if (ext === "mjs") → type = "pieces"  → URL: $abc  
else if (ext === "zip") → type = "tapes"   → URL: !abc
else → type = "stows"                       → URL: ^abc  ✨ NEW
```

### Storage Pattern
```
user-aesthetic-computer/
  {user.sub}/
    painting/
      {slug}.png
    piece/
      {slug}.mjs
    video/
      {slug}.zip
    stow/              ← NEW
      {slug}.{ext}     ← NEW
```

### MongoDB Pattern
```javascript
// Similar to paintings and tapes collections
paintings: { code, slug, user, when, bucket, nuked?, atproto? }
tapes:     { code, slug, user, when, bucket, nuked?, mp4Status, mp4Url, atproto? }
stows:     { code, slug, user, when, bucket, nuked?, filename, fileSize, contentType, extension }
```

### Code Reference Files
- `system/netlify/functions/presigned-url.js` - S3 presigned URL generation (extend MIME types)
- `system/netlify/functions/track-media.mjs` - Media tracking (extend for stows)
- `system/public/aesthetic.computer/bios.mjs` - Upload handler (`receivedUpload()`)
- `system/backend/generate-short-code.mjs` - Code generation (already supports random mode)
- `system/backend/database.mjs` - MongoDB connection

### Implementation Notes
**Why reuse `track-media.mjs`?**
- ✅ Reduces code duplication
- ✅ Consistent authentication/authorization logic
- ✅ Reuses existing code generation (same 3-char system)
- ✅ Same MongoDB connection pattern
- ✅ Consistent error handling
- ✅ Single endpoint to maintain

**Minimal changes needed:**
1. Add `else` case in `track-media.mjs` for non-png/mjs/zip extensions
2. Add MIME type mappings in `presigned-url.js`
3. Create `/media/stows/:code` retrieval endpoint
4. Build `stow.mjs` frontend piece
5. Update router to recognize `^code` pattern for stows (caret prefix)

**Code prefix patterns:**
- `#abc` → Paintings (hash)
- `$abc` → KidLisp pieces (dollar)
- `!abc` → Tapes (bang/exclamation)
- `^abc` → Stows (caret) ✨ NEW

**Code prefix patterns:**
- `#abc` → Paintings (hash)
- `$abc` → KidLisp pieces (dollar)
- `!abc` → Tapes (bang/exclamation)
- `^abc` → Stows (caret) ✨ NEW
