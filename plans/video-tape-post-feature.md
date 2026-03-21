# 📼 Video Tape Post Feature - Implementation Plan

**Last Updated:** 2025-10-17  
**Status:** Planning Complete ✅

## 📋 Quick Reference

### **Architecture Pattern**
- **Base Model:** Paintings (MongoDB only) + Moods (MongoDB + ATProto dual-write)
- **Tapes Approach:** Deferred ATProto sync (wait for MP4 conversion)

### **Key Files & Line Numbers**

| File | Lines | Purpose |
|------|-------|---------|
| `video.mjs` | ~237, ~630 | Add POST button UI and handler |
| `bios.mjs` | ~4950+ | Add `create-and-post-tape` message handler |
| `track-tape.mjs` | NEW | Create MongoDB record, queue MP4 conversion |
| `tape-atproto.mjs` | NEW | Create ATProto record with MP4 blob |
| `tape-mp4-complete.mjs` | NEW | Webhook for MP4 completion → ATProto sync |
| `tape.json` | NEW | ATProto lexicon definition |

### **Execution Flow**
```
User clicks POST
  → video.mjs:630 creates ZIP frames
  → bios.mjs:4950+ creates ZIP file
  → track-tape.mjs:104 inserts MongoDB record
  → track-tape.mjs:110 queues MP4 conversion (async)
  → track-tape.mjs:115 returns { code, slug } to user ✅
  
[Background: 5-10 minutes later]
  → MP4 conversion completes
  → tape-mp4-complete.mjs:15 receives callback
  → tape-atproto.mjs:23 downloads MP4
  → tape-atproto.mjs:26 uploads MP4 blob to ATProto
  → tape-atproto.mjs:48 updates MongoDB with at: { rkey, uri, cid } ✅
```

---

## 🎯 Feature Overview

Add a new "Post" button to the `video.mjs` piece that uploads recorded tapes (.zip) to cloud storage, creates MongoDB records, generates ATProto records, and queues MP4 conversion via serverless function.

### Key Components
1. **Frontend:** New "Post" button in `video.mjs` 
2. **Upload:** Presigned S3/DO Spaces upload (existing pattern)
3. **Database:** New `tapes` MongoDB collection
4. **ATProto:** New `computer.aesthetic.tape` lexicon
5. **Video Processing:** Netlify/microservice for MP4 conversion from ZIP frames

---

## 🎯 Goals

- ✅ Upload tape .zip files to user/guest buckets (like paintings)
- ✅ Create MongoDB `tapes` collection records
- ✅ Generate ATProto records for federated sharing
- ✅ Queue async MP4 generation from ZIP frames
- ✅ Return shareable code (#abc) for tape lookup
- ✅ Support both authenticated users and anonymous guests

---

## 📊 Current Architecture Review

### Existing Patterns to Follow

#### 1. **Paintings Upload Flow** (Reference: `track-media.mjs`)
```javascript
// POST /api/track-media
{
  slug: "2025.10.17.12.30.00.123",
  ext: "png",
  // Returns: { slug, code: "abc" }
}

// MongoDB paintings collection:
{
  _id: ObjectId,
  code: "abc",           // 3-char short code
  slug: "timestamp",
  user: "auth0|..." | undefined,  // Optional for guests
  when: Date,
  bucket: "user-aesthetic-computer" | "art-aesthetic-computer",
  nuked: false,
  atproto: {            // Optional, added after ATProto sync
    rkey: "...",
    uri: "...",
    cid: "..."
  }
}
```

#### 2. **Moods ATProto Integration** (Reference: `/at/lexicons/computer/aesthetic/mood.json`)
```json
{
  "lexicon": 1,
  "id": "computer.aesthetic.mood",
  "defs": {
    "main": {
      "type": "record",
      "key": "tid",
      "record": {
        "required": ["mood", "when", "ref"],
        "properties": {
          "mood": { "type": "string" },
          "when": { "type": "string", "format": "datetime" },
          "ref": { "type": "string" }  // MongoDB _id reference
        }
      }
    }
  }
}
```

#### 3. **Presigned Upload Flow** (Reference: `bios.mjs` lines 12030-12200)
```javascript
// 1. Request presigned URL
fetch("/presigned-upload-url/zip", { 
  headers: { Authorization: `Bearer ${token}` } 
});

// 2. Upload to S3/DO Spaces
xhr.open("PUT", presignedUrl);
xhr.send(blob);

// 3. Track in database
fetch("/api/track-media", {
  method: "POST",
  body: JSON.stringify({ slug, ext: "zip" })
});

// Returns: { slug, code, url }
```

#### 4. **ZIP Export** (Reference: `bios.mjs` lines 4861-4950, `video.mjs` lines 549-630)
```javascript
// video.mjs sends message:
send({
  type: "create-animated-frames-zip",
  content: { frames: frameRecord }
});

// bios.mjs creates ZIP with:
// - PNG frames (6x scaled, with stamps)
// - timing.json (frame timing data)
// - metadata.json (dimensions, duration, fps)
```

---

## 🗄️ New MongoDB Schema: `tapes` Collection

**What's in the ZIP's metadata.json:**
```javascript
{
  originalSize: { width: 256, height: 256 },
  scaledSize: { width: 1536, height: 1536 },  // 6x scaling
  scale: 6,
  frameCount: 240,
  totalDuration: 4000,  // milliseconds
  exportedAt: "2025-10-17T12:30:00.123Z"
}
```

**MongoDB tapes record (minimal - follows paintings pattern):**
```javascript
{
  _id: ObjectId("..."),
  code: "xyz",                    // 3-char short code (like paintings)
  slug: "piece-2025.10.17.12.30.00.123",  // piece-timestamp format
  user: "auth0|123" | undefined,  // Optional: auth0 subject or undefined for guests
  when: ISODate("2025-10-17T12:30:00.123Z"),
  bucket: "user-aesthetic-computer" | "art-aesthetic-computer",
  
  // ATProto record (optional - only exists if ATProto sync succeeds)
  at: {
    rkey: "...",
    uri: "at://did:plc:abc/computer.aesthetic.tape/rkey",
    cid: "..."
  },
  
  nuked: false,
  
  // Indexes: code (unique), user, when, slug
}
```

**Key simplifications:**
- ❌ No mp4Status/mp4JobId - we don't need to track conversion in MongoDB
- ❌ No atprotoStatus - if `at` field exists, it's synced; if not, it's not
- ❌ No metadata duplication - read from ZIP's metadata.json when needed
- ✅ Follows exact same pattern as `paintings` collection
- ✅ URLs constructed on-demand: `https://${bucket}.nyc3.digitaloceanspaces.com/${slug}.zip`

---

## 🎨 ATProto Lexicon: `computer.aesthetic.tape`

**File:** `/at/lexicons/computer/aesthetic/tape.json`

```json
## 🌐 ATProto Lexicon: `computer.aesthetic.tape`

```json
{
  "lexicon": 1,
  "id": "computer.aesthetic.tape",
  "defs": {
    "main": {
      "type": "record",
      "description": "A video tape recording from Aesthetic Computer",
      "key": "tid",
      "record": {
        "type": "object",
        "required": ["slug", "code", "when", "video", "ref"],
        "properties": {
          "slug": {
            "type": "string",
            "description": "Unique slug identifier (piece-timestamp)"
          },
          "code": {
            "type": "string",
            "maxLength": 10,
            "description": "Short code for discovery (#xyz)"
          },
          "when": {
            "type": "string",
            "format": "datetime",
            "description": "When the tape was created (ISO 8601)"
          },
          "video": {
            "type": "blob",
            "accept": ["video/mp4"],
            "maxSize": 10485760,
            "description": "MP4 video blob (max 10MB)"
          },
          "ref": {
            "type": "string",
            "description": "Reference to source database record for bidirectional sync"
          }
        }
      }
    }
  }
}
```

**Simplified to essentials:**
- ✅ Only required fields: slug, code, when, video blob, ref
- ❌ Removed frames, duration, fps, resolution, piece - not needed for ATProto display
- ❌ Removed zipUrl, mp4Url - not needed, video blob contains the content
- ✅ Follows mood.json pattern (minimal lexicon)
```

---

## � Case Study: Existing Pipelines (Paintings & Moods)

### **PAINTINGS Pipeline** (No ATProto)

**File:** `/system/netlify/functions/track-media.mjs`

```javascript
// Line 99-106: MongoDB record creation
const record = {
  code,          // 3-char short code
  slug,
  when: new Date(),
  bucket: user ? "user-aesthetic-computer" : "art-aesthetic-computer",
};

if (user) {
  record.user = user.sub;
}

await collection.insertOne(record);  // Line 104

// That's it! No ATProto integration.
return respond(200, { slug, code });  // Line 109
```

**MongoDB Schema:**
```javascript
{
  _id: ObjectId("..."),
  code: "xyz",
  slug: "line-2025.10.17.12.30.00.123",
  user: "auth0|123" | undefined,
  when: ISODate("2025-10-17T12:30:00.123Z"),
  bucket: "user-aesthetic-computer" | "art-aesthetic-computer",
  nuked: false
}
```

**No ATProto fields** - paintings are MongoDB only!

---

### **MOODS Pipeline** (With ATProto Dual-Write)

**File:** `/system/netlify/functions/mood.mjs`

```javascript
// Line 107-111: MongoDB record creation FIRST
const insertResult = await collection.insertOne({
  user: user.sub,
  mood,
  when: new Date(),
});

// Line 114-121: Dual-write to ATProto IMMEDIATELY
try {
  const atprotoResult = await createMoodOnAtproto(
    database,
    user.sub,
    mood,
    new Date(),
    insertResult.insertedId.toString(),  // Pass MongoDB ID as ref
  );

  // Line 123-128: Update MongoDB with ATProto rkey
  if (atprotoResult.rkey) {
    await collection.updateOne(
      { _id: insertResult.insertedId },
      { $set: { atproto: { rkey: atprotoResult.rkey } } },
    );
  }
} catch (atprotoError) {
  // Line 131: Log error but don't fail - mood already saved
  console.error("⚠️  Failed to sync mood to ATProto:", atprotoError);
}
```

**File:** `/system/backend/mood-atproto.mjs`

```javascript
// Line 17-23: Function signature
export async function createMoodOnAtproto(
  database,
  sub,
  moodText,
  moodDate,
  refId  // MongoDB _id as reference
)

// Line 28-33: Check user has ATProto credentials
const user = await users.findOne({ _id: sub });
if (!user?.atproto?.did || !user?.atproto?.password) {
  return { error: "No ATProto account" };
}

// Line 38-41: Login to ATProto
const agent = new AtpAgent({ service: PDS_URL });
await agent.login({
  identifier: user.atproto.did,
  password: user.atproto.password,
});

// Line 44-53: Create ATProto record
const atprotoRecord = await agent.com.atproto.repo.createRecord({
  repo: user.atproto.did,
  collection: "computer.aesthetic.mood",
  record: {
    $type: "computer.aesthetic.mood",
    mood: moodText,
    when: moodDate.toISOString(),
    ref: refId,  // Bidirectional reference to MongoDB
  },
});

// Line 61-62: Return rkey
const rkey = uri.split("/").pop();
return { rkey, uri };
```

**MongoDB Schema (After ATProto sync):**
```javascript
{
  _id: ObjectId("67117c4d1a2b3c4d5e6f7890"),
  user: "auth0|123",
  mood: "feeling great! ☀️",
  when: ISODate("2025-10-17T12:30:00.123Z"),
  atproto: {
    rkey: "3labc123def"  // Only if ATProto sync succeeded
  }
}
```

**ATProto Lexicon:** `/at/lexicons/computer/aesthetic/mood.json`
```json
{
  "required": ["mood", "when", "ref"],
  "properties": {
    "mood": { "type": "string", "maxLength": 5000 },
    "when": { "type": "string", "format": "datetime" },
    "ref": { "type": "string", "description": "MongoDB _id reference" }
  }
}
```

---

### **Key Differences:**

| Feature | Paintings | Moods | Tapes (Proposed) |
|---------|-----------|-------|------------------|
| MongoDB fields | `code, slug, when, bucket, user` | `user, mood, when, atproto.rkey` | `code, slug, when, bucket, user, at.{rkey,uri,cid}` |
| ATProto sync | ❌ None | ✅ Immediate dual-write | ✅ After MP4 conversion |
| ATProto field | N/A | `atproto: { rkey }` | `at: { rkey, uri, cid }` |
| Timing | Instant response | Instant (dual-write) | ZIP instant, ATProto deferred |

---

### **TAPES Pipeline Design** (Hybrid Approach)

**Why different from moods?**
- Moods are instant text → can dual-write immediately
- Tapes need MP4 conversion (minutes) → must defer ATProto sync

**Flow:**
1. **track-tape.mjs** (Line ~104): `insertOne({ code, slug, when, bucket, user })`
2. **track-tape.mjs** (Line ~110): Queue MP4 conversion (async)
3. **track-tape.mjs** (Line ~115): Return `{ code, slug }` immediately
4. **[Background]** MP4 conversion service processes ZIP → MP4
5. **tape-mp4-complete.mjs** (Line ~20): Receives callback with `mp4Url`
6. **tape-atproto.mjs** (Line ~30): Downloads MP4, uploads to ATProto as blob
7. **tape-atproto.mjs** (Line ~50): Creates ATProto record with video blob
8. **tape-atproto.mjs** (Line ~60): Updates MongoDB `at: { rkey, uri, cid }`

**Result:** Same bi-directional sync as moods, but deferred until MP4 ready.

---

### **Execution Flow Comparison**

#### **MOODS (Immediate Sync)**
```
mood.mjs:107     → collection.insertOne({ user, mood, when })
                   ✅ MongoDB record created
                   
mood.mjs:114     → createMoodOnAtproto(database, user, mood, date, mongoId)
  ↳ mood-atproto.mjs:28  → Check user.atproto.did/password
  ↳ mood-atproto.mjs:38  → agent.login()
  ↳ mood-atproto.mjs:44  → agent.createRecord({ mood, when, ref })
  ↳ mood-atproto.mjs:61  → return { rkey, uri }
  
mood.mjs:123     → collection.updateOne({ $set: { atproto: { rkey } } })
                   ✅ MongoDB updated with ATProto reference
                   
mood.mjs:146     → respond(200, { mood })
                   ✅ User gets response (all sync complete)
```

#### **TAPES (Deferred Sync)**
```
track-tape.mjs:104    → collection.insertOne({ code, slug, when, bucket, user })
                        ✅ MongoDB record created (no ATProto yet)
                        
track-tape.mjs:110    → queueMP4Conversion({ mongoId, bucket, slug, callbackUrl })
                        🔄 Background job starts (non-blocking)
                        
track-tape.mjs:115    → respond(200, { code, slug })
                        ✅ User gets response immediately
                        
[5-10 minutes later, MP4 conversion completes]

tape-mp4-complete.mjs:15    → POST callback receives { mongoId, mp4Url, status: "complete" }
tape-mp4-complete.mjs:23    → createTapeOnAtproto(mongoId, mp4Url)
  ↳ tape-atproto.mjs:15     → tapes.findOne({ _id: mongoId })
  ↳ tape-atproto.mjs:23     → fetch(mp4Url) → download MP4
  ↳ tape-atproto.mjs:26     → agent.uploadBlob(mp4Buffer) → upload to ATProto
  ↳ tape-atproto.mjs:31     → agent.createRecord({ slug, code, when, video: blob, ref })
  ↳ tape-atproto.mjs:48     → tapes.updateOne({ $set: { at: { rkey, uri, cid } } })
  
tape-mp4-complete.mjs:26    → respond(200, { message: "Success" })
                              ✅ ATProto sync complete (user doesn't see this)
```

**Key Insight:** 
- Moods: 1 request → MongoDB + ATProto done before response
- Tapes: 1 request → MongoDB done, response sent; ATProto happens later via webhook

---

### **Field Mapping Reference**

#### **MongoDB → ATProto Sync**

| MongoDB Field | ATProto Field | Moods | Tapes | Notes |
|---------------|---------------|-------|-------|-------|
| `_id.toString()` | `record.ref` | ✅ | ✅ | Bidirectional reference |
| `user` | (not synced) | ❌ | ❌ | ATProto uses agent.session.did |
| `mood` | `record.mood` | ✅ | N/A | Text content |
| `code` | `record.code` | N/A | ✅ | Short discovery code |
| `slug` | `record.slug` | N/A | ✅ | Unique identifier |
| `when` | `record.when` | ✅ | ✅ | ISO 8601 timestamp |
| (mp4 blob) | `record.video` | N/A | ✅ | Binary blob upload |

#### **ATProto → MongoDB Sync**

| ATProto Response | MongoDB Field | Moods | Tapes |
|------------------|---------------|-------|-------|
| `response.uri.split('/').pop()` | `atproto.rkey` | ✅ | `at.rkey` ✅ |
| `response.uri` | (not stored) | ❌ | `at.uri` ✅ |
| `response.cid` | (not stored) | ❌ | `at.cid` ✅ |

**Why tapes stores more?**
- Moods only need `rkey` for deletion/updates
- Tapes store full `{ rkey, uri, cid }` for richer federation features

---

### **MongoDB Collection Schemas (Final)**

#### **paintings** (No ATProto)
```javascript
{
  _id: ObjectId,
  code: string,      // "xyz"
  slug: string,      // "line-2025.10.17..."
  user?: string,     // "auth0|123"
  when: Date,
  bucket: string,    // "user-aesthetic-computer"
  nuked: boolean
}
```

#### **moods** (With ATProto)
```javascript
{
  _id: ObjectId,
  user: string,      // "auth0|123"
  mood: string,      // "feeling great!"
  when: Date,
  atproto?: {        // Only if synced
    rkey: string     // "3labc123"
  },
  deleted?: boolean
}
```

#### **tapes** (With Deferred ATProto)
```javascript
{
  _id: ObjectId,
  code: string,      // "xyz"
  slug: string,      // "wand-2025.10.17..."
  user?: string,     // "auth0|123"
  when: Date,
  bucket: string,    // "user-aesthetic-computer"
  at?: {             // Only after MP4 + ATProto complete
    rkey: string,    // "3lxyz789"
    uri: string,     // "at://did:plc:.../computer.aesthetic.tape/3lxyz789"
    cid: string      // "bafyrei..."
  },
  nuked: boolean
}
```

**Design Consistency:**
- ✅ All use `code` for short lookups (paintings, tapes)
- ✅ All use `slug` for unique identifiers (paintings, tapes)
- ✅ All use optional `user` field (undefined for guests)
- ✅ ATProto field naming: moods use `atproto`, tapes use `at` (shorter, like paintings would)
- ✅ All support soft delete via `nuked` or `deleted` field

---

### **ATProto Record Examples**

#### **Mood ATProto Record** (computer.aesthetic.mood)
```javascript
// Created at: mood-atproto.mjs:44-53
{
  $type: "computer.aesthetic.mood",
  mood: "feeling great! ☀️",
  when: "2025-10-17T12:30:00.123Z",
  ref: "67117c4d1a2b3c4d5e6f7890"  // MongoDB _id
}

// Response:
{
  uri: "at://did:plc:abc123.../computer.aesthetic.mood/3labc123def",
  cid: "bafyrei..."
}

// Stored in MongoDB as:
{ atproto: { rkey: "3labc123def" } }
```

#### **Tape ATProto Record** (computer.aesthetic.tape)
```javascript
// Created at: tape-atproto.mjs:31-42
{
  $type: "computer.aesthetic.tape",
  slug: "wand-2025.10.17.12.30.00.123",
  code: "xyz",
  when: "2025-10-17T12:30:00.123Z",
  video: {  // BLOB - uploaded via agent.uploadBlob()
    $type: "blob",
    ref: { $link: "bafkreiabcdef..." },
    mimeType: "video/mp4",
    size: 5242880  // 5MB
  },
  ref: "67117c4d1a2b3c4d5e6f7890"  // MongoDB _id
}

// Response:
{
  uri: "at://did:plc:abc123.../computer.aesthetic.tape/3lxyz789",
  cid: "bafyrei..."
}

// Stored in MongoDB as:
{
  at: {
    rkey: "3lxyz789",
    uri: "at://did:plc:abc123.../computer.aesthetic.tape/3lxyz789",
    cid: "bafyrei..."
  }
}
```

**Blob Upload Flow:**
```javascript
// tape-atproto.mjs:23-28
const mp4Response = await fetch(mp4Url);
const mp4Buffer = await mp4Response.arrayBuffer();

const uploadRes = await agent.uploadBlob(new Uint8Array(mp4Buffer), {
  encoding: 'video/mp4',
});

const videoBlob = uploadRes.data.blob;  // { $type: "blob", ref: {...}, mimeType, size }
```

---

## 🔧 Implementation Phases

---

## �🔧 Implementation Phases

> **🎯 KEY CHANGE:** ATProto record is created AFTER MP4 conversion completes, so the blob is the actual MP4 video, not a thumbnail.

### Phase 1: Frontend - Add "Post" Button to `video.mjs`

**File:** `/system/public/aesthetic.computer/disks/video.mjs`

**Changes:**

1. **Add Post button UI** (after line 237)
```javascript
// Add Post button (positioned separately)
if (!postBtn)
  postBtn = new ui.TextButton("POST", { right: 40, bottom: 40, screen });
postBtn.reposition({ right: 40, bottom: 40, screen });
postBtn.paint(api);
```

2. **Add Post button action handler** (after framesBtn handler ~line 630)
```javascript
// Post tape to cloud (upload + database + atproto)
postBtn?.act(e, {
  down: () => {
    synth({
      type: "sine",
      tone: 900,
      attack: 0.1,
      decay: 0.99,
      volume: 0.75,
      duration: 0.001,
    });
  },
  push: async () => {
    isPostingTape = true;
    isPrinting = true;
    currentExportType = "post";
    postBtn.disabled = true;
    
    // Initialize progress
    if (!useExtendedProgressBar) {
      rec.tapeProgress = 0;
    }
    
    try {
      // Request frames to create ZIP
      rec.requestFrames(async (frameData) => {
        if (frameData.frames && frameData.frames.length > 0) {
          // Convert frames to ZIP format
          const frameRecord = frameData.frames.map((frame, index) => {
            const [timestamp, imageData] = frame;
            let duration = 16.67; // Default 60fps
            
            if (index < frameData.frames.length - 1) {
              const nextTimestamp = frameData.frames[index + 1][0];
              duration = Math.max(10, nextTimestamp - timestamp);
            }
            
            return {
              timestamp,
              duration,
              width: imageData.width,
              height: imageData.height,
              data: imageData.data
            };
          });
          
          // Send to bios for ZIP creation AND upload
          send({
            type: "create-and-post-tape",
            content: {
              frames: frameRecord,
              piece: parsed.text[0] || "unknown", // Current piece name
            }
          });
          
          // Reset will happen after upload completes
        } else {
          isPrinting = false;
          currentExportType = "";
          postBtn.disabled = false;
        }
        
        isPostingTape = false;
      });
    } catch (error) {
      console.error("Error posting tape:", error);
      isPostingTape = false;
      isPrinting = false;
      currentExportType = "";
      postBtn.disabled = false;
    }
    
    synth({
      type: "sine",
      tone: 1000,
      attack: 0.1,
      decay: 0.99,
      volume: 0.75,
      duration: 0.005,
    });
  },
});
```

3. **Add state variables** (after line 48)
```javascript
let postBtn;
let isPostingTape = false;
```

4. **Handle upload completion callback** (in message handler)
```javascript
// In boot() function where messages are handled:
if (type === "tape:posted") {
  isPrinting = false;
  currentExportType = "";
  postBtn.disabled = false;
  
  const { code, slug, url } = content.data;
  completionMessage = `POSTED! #${code}`;
  completionMessageTimer = 180; // 3 seconds
  
  console.log(`📼 Tape posted successfully: #${code}`, url);
  triggerRender();
}

if (type === "tape:post-error") {
  isPrinting = false;
  currentExportType = "";
  postBtn.disabled = false;
  
  completionMessage = "POST FAILED";
  completionMessageTimer = 120;
  
  console.error("📼 Tape post failed:", content.error);
  triggerRender();
}
```

---

### Phase 2: BIOS - ZIP Creation & Upload Handler

**File:** `/system/public/aesthetic.computer/bios.mjs`

**Add new message handler** (after line 4950)

```javascript
// 📼 Create tape ZIP and upload to cloud
if (type === "create-and-post-tape") {
  console.log(
    "📼 Creating and posting tape with",
    content.frames.length,
    "frames"
  );

  // Send initial progress
  send({
    type: "recorder:transcode-progress",
    content: 0.01,
  });

  try {
    if (content.frames.length === 0) {
      throw new Error("No frames provided for tape creation");
    }

    // Load JSZip
    if (!window.JSZip) await loadJSZip();
    const zip = new window.JSZip();

    // Use 6x scaling for frames (same as regular ZIP export)
    const scale = 6;
    const originalWidth = content.frames[0].width;
    const originalHeight = content.frames[0].height;
    const scaledWidth = originalWidth * scale;
    const scaledHeight = originalHeight * scale;

    console.log(
      `📏 Tape scaling: ${originalWidth}x${originalHeight} -> ${scaledWidth}x${scaledHeight}`
    );

    // Create canvas for processing
    const canvas = document.createElement("canvas");
    const ctx = canvas.getContext("2d");
    canvas.width = scaledWidth;
    canvas.height = scaledHeight;

    // Collect timing and metadata
    const timingData = [];
    let totalDuration = 0;

    // Process each frame
    for (let i = 0; i < content.frames.length; i++) {
      const frame = content.frames[i];

      // Update progress (0.01-0.70 for frame processing)
      const progress = (i / content.frames.length) * 0.7 + 0.01;
      send({
        type: "recorder:transcode-progress",
        content: progress,
      });

      if (i % 10 === 0 || i === content.frames.length - 1) {
        console.log(
          `🖼️ Processing tape frame ${i + 1}/${content.frames.length}`
        );
      }

      // Create ImageData
      const imageData = new ImageData(
        new Uint8ClampedArray(frame.data),
        frame.width,
        frame.height
      );

      // Clear and draw scaled frame
      ctx.clearRect(0, 0, scaledWidth, scaledHeight);
      
      const tempCanvas = document.createElement("canvas");
      const tempCtx = tempCanvas.getContext("2d");
      tempCanvas.width = frame.width;
      tempCanvas.height = frame.height;
      
      tempCtx.putImageData(imageData, 0, 0);
      
      // Add aesthetic computer stamp
      await addAestheticComputerStamp(
        tempCtx,
        frame.width,
        frame.height,
        0,
        frame.data,
        frame,
        i,
        content.frames.length
      );
      
      // Scale up with nearest neighbor
      ctx.imageSmoothingEnabled = false;
      ctx.drawImage(tempCanvas, 0, 0, scaledWidth, scaledHeight);

      // Convert to PNG blob
      const blob = await new Promise((resolve) =>
        canvas.toBlob(resolve, "image/png")
      );

      // Add to ZIP with padded filename
      const filename = `frame${String(i).padStart(6, "0")}.png`;
      zip.file(filename, blob);

      // Record timing
      timingData.push({
        filename,
        duration: frame.duration,
        timestamp: frame.timestamp,
      });
      
      totalDuration += frame.duration;
    }

    // Add timing.json
    send({
      type: "recorder:transcode-progress",
      content: 0.75,
    });
    
    zip.file("timing.json", JSON.stringify(timingData, null, 2));

    // Add metadata.json
    const metadata = {
      piece: content.piece || "unknown",
      frames: content.frames.length,
      duration: totalDuration,
      fps: Math.round(content.frames.length / (totalDuration / 1000)),
      resolution: {
        width: scaledWidth,
        height: scaledHeight,
        originalWidth,
        originalHeight,
        scale,
      },
      created: new Date().toISOString(),
    };
    
    zip.file("metadata.json", JSON.stringify(metadata, null, 2));

    // Generate ZIP blob
    send({
      type: "recorder:transcode-progress",
      content: 0.85,
    });
    
    const zipBlob = await zip.generateAsync({ type: "blob" });
    
    console.log(
      `📦 Tape ZIP created: ${(zipBlob.size / 1024 / 1024).toFixed(2)}MB`
    );

    // Generate filename: piece-timestamp.zip
    const timestamp = new Date()
      .toISOString()
      .replace(/[-:]/g, ".")
      .replace("T", ".")
      .split(".")[0] + "." + Date.now() % 1000;
    const piece = content.piece || "tape";
    const filename = `${piece}-${timestamp}.zip`;

    // Upload to cloud (using existing receivedUpload pattern)
    send({
      type: "recorder:transcode-progress",
      content: 0.90,
    });
    
    // Upload and track
    receivedUpload(
      { filename, data: zipBlob },
      "tape:posted",  // Success callback
      metadata  // Pass metadata for database
    );

  } catch (error) {
    console.error("Error creating/posting tape:", error);
    send({
      type: "tape:post-error",
      content: { error: error.message },
    });
  }
  
  return;
}
```

**Modify `receivedUpload` function** (around line 12030) to handle tape metadata:

```javascript
async function receivedUpload(
  { filename, data },
  callbackMessage = "uploaded",
  metadata = null  // NEW: Optional metadata for tapes
) {
  // ... existing code ...
  
  // After successful upload, if it's a tape:
  if (ext === "zip" && metadata && callbackMessage === "tape:posted") {
    // Track tape in database with metadata
    const tapeData = {
      slug: slug,
      ext: "zip",
      metadata: metadata  // Include frames, duration, fps, etc.
    };
    
    options.body = JSON.stringify(tapeData);
    const added = await fetch("api/track-tape", options);  // NEW endpoint
    const addedData = await added.json();
    
    if (addedData.code) {
      console.log(`📼 Tape code: #${addedData.code}`);
      data.code = addedData.code;
    }
  }
  
  // ... rest of existing code ...
}
```

---

### Phase 3: Backend - Track Tape Endpoint

**File:** `/system/netlify/functions/track-tape.mjs` (NEW)

```javascript
// Track Tape, 25.10.17
// POST: Create a new record in the database for a user uploaded tape

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { customAlphabet } from 'nanoid';

const dev = process.env.CONTEXT === "dev";

// Code generator (same pattern as track-media)
const consonants = 'bcdfghjklmnpqrstvwxyz' + 'bcdfghjklmnpqrstvwxyz' + 'BCDFGHJKLMNPQRSTVWXYZ';
const vowels = 'aeiou' + 'aeiou' + 'AEIOU';
const numbers = '23456789';
const alphabet = consonants + vowels + numbers;
const CODE_LENGTH = 3;
const nanoid = customAlphabet(alphabet, CODE_LENGTH);
const MAX_COLLISION_ATTEMPTS = 100;

async function generateUniqueCode(collection) {
  for (let attempt = 0; attempt < MAX_COLLISION_ATTEMPTS; attempt++) {
    const code = nanoid();
    const existing = await collection.findOne({ code });
    if (!existing) return code;
    console.log(`⚠️  Tape code collision: ${code}, retrying...`);
  }
  const longerNanoid = customAlphabet(alphabet, CODE_LENGTH + 1);
  const longerCode = longerNanoid();
  console.log(`⚠️  Max collisions reached, using longer code: ${longerCode}`);
  return longerCode;
}

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  try {
    const body = JSON.parse(event.body);
    
    // Try to authorize (optional for guests)
    let user;
    try {
      user = await authorize(event.headers);
    } catch (authError) {
      console.log(`🔓 Guest tape upload: ${authError.message}`);
    }

    const database = await connect();
    const tapes = database.db.collection("tapes");
    
    // Create indexes
    await tapes.createIndex({ code: 1 }, { unique: true });
    await tapes.createIndex({ user: 1 }, { sparse: true });
    await tapes.createIndex({ when: 1 });
    await tapes.createIndex({ slug: 1 });
    
    if (user) {
      await tapes.createIndex({ slug: 1, user: 1 }, { unique: true });
    }

    // Generate unique code
    const code = await generateUniqueCode(tapes);
    
    try {
      const slug = body.slug;
      const metadata = body.metadata || {};
      
      const record = {
        code,
        slug,
        when: new Date(),
        bucket: user ? "user-aesthetic-computer" : "art-aesthetic-computer",
        nuked: false,
      };
      
      // Add user field only if authenticated
      if (user) {
        record.user = user.sub;
      }
      
      await tapes.insertOne(record);
      
      const logType = user ? "user" : "guest";
      console.log(`✅ Created ${logType} tape: slug=${slug}, code=${code}`);
      
      // Queue MP4 conversion job (async - happens in background)
      queueMP4Conversion({
        mongoId: record._id.toString(),
        bucket: record.bucket,
        slug: record.slug,
        callbackUrl: `${process.env.URL}/.netlify/functions/tape-mp4-complete`,
      }).catch(err => console.error('❌ MP4 conversion queue error:', err));
      
      return respond(200, { slug, code, status: "posted" });
      
    } catch (error) {
      console.error(`❌ Failed to insert tape:`, error);
      return respond(500, { message: error.message || String(error) });
    } finally {
      await database.disconnect();
    }
    
  } catch (error) {
    console.error(`❌ Track tape error:`, error);
    return respond(400, { message: error.message || "Cannot parse input body." });
  }
}

// Queue MP4 conversion (trigger Netlify function or external service)
async function queueMP4Conversion(tapeId, slug, user) {
  try {
    // Call Netlify background function or external service
    const response = await fetch("/api/convert-tape-to-mp4", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ tapeId, slug, user: user?.sub }),
    });
    
    const data = await response.json();
    console.log(`🎬 MP4 conversion queued: ${data.jobId}`);
    
  } catch (error) {
    console.error(`⚠️  Failed to queue MP4 conversion:`, error);
    // Non-blocking - tape is still saved
  }
}
```

**Add to `netlify.toml`:**

```toml
[functions.track-tape]
included_env_vars = ["CONTEXT", "ADMIN_SUB", "AUTH0_M2M_CLIENT_ID", "AUTH0_M2M_SECRET", "MONGODB_CONNECTION_STRING", "MONGODB_NAME"]

[[redirects]]
from = "/api/track-tape"
to = "/.netlify/functions/track-tape"
status = 200
force = false
```

---

### Phase 4: ATProto Integration

**File:** `/system/backend/tape-atproto.mjs` (NEW)

```javascript
// tape-atproto.mjs
### `/system/backend/tape-atproto.mjs`

Create ATProto record for tape (called AFTER MP4 conversion completes):

```javascript
import { AtpAgent } from '@atproto/api';
import fetch from 'node-fetch';
import { ObjectId } from 'mongodb';

export async function createTapeOnAtproto(mongoId, mp4Url) {
  const agent = new AtpAgent({ service: 'https://bsky.social' });
  
  await agent.login({
    identifier: process.env.ATPROTO_IDENTIFIER,
    password: process.env.ATPROTO_PASSWORD,
  });

  // Fetch tape data from MongoDB
  const { connect } = await import('./database.mjs');
  const database = await connect();
  const tapeData = await database.db.collection('tapes').findOne({ _id: new ObjectId(mongoId) });
  
  if (!tapeData) {
    throw new Error(`Tape not found: ${mongoId}`);
  }
  
  try {
    // Download MP4 and upload as blob to ATProto
    const mp4Response = await fetch(mp4Url);
    const mp4Buffer = await mp4Response.arrayBuffer();
    
    const uploadRes = await agent.uploadBlob(new Uint8Array(mp4Buffer), {
      encoding: 'video/mp4',
    });
    
    const videoBlob = uploadRes.data.blob;
    
    // Create minimal ATProto record (follows mood.json pattern)
    const record = {
      $type: 'computer.aesthetic.tape',
      slug: tapeData.slug,
      code: tapeData.code,
      when: tapeData.when.toISOString(),
      video: videoBlob,  // MP4 blob
      ref: mongoId,      // Bidirectional reference
    };

    const response = await agent.com.atproto.repo.createRecord({
      repo: agent.session.did,
      collection: 'computer.aesthetic.tape',
      record,
    });

    // Update MongoDB with ATProto info (using `at` field like paintings)
    await database.db.collection('tapes').updateOne(
      { _id: new ObjectId(mongoId) },
      {
        $set: {
          at: {
            rkey: response.uri.split('/').pop(),
            uri: response.uri,
            cid: response.cid,
          },
        },
      },
    );

    await database.disconnect();
    
    console.log(`✅ ATProto record created for tape: ${mongoId}`);
    return response;
    
  } catch (error) {
    await database.disconnect();
    console.error('❌ Failed to create ATProto record:', error);
    throw error;
  }
}
```
    
    return {
      success: true,
      rkey,
      uri: result.uri,
      cid: result.cid,
      videoBlob: videoBlob,
    };
    
  } catch (error) {
    console.error('❌ ATProto tape creation failed:', error);
    return {
      success: false,
      error: error.message,
    };
  }
}

// NOTE: updateTapeOnAtproto() is NOT needed
// We create the ATProto record only once, after MP4 is ready
// No updates needed because we wait for all data before creating
```

**DON'T sync with ATProto yet in `track-tape.mjs`:**

```javascript
// After MongoDB insert:
// NOTE: ATProto sync happens AFTER MP4 conversion completes
// See tape-mp4-complete.mjs for ATProto record creation

const record = {
  code,
  slug,
  // ... other fields ...
  atprotoStatus: user ? "pending" : null,  // Will sync after MP4 ready
};

await tapes.insertOne(record);
console.log(`✅ Tape saved, MP4 conversion queued, ATProto sync pending...`);
```

---

### Phase 5: MP4 Conversion Service

**Option A: Netlify Background Function** (Limited to 10s execution)

**File:** `/system/netlify/functions/convert-tape-to-mp4.mjs` (NEW)

```javascript
// Convert Tape to MP4
// Background function - triggers immediately but can't handle long conversions

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  try {
    const { tapeId, slug, user } = JSON.parse(event.body);
    
    const database = await connect();
    const tapes = database.db.collection("tapes");
    
    // Update status to processing
    await tapes.updateOne(
      { _id: new ObjectId(tapeId) },
      { 
        $set: { 
          mp4Status: "processing",
          mp4JobId: context.requestId,
        }
      }
    );
    
    // For Netlify: Queue to external service (FFmpeg.wasm too slow)
    // Trigger webhook to external microservice or serverless function
    const conversionUrl = process.env.MP4_CONVERSION_SERVICE_URL;
    
    if (conversionUrl) {
      await fetch(conversionUrl, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          tapeId,
          slug,
          bucket: user ? "user-aesthetic-computer" : "art-aesthetic-computer",
          zipUrl: `https://${bucket}.nyc3.digitaloceanspaces.com/${slug}.zip`,
          callbackUrl: `https://aesthetic.computer/api/tape-mp4-complete`,
        }),
      });
    }
    
    await database.disconnect();
    
    return respond(200, { 
      jobId: context.requestId,
      status: "queued",
    });
    
  } catch (error) {
    console.error("MP4 conversion error:", error);
    return respond(500, { message: error.message });
  }
}
```

**Option B: External Microservice** (Recommended for long conversions)

**New Service:** `video-converter-service` (Separate repo/deployment)

```javascript
// video-converter/index.mjs
// Standalone Node.js service with FFmpeg

import express from 'express';
import fetch from 'node-fetch';
import JSZip from 'jszip';
import { exec } from 'child_process';
import { promisify } from 'util';
import fs from 'fs/promises';
import path from 'path';

const execAsync = promisify(exec);
const app = express();
app.use(express.json());

app.post('/convert', async (req, res) => {
  const { tapeId, slug, bucket, zipUrl, callbackUrl } = req.body;
  
  // Respond immediately
  res.json({ status: 'processing', tapeId });
  
  // Process in background
  processConversion({ tapeId, slug, bucket, zipUrl, callbackUrl })
    .catch(err => console.error('Conversion error:', err));
});

async function processConversion({ tapeId, slug, bucket, zipUrl, callbackUrl }) {
  const workDir = `/tmp/tape-${tapeId}`;
  await fs.mkdir(workDir, { recursive: true });
  
  try {
    // 1. Download ZIP
    const zipResponse = await fetch(zipUrl);
    const zipBuffer = await zipResponse.arrayBuffer();
    
    // 2. Extract frames
    const zip = new JSZip();
    const contents = await zip.loadAsync(zipBuffer);
    const timing = JSON.parse(await contents.file('timing.json').async('text'));
    
    // Extract all frame PNGs
    for (const file of Object.values(contents.files)) {
      if (file.name.endsWith('.png')) {
        const data = await file.async('nodebuffer');
        await fs.writeFile(path.join(workDir, file.name), data);
      }
    }
    
    // 3. Create concat file for FFmpeg
    const concatContent = timing.map(t => 
      `file '${t.filename}'\nduration ${t.duration / 1000}`
    ).join('\n');
    await fs.writeFile(path.join(workDir, 'concat.txt'), concatContent);
    
    // 4. Run FFmpeg
    const outputPath = path.join(workDir, 'output.mp4');
    await execAsync(`ffmpeg -f concat -safe 0 -i ${workDir}/concat.txt -c:v libx264 -pix_fmt yuv420p ${outputPath}`);
    
    // 5. Upload MP4 to S3/DO Spaces
    const mp4Buffer = await fs.readFile(outputPath);
    const uploadUrl = `https://${bucket}.nyc3.digitaloceanspaces.com/${slug}.mp4`;
    
    // Upload to DO Spaces (needs credentials)
    // ... S3 upload logic ...
    
    // 6. Notify completion
    await fetch(callbackUrl, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        tapeId,
        mp4Url: uploadUrl,
        status: 'complete',
      }),
    });
    
  } catch (error) {
    // Notify error
    await fetch(callbackUrl, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        tapeId,
        status: 'error',
        error: error.message,
      }),
    });
  } finally {
    // Cleanup
    await fs.rm(workDir, { recursive: true, force: true });
  }
}

app.listen(3000, () => console.log('Video converter running on port 3000'));
```

### `/system/netlify/functions/tape-mp4-complete.mjs`

Webhook endpoint called when MP4 conversion completes. Creates ATProto record:

```javascript
import { respond } from "../../backend/http.mjs";
import { createTapeOnAtproto } from "../../backend/tape-atproto.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  try {
    const { mongoId, mp4Url, status } = JSON.parse(event.body);
    
    if (status !== "complete") {
      console.error(`❌ MP4 conversion failed for: ${mongoId}`);
      return respond(400, { message: "Conversion failed" });
    }

    // Create ATProto record with MP4 blob
    // This also updates MongoDB with `at` field
    await createTapeOnAtproto(mongoId, mp4Url);

    console.log(`✅ Tape MP4 complete and ATProto synced: ${mongoId}`);
    return respond(200, { message: "Success" });
    
  } catch (error) {
    console.error('❌ Tape MP4 completion error:', error);
    return respond(500, { message: error.message });
  }
}
```

---

## 📋 Implementation Checklist

### Phase 1: Frontend ✅
- [ ] Add `postBtn` button to `video.mjs`
- [ ] Add `isPostingTape` state variable
- [ ] Implement Post button action handler
- [ ] Add message handlers for `tape:posted` and `tape:post-error`
- [ ] Test UI layout and button interactions

### Phase 2: BIOS Upload ✅
- [ ] Add `create-and-post-tape` message handler to `bios.mjs`
- [ ] Implement ZIP creation with frames, timing.json, metadata.json
- [ ] Modify `receivedUpload` to handle tape metadata
- [ ] Test ZIP creation and upload flow

### Phase 3: Backend ✅
- [ ] Create `/system/netlify/functions/track-tape.mjs`
- [ ] Implement code generation (same pattern as paintings)
- [ ] Create MongoDB indexes for tapes collection
- [ ] Add netlify.toml configuration
- [ ] Test database insertion

### Phase 4: ATProto ✅
- [ ] Create lexicon `/at/lexicons/computer/aesthetic/tape.json`
- [ ] Update lexicon: `video` blob instead of `thumbnail`
- [ ] Create `/system/backend/tape-atproto.mjs`
- [ ] Implement `createTapeOnAtproto()` function (requires mp4Url param)
- [ ] Remove `updateTapeOnAtproto()` function (not needed - create after MP4 ready)
- [ ] Update `tape-mp4-complete.mjs` to create ATProto record
- [ ] Test ATProto record creation with MP4 blob

### Phase 5: MP4 Conversion ✅
- [ ] **Option A:** Create Netlify background function (limited)
  - [ ] `/system/netlify/functions/convert-tape-to-mp4.mjs`
  - [ ] Queue to external service
- [ ] **Option B:** Deploy external microservice (recommended)
  - [ ] Create `video-converter-service` repo
  - [ ] Implement FFmpeg conversion
  - [ ] Deploy to DigitalOcean/Railway/Fly.io
- [ ] Create completion callback `/api/tape-mp4-complete`
- [ ] Test end-to-end conversion flow

### Phase 6: Testing & Polish ✅
- [ ] Test guest upload flow (anonymous)
- [ ] Test user upload flow (authenticated)
- [ ] Test ATProto sync for users with accounts
- [ ] Test MP4 conversion queue
- [ ] Add error handling and retry logic
- [ ] Add progress indicators
- [ ] Update UI to show tape code (#xyz)

---

## 🔐 Environment Variables

Add to Netlify:

```bash
# Existing (should already be set)
MONGODB_CONNECTION_STRING=mongodb+srv://...
MONGODB_NAME=aesthetic
PDS_URL=https://at.aesthetic.computer
PDS_ADMIN_PASSWORD=...

# New for MP4 conversion
MP4_CONVERSION_SERVICE_URL=https://video-converter.yourservice.com/convert
MP4_CONVERSION_API_KEY=secret-key-here
```

---

## 📊 Database Indexes

```javascript
// tapes collection indexes (same as paintings)
db.tapes.createIndex({ code: 1 }, { unique: true });
db.tapes.createIndex({ user: 1 }, { sparse: true });
db.tapes.createIndex({ when: 1 });
db.tapes.createIndex({ slug: 1 });
db.tapes.createIndex({ slug: 1, user: 1 }, { unique: true, sparse: true });
```

---

## 🚀 Deployment Steps

1. **Deploy ATProto Lexicon**
   ```bash
   cd /workspaces/aesthetic-computer/at
   node scripts/deploy-tape-lexicon.mjs
   ```

2. **Deploy Netlify Functions**
   ```bash
   cd /workspaces/aesthetic-computer/system
   git add netlify/functions/track-tape.mjs
   git add netlify/functions/tape-mp4-complete.mjs
   git add netlify.toml
   git commit -m "Add tape posting functions"
   git push
   ```

3. **Deploy Video Converter Service** (if using external service)
   ```bash
   # Deploy to DigitalOcean App Platform, Railway, or Fly.io
   ```

4. **Update Frontend**
   ```bash
   git add system/public/aesthetic.computer/disks/video.mjs
   git add system/public/aesthetic.computer/bios.mjs
   git commit -m "Add Post button to video piece"
   git push
   ```

---

## ✅ Implementation Checklist

### Phase 1: Frontend (video.mjs)
- [ ] Add POST button UI element (line ~237)
- [ ] Add POST button action handler (line ~630)
- [ ] Add state variables: `postBtn`, `isPostingTape`
- [ ] Handle `tape:posted` callback message
- [ ] Handle `tape:post-error` callback message

### Phase 2: BIOS (bios.mjs)
- [ ] Add `create-and-post-tape` message handler (after line 4950)
- [ ] Reuse existing ZIP creation logic with 6x scaling
- [ ] Add metadata.json with piece, frames, duration, fps, resolution
- [ ] Modify `receivedUpload` to accept tape metadata parameter
- [ ] Call Netlify function with tape metadata

### Phase 3: Backend - MongoDB (track-tape.mjs)
- [ ] Create new Netlify function at `/system/netlify/functions/track-tape.mjs`
- [ ] Copy code generation logic from track-media.mjs
- [ ] Create `tapes` collection with indexes
- [ ] Insert record: `{ code, slug, when, bucket, user, nuked }`
- [ ] Queue MP4 conversion (non-blocking)
- [ ] Return `{ code, slug }` to client

### Phase 4: Backend - ATProto (AFTER MP4 conversion)
- [ ] Create ATProto lexicon at `/at/lexicons/computer/aesthetic/tape.json`
- [ ] Create `/system/backend/tape-atproto.mjs`
- [ ] Implement `createTapeOnAtproto(mongoId, mp4Url)`
  - [ ] Fetch tape from MongoDB
  - [ ] Download MP4 from URL
  - [ ] Upload MP4 as blob to ATProto
  - [ ] Create ATProto record with video blob
  - [ ] Update MongoDB with `at: { rkey, uri, cid }`
- [ ] Create `/system/netlify/functions/tape-mp4-complete.mjs`
  - [ ] Receive webhook: `{ mongoId, mp4Url, status }`
  - [ ] Call `createTapeOnAtproto()` to sync to ATProto
  - [ ] Return success response

### Phase 5: Video Conversion
- [ ] Choose implementation: Netlify background function OR external microservice
- [ ] Implement ZIP → MP4 conversion with FFmpeg
- [ ] Upload MP4 to S3/DO Spaces
- [ ] POST callback to tape-mp4-complete.mjs with `{ mongoId, mp4Url, status: "complete" }`

### Phase 6: Testing
- [ ] Test authenticated user tape post
- [ ] Test guest tape post (no ATProto)
- [ ] Test ATProto sync after MP4 completion
- [ ] Test code lookup (#xyz)
- [ ] Test error handling (upload fails, MP4 fails, ATProto fails)
- [ ] Verify existing ZIP/GIF/MP4 buttons still work

### Phase 7: Deployment
- [ ] Deploy lexicon to ATProto PDS
- [ ] Deploy Netlify functions
- [ ] Deploy video conversion service
- [ ] Update environment variables
- [ ] Monitor logs for first production tape post

---

## 🎯 Success Criteria

- ✅ Users can click "Post" on video piece
- ✅ ZIP of tape frames uploads to cloud storage
- ✅ MongoDB record created with unique code
- ✅ MP4 conversion completes successfully
- ✅ ATProto record created AFTER MP4 with video blob (for authenticated users)
- ✅ User receives shareable code (#xyz) immediately
- ✅ Guest uploads work without authentication (no ATProto)
- ✅ Existing ZIP/GIF/MP4 buttons continue to work

---

## 📊 Final Schema Summary

### **Complete MongoDB Records**

```javascript
// paintings collection (track-media.mjs:99-106)
{
  _id: ObjectId("..."),
  code: "xyz",                    // 3-char code
  slug: "line-2025.10.17...",     // timestamp-based
  user: "auth0|123",              // optional (undefined for guests)
  when: ISODate("..."),
  bucket: "user-aesthetic-computer",
  nuked: false
}

// moods collection (mood.mjs:107-111, updated at 123-128)
{
  _id: ObjectId("..."),
  user: "auth0|123",              // required
  mood: "feeling great!",
  when: ISODate("..."),
  atproto: {                      // added after ATProto sync
    rkey: "3labc123"
  },
  deleted: false                  // soft delete
}

// tapes collection (NEW - track-tape.mjs:104, updated by tape-atproto.mjs:48)
{
  _id: ObjectId("..."),
  code: "xyz",                    // 3-char code
  slug: "wand-2025.10.17...",     // piece-timestamp
  user: "auth0|123",              // optional (undefined for guests)
  when: ISODate("..."),
  bucket: "user-aesthetic-computer",
  at: {                           // added AFTER MP4 + ATProto complete
    rkey: "3lxyz789",
    uri: "at://did:plc:.../computer.aesthetic.tape/3lxyz789",
    cid: "bafyrei..."
  },
  nuked: false
}
```

### **ATProto Record Structure**

```javascript
// computer.aesthetic.mood (mood-atproto.mjs:44-53)
{
  $type: "computer.aesthetic.mood",
  mood: "feeling great!",
  when: "2025-10-17T12:30:00.123Z",
  ref: "67117c4d..."             // MongoDB _id
}

// computer.aesthetic.tape (tape-atproto.mjs:31-42)
{
  $type: "computer.aesthetic.tape",
  slug: "wand-2025.10.17...",
  code: "xyz",
  when: "2025-10-17T12:30:00.123Z",
  video: {                       // BLOB uploaded via agent.uploadBlob()
    $type: "blob",
    ref: { $link: "bafkreiabcdef..." },
    mimeType: "video/mp4",
    size: 5242880
  },
  ref: "67117c4d..."             // MongoDB _id
}
```

### **URL Construction Patterns**

```javascript
// Paintings/Tapes ZIP URL (constructed on-demand, not stored)
const zipUrl = `https://${bucket}.nyc3.digitaloceanspaces.com/${slug}.zip`;

// Tapes MP4 URL (constructed on-demand, not stored)
const mp4Url = `https://${bucket}.nyc3.digitaloceanspaces.com/${slug}.mp4`;

// Lookup by code
const tape = await db.collection('tapes').findOne({ code: 'xyz' });
const url = `https://${tape.bucket}.nyc3.digitaloceanspaces.com/${tape.slug}.zip`;
```

### **Implementation Pattern Comparison**

| Aspect | Paintings | Moods | Tapes |
|--------|-----------|-------|-------|
| **Collection** | `paintings` | `moods` | `tapes` |
| **Unique ID** | `code` (3-char) | N/A | `code` (3-char) |
| **Timestamp ID** | `slug` | N/A | `slug` |
| **Content ID** | N/A | `mood` (text) | (ZIP file) |
| **User Field** | `user?` (optional) | `user` (required) | `user?` (optional) |
| **Guest Support** | ✅ Yes | ❌ No | ✅ Yes |
| **ATProto Sync** | ❌ None | ✅ Immediate | ✅ Deferred |
| **ATProto Field** | N/A | `atproto.rkey` | `at.{rkey,uri,cid}` |
| **ATProto Blob** | N/A | ❌ None | ✅ MP4 video |
| **Response Time** | Instant | Instant | Instant (MP4 async) |
| **File Location** | `/track-media.mjs` | `/mood.mjs` | `/track-tape.mjs` |
| **ATProto Helper** | N/A | `/mood-atproto.mjs` | `/tape-atproto.mjs` |

### **Code References (Line Numbers)**

| Operation | Paintings | Moods | Tapes |
|-----------|-----------|-------|-------|
| Insert MongoDB | `track-media.mjs:104` | `mood.mjs:107` | `track-tape.mjs:104` |
| Create ATProto | N/A | `mood.mjs:114` | `tape-mp4-complete.mjs:23` |
| Update MongoDB | N/A | `mood.mjs:123` | `tape-atproto.mjs:48` |
| Return Response | `track-media.mjs:109` | `mood.mjs:146` | `track-tape.mjs:115` |
| ATProto Login | N/A | `mood-atproto.mjs:38` | `tape-atproto.mjs:TBD` |
| ATProto Create | N/A | `mood-atproto.mjs:44` | `tape-atproto.mjs:TBD` |

---

## 🔄 Execution Flow Summary

### **Complete Architecture Diagram**

```
┌─────────────────────────────────────────────────────────────────────┐
│                         USER INTERACTION                            │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                    [User clicks "Post" button]
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│  video.mjs:630 - Create frame data array from recorded tape        │
│  → Sends message: { type: "create-and-post-tape", content: {...} } │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│  bios.mjs:4950+ - Create ZIP file with 6x scaled frames            │
│  → Add timing.json (frame durations)                               │
│  → Add metadata.json (piece, frames, duration, fps, resolution)    │
│  → Call receivedUpload({ filename, data, metadata })               │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│  PRESIGNED S3 UPLOAD (existing flow)                                │
│  → Upload ZIP to user-aesthetic-computer/slug.zip                   │
│  → POST to /api/track-tape with { slug, ext: "zip", metadata }     │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│  track-tape.mjs:80-120 - NETLIFY FUNCTION                          │
│                                                                      │
│  :104  collection.insertOne({                                       │
│           code: "xyz",                                              │
│           slug: "wand-2025.10.17...",                               │
│           user: "auth0|123" || undefined,                           │
│           when: new Date(),                                         │
│           bucket: "user-aesthetic-computer",                        │
│           nuked: false                                              │
│        })                                                           │
│                                                                      │
│  :110  queueMP4Conversion({ mongoId, bucket, slug, callbackUrl })  │
│        ↳ Fire-and-forget async call                                │
│                                                                      │
│  :115  return respond(200, { code: "xyz", slug })                  │
│        ✅ USER GETS IMMEDIATE RESPONSE                              │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                    ┌──────────────┴──────────────┐
                    │                             │
                    ▼                             ▼
    ┌────────────────────────┐     ┌────────────────────────────┐
    │  FOREGROUND (Done)     │     │  BACKGROUND (Continues)    │
    │                        │     │                            │
    │  video.mjs receives    │     │  MP4 Conversion Service    │
    │  tape:posted callback  │     │  (5-10 minutes)            │
    │                        │     │                            │
    │  Shows: "POSTED! #xyz" │     │  1. Download ZIP           │
    │                        │     │  2. Extract frames         │
    │  User can continue     │     │  3. Run FFmpeg             │
    │  using app ✅          │     │  4. Upload MP4 to S3       │
    └────────────────────────┘     │  5. POST to callback       │
                                    └────────────────────────────┘
                                                  │
                                                  ▼
┌─────────────────────────────────────────────────────────────────────┐
│  tape-mp4-complete.mjs:15-30 - WEBHOOK HANDLER                     │
│                                                                      │
│  :15  Receive POST: { mongoId, mp4Url, status: "complete" }        │
│                                                                      │
│  :23  await createTapeOnAtproto(mongoId, mp4Url)                   │
│       ↓                                                             │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│  tape-atproto.mjs:15-60 - ATPROTO SYNC                             │
│                                                                      │
│  :15  const tapeData = await tapes.findOne({ _id: mongoId })       │
│                                                                      │
│  :23  const mp4Response = await fetch(mp4Url)                      │
│  :26  const uploadRes = await agent.uploadBlob(mp4Buffer)          │
│       → MP4 uploaded to ATProto as blob                            │
│                                                                      │
│  :31  await agent.createRecord({                                    │
│         collection: 'computer.aesthetic.tape',                      │
│         record: {                                                   │
│           $type: 'computer.aesthetic.tape',                         │
│           slug: tapeData.slug,                                      │
│           code: tapeData.code,                                      │
│           when: tapeData.when.toISOString(),                        │
│           video: videoBlob,  // ← MP4 blob                          │
│           ref: mongoId                                              │
│         }                                                           │
│       })                                                            │
│                                                                      │
│  :48  await tapes.updateOne(                                        │
│         { _id: mongoId },                                           │
│         { $set: { at: { rkey, uri, cid } } }                        │
│       )                                                             │
│       ✅ ATPROTO SYNC COMPLETE                                      │
└─────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
                    ┌──────────────────────────┐
                    │  FINAL MONGODB RECORD    │
                    │                          │
                    │  {                       │
                    │    code: "xyz",          │
                    │    slug: "wand-2025...", │
                    │    user: "auth0|123",    │
                    │    when: ISODate(...),   │
                    │    bucket: "user-...",   │
                    │    at: {                 │
                    │      rkey: "3lxyz789",   │
                    │      uri: "at://...",    │
                    │      cid: "bafyrei..."   │
                    │    },                    │
                    │    nuked: false          │
                    │  }                       │
                    └──────────────────────────┘
```

### Updated Flow (ATProto AFTER MP4):

1. **User clicks "Post"** → ZIP created from frames
2. **ZIP uploaded** to S3/DO Spaces → MongoDB record created
3. **User gets #code** immediately (tape is "posted")
4. **MP4 conversion queued** (background process)
5. **MP4 completes** → Uploaded to S3/DO Spaces
6. **ATProto record created** → MP4 blob uploaded to PDS
7. **MongoDB updated** with ATProto references

### **Three Pattern Comparison**

```
PAINTINGS FLOW (MongoDB Only - No ATProto)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
User uploads PNG
    ↓
track-media.mjs:104 → insertOne({ code, slug, when, bucket, user })
    ↓
track-media.mjs:109 → respond(200, { code, slug })
    ↓
✅ DONE (No ATProto)


MOODS FLOW (Immediate ATProto Dual-Write)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
User posts mood text
    ↓
mood.mjs:107 → insertOne({ user, mood, when })
    ↓
mood.mjs:114 → createMoodOnAtproto(database, user, mood, date, mongoId)
    ├─ mood-atproto.mjs:38 → agent.login()
    ├─ mood-atproto.mjs:44 → agent.createRecord({ mood, when, ref })
    └─ mood-atproto.mjs:61 → return { rkey, uri }
    ↓
mood.mjs:123 → updateOne({ $set: { atproto: { rkey } } })
    ↓
mood.mjs:146 → respond(200, { mood })
    ↓
✅ DONE (MongoDB + ATProto in one request)


TAPES FLOW (Deferred ATProto After MP4)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
User posts tape ZIP
    ↓
track-tape.mjs:104 → insertOne({ code, slug, when, bucket, user })
    ↓
track-tape.mjs:110 → queueMP4Conversion(...) [async, fire-and-forget]
    ↓
track-tape.mjs:115 → respond(200, { code, slug })
    ↓
✅ USER DONE (immediate response)

[BACKGROUND: 5-10 minutes later]
    ↓
MP4 conversion completes
    ↓
tape-mp4-complete.mjs:15 → Receive webhook callback
    ↓
tape-mp4-complete.mjs:23 → createTapeOnAtproto(mongoId, mp4Url)
    ├─ tape-atproto.mjs:15 → findOne({ _id: mongoId })
    ├─ tape-atproto.mjs:23 → fetch(mp4Url) + download MP4
    ├─ tape-atproto.mjs:26 → agent.uploadBlob(mp4Buffer) → video blob
    ├─ tape-atproto.mjs:31 → agent.createRecord({ slug, code, when, video, ref })
    └─ tape-atproto.mjs:48 → updateOne({ $set: { at: { rkey, uri, cid } } })
    ↓
✅ ATPROTO DONE (background, user unaware)
```

### Why This Order?

- ✅ **Immediate feedback:** User gets code right away
- ✅ **MP4 as blob:** ATProto gets the actual video, not placeholder
- ✅ **Non-blocking:** Frontend doesn't wait for MP4 conversion
- ✅ **Reliable:** ATProto sync only happens when MP4 exists
- ✅ **Guest-friendly:** Guests get instant upload, no ATProto needed

---

## 📝 Future Enhancements

1. **Tape Gallery:** Browse tapes by #code or user
2. **Tape Playback:** Load and play tapes from #code
3. **Tape Feed:** ATProto feed of shared tapes
4. **Thumbnail Generation:** First frame as preview
5. **Tape Editing:** Trim, combine, or remix tapes
6. **Social Sharing:** Direct links to tapes
7. **Tape Metadata:** Tags, descriptions, piece info

---

## 📚 Reference Files

### Existing Patterns
- **Paintings Upload:** `/system/netlify/functions/track-media.mjs`
- **Moods ATProto:** `/at/lexicons/computer/aesthetic/mood.json`
- **Upload Flow:** `/system/public/aesthetic.computer/bios.mjs` (lines 12030-12200)
- **ZIP Export:** `/system/public/aesthetic.computer/bios.mjs` (lines 4861-4950)

### New Files to Create
- `/system/public/aesthetic.computer/disks/video.mjs` (modify)
- `/system/public/aesthetic.computer/bios.mjs` (modify)
- `/system/netlify/functions/track-tape.mjs`
- `/system/netlify/functions/tape-mp4-complete.mjs`
- `/system/backend/tape-atproto.mjs`
- `/at/lexicons/computer/aesthetic/tape.json`
- External: `video-converter-service/` (if using microservice)

---

**Status:** 📝 Ready for implementation  
**Estimated Time:** 2-3 days for core features, 1 week with MP4 service  
**Next Step:** Create ATProto lexicon and start with Phase 1 (Frontend)
