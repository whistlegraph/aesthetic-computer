# 🦋 Moods → Bluesky Mirror & Permalinks Plan

**Created:** January 28, 2026  
**Status:** ✅ Implementation Complete

---

## 📖 Overview

This plan covers three interconnected features:

1. **Bluesky Mirroring** - Auto-post @jeffrey's moods (or any moods) to the `@aesthetic.computer` Bluesky account
2. **Mood Permalinks** - View individual moods at `moods~@handle~<rkey>` URLs
3. **Engagement Display** - Show likes and replies from Bluesky on mood permalinks

---

## ✅ Implementation Summary

### Files Created
- [system/backend/bluesky-mirror.mjs](../system/backend/bluesky-mirror.mjs) - Bluesky posting functions
- [system/backend/bluesky-engagement.mjs](../system/backend/bluesky-engagement.mjs) - Fetch likes/reposts/replies

### Files Modified
- [system/netlify/functions/mood.mjs](../system/netlify/functions/mood.mjs) - Added Bluesky mirroring hook + `/api/mood/single` endpoint
- [system/backend/database.mjs](../system/backend/database.mjs) - Added `getMoodByRkey()` function
- [system/public/aesthetic.computer/disks/moods.mjs](../system/public/aesthetic.computer/disks/moods.mjs) - Added single mood permalink view

### Environment Variables Needed
Add to `system/.env`:
```
BSKY_IDENTIFIER=aesthetic.computer
BSKY_APP_PASSWORD=<from vault/at/.env>
BSKY_MIRROR_HANDLES=@jeffrey  # comma-separated list of handles to mirror
```

---

## 🔮 Future: ATProto Federation for Reactions

**Idea:** Allow external ATProto/Bluesky users to react to AC content through our PDS.

### Concept
```
External Bluesky user → discovers AC mood via federation
                      → creates reaction record on their PDS
                      → AC PDS receives via firehose/subscription
                      → AC displays alongside Bluesky engagement
```

### Potential Lexicon: `computer.aesthetic.reaction`
```json
{
  "lexicon": 1,
  "id": "computer.aesthetic.reaction",
  "defs": {
    "main": {
      "type": "record",
      "description": "A reaction to AC content from any ATProto user",
      "key": "tid",
      "record": {
        "type": "object",
        "required": ["subject", "type"],
        "properties": {
          "subject": {
            "type": "ref",
            "ref": "com.atproto.repo.strongRef",
            "description": "Reference to the AC content (mood, painting, etc.)"
          },
          "type": {
            "type": "string",
            "enum": ["like", "love", "fire", "think"],
            "description": "Type of reaction"
          },
          "comment": {
            "type": "string",
            "maxLength": 500,
            "description": "Optional comment with the reaction"
          }
        }
      }
    }
  }
}
```

### Implementation Steps (Future)
1. Create reaction lexicon on PDS
2. Set up firehose subscription to watch for reactions
3. Store reactions in MongoDB linked to content
4. Display alongside Bluesky engagement
5. Allow AC users to react from within AC (creates record on their ATProto account)

This would make AC content truly federated - anyone on ATProto could interact!

---

## 🏗️ Current Architecture

### Existing Infrastructure

| Component | Location | Description |
|-----------|----------|-------------|
| **Moods Lexicon** | [at/lexicons/computer/aesthetic/mood.json](../at/lexicons/computer/aesthetic/mood.json) | ATProto schema with `mood`, `when`, `ref` fields |
| **Mood API** | [system/netlify/functions/mood.mjs](../system/netlify/functions/mood.mjs) | POST/GET moods, dual-writes to MongoDB + ATProto |
| **ATProto Helper** | [system/backend/mood-atproto.mjs](../system/backend/mood-atproto.mjs) | `createMoodOnAtproto()`, `deleteMoodFromAtproto()` |
| **Moods Piece** | [system/public/aesthetic.computer/disks/moods.mjs](../system/public/aesthetic.computer/disks/moods.mjs) | Display all moods with filtering via `?for=@handle` |
| **Mood Piece** | [system/public/aesthetic.computer/disks/mood.mjs](../system/public/aesthetic.computer/disks/mood.mjs) | Single mood display (for posting) |
| **Bluesky Poster** | [at/post-to-bluesky.mjs](../at/post-to-bluesky.mjs) | CLI tool for posting to Bluesky |
| **Bluesky Creds** | [aesthetic-computer-vault/at/.env](../aesthetic-computer-vault/at/.env) | `BSKY_IDENTIFIER=aesthetic.computer`, `BSKY_APP_PASSWORD` |

### Current Mood Data Flow

```
User posts mood → mood.mjs API
                      ↓
            ┌─────────┴─────────┐
            ↓                   ↓
        MongoDB             ATProto PDS
    (primary store)     (at.aesthetic.computer)
            ↓                   ↓
    { mood, when,         computer.aesthetic.mood
      user, atproto:      { mood, when, ref }
      { rkey } }                ↓
                          Returns rkey
                                ↓
                     MongoDB updated with rkey
```

---

## 🎯 Feature 1: Bluesky Mirroring

### Goal
When @jeffrey (or configured handles) posts a mood, automatically cross-post to Bluesky as `@aesthetic.computer`.

### Implementation Approach

#### Option A: Hook into Existing Dual-Write (Recommended)
Add Bluesky posting after ATProto sync in `mood.mjs`:

```javascript
// After ATProto sync succeeds in mood.mjs (line ~230)
if (atprotoResult.rkey) {
  // ... existing rkey update ...
  
  // NEW: Mirror to Bluesky for @jeffrey
  if (handle === '@jeffrey') {
    try {
      await postMoodToBluesky(mood, handle, atprotoResult.rkey);
    } catch (bskyError) {
      console.error("⚠️ Failed to mirror to Bluesky:", bskyError);
      // Don't fail the request - mood is already saved
    }
  }
}
```

#### New File: `system/backend/bluesky-mirror.mjs`

```javascript
// bluesky-mirror.mjs
// Mirror moods to Bluesky @aesthetic.computer account

import { BskyAgent } from '@atproto/api';

const BSKY_SERVICE = 'https://bsky.social';
const BSKY_IDENTIFIER = process.env.BSKY_IDENTIFIER;     // aesthetic.computer
const BSKY_APP_PASSWORD = process.env.BSKY_APP_PASSWORD;

// Handles that should be mirrored to Bluesky
const MIRROR_HANDLES = ['@jeffrey'];

export function shouldMirror(handle) {
  return MIRROR_HANDLES.includes(handle);
}

export async function postMoodToBluesky(moodText, handle, atprotoRkey) {
  if (!BSKY_IDENTIFIER || !BSKY_APP_PASSWORD) {
    throw new Error('Bluesky credentials not configured');
  }

  const agent = new BskyAgent({ service: BSKY_SERVICE });
  
  await agent.login({
    identifier: BSKY_IDENTIFIER,
    password: BSKY_APP_PASSWORD
  });

  // Format post with mood and permalink
  const permalink = `https://aesthetic.computer/moods~${handle.replace('@', '')}~${atprotoRkey}`;
  const postText = `${moodText}\n\n${permalink}`;

  const response = await agent.post({
    text: postText,
    createdAt: new Date().toISOString(),
    // Could add facets for rich links in future
  });

  return {
    uri: response.uri,
    cid: response.cid,
    rkey: response.uri.split('/').pop()
  };
}
```

### Environment Variables Needed

Add to `system/.env`:
```
BSKY_IDENTIFIER=aesthetic.computer
BSKY_APP_PASSWORD=<from vault>
```

### Configuration

Make mirrored handles configurable:
```javascript
// Option 1: Environment variable
const MIRROR_HANDLES = (process.env.BSKY_MIRROR_HANDLES || '@jeffrey').split(',');

// Option 2: Database config (more flexible)
// Could store in a "settings" collection
```

---

## 🎯 Feature 2: Mood Permalinks

### URL Structure

```
aesthetic.computer/moods~@jeffrey~<rkey>
```

Where:
- `moods` = piece name
- `@jeffrey` = handle (without @, so `jeffrey`)
- `<rkey>` = ATProto record key (e.g., `3lfjwksvt2c2a`)

Example: `aesthetic.computer/moods~jeffrey~3lfjwksvt2c2a`

### Why Use ATProto rkey?

1. **Already exists** - Every synced mood has `atproto.rkey` in MongoDB
2. **Globally unique** - TID format guarantees uniqueness
3. **Federated** - Can resolve via ATProto if needed
4. **Short-ish** - ~13 chars vs MongoDB ObjectId's 24 chars

### Implementation

#### 1. Update moods.mjs to Handle Single Mood View

```javascript
// In boot() function of moods.mjs
function boot({ params, ... }) {
  // Check for single mood permalink
  // params will be ['@jeffrey', '3lfjwksvt2c2a'] or ['jeffrey', '3lfjwksvt2c2a']
  if (params.length === 2) {
    const handle = params[0].startsWith('@') ? params[0] : `@${params[0]}`;
    const rkey = params[1];
    isSingleMoodView = true;
    
    fetch(`/api/mood/single?handle=${handle}&rkey=${rkey}`)
      .then(res => res.json())
      .then(body => {
        if (body.mood) {
          singleMood = body;
          // Also fetch engagement from Bluesky if mirrored
          if (body.bluesky?.uri) {
            fetchBlueskyEngagement(body.bluesky.uri);
          }
        }
      });
  } else {
    // Existing list view logic
    fetch(`/api/mood/all?for=${params.join(',')}`) ...
  }
}
```

#### 2. New API Endpoint: `GET /api/mood/single`

Add to `mood.mjs`:

```javascript
// In GET handler, check for 'single' slug
if (slug === "single") {
  const handle = event.queryStringParameters?.handle;
  const rkey = event.queryStringParameters?.rkey;
  
  if (!handle || !rkey) {
    return respond(400, { message: "Missing handle or rkey" });
  }
  
  const mood = await getMoodByRkey(database, handle, rkey);
  if (!mood) {
    return respond(404, { message: "Mood not found" });
  }
  
  // Optionally fetch Bluesky engagement
  const engagement = await fetchBlueskyEngagement(mood);
  
  await database.disconnect();
  return respond(200, { ...mood, engagement });
}
```

#### 3. Database Query: `getMoodByRkey()`

```javascript
async function getMoodByRkey(database, handle, rkey) {
  const collection = database.db.collection("moods");
  
  const pipeline = [
    {
      $lookup: {
        from: "@handles",
        localField: "user",
        foreignField: "_id",
        as: "handleInfo",
      },
    },
    { $unwind: "$handleInfo" },
    {
      $match: {
        "handleInfo.handle": handle.replace('@', ''),
        "atproto.rkey": rkey,
        deleted: { $ne: true }
      }
    },
    {
      $project: {
        _id: 0,
        mood: 1,
        when: 1,
        handle: { $concat: ["@", "$handleInfo.handle"] },
        atproto: 1,
        bluesky: 1 // If we store Bluesky cross-post info
      }
    }
  ];

  const records = await collection.aggregate(pipeline).toArray();
  return records[0] || null;
}
```

---

## 🎯 Feature 3: Engagement Display (Likes & Replies)

### Data Sources

| Source | Data | Notes |
|--------|------|-------|
| **ATProto PDS** | N/A | Our PDS doesn't have social features |
| **Bluesky** | Likes, reposts, replies | Only for mirrored posts |

### Implementation

#### Store Bluesky Post Reference

When mirroring to Bluesky, store the reference:

```javascript
// In mood.mjs, after Bluesky post succeeds
if (blueskyResult) {
  await collection.updateOne(
    { _id: insertResult.insertedId },
    { 
      $set: { 
        atproto: { rkey: atprotoResult.rkey },
        bluesky: { 
          uri: blueskyResult.uri,
          cid: blueskyResult.cid
        }
      } 
    }
  );
}
```

#### Fetch Bluesky Engagement

```javascript
// system/backend/bluesky-engagement.mjs

import { BskyAgent } from '@atproto/api';

export async function fetchBlueskyEngagement(blueskyUri) {
  const agent = new BskyAgent({ service: 'https://bsky.social' });
  // Public API - no auth needed for reading
  
  try {
    // Parse URI: at://did:plc:xxx/app.bsky.feed.post/rkey
    const [, , did, , rkey] = blueskyUri.split('/');
    
    // Get thread with replies
    const thread = await agent.getPostThread({ uri: blueskyUri });
    
    return {
      likes: thread.data.thread.post?.likeCount || 0,
      reposts: thread.data.thread.post?.repostCount || 0,
      replies: (thread.data.thread.replies || []).map(r => ({
        author: r.post.author.handle,
        text: r.post.record.text,
        when: r.post.record.createdAt
      }))
    };
  } catch (error) {
    console.error("Failed to fetch Bluesky engagement:", error);
    return { likes: 0, reposts: 0, replies: [] };
  }
}
```

### Display in moods.mjs

```javascript
// In renderSingleMood() - new function for permalink view
function renderSingleMood({ ink, text, screen, ... }) {
  // Large mood display
  ink("white").write(singleMood.mood, { 
    x: screen.width / 2, 
    y: 60, 
    size: 3, 
    center: "x" 
  });
  
  // Author and time
  ink("gray").write(`${singleMood.handle} · ${formatTime(singleMood.when)}`, {
    x: screen.width / 2,
    y: 120,
    center: "x"
  });
  
  // Engagement (if available)
  if (singleMood.engagement) {
    const { likes, reposts, replies } = singleMood.engagement;
    ink("pink").write(`♥ ${likes}  ↻ ${reposts}  💬 ${replies.length}`, {
      x: screen.width / 2,
      y: 150,
      center: "x"
    });
    
    // Show replies
    replies.forEach((reply, i) => {
      ink("cyan").write(`@${reply.author}: ${reply.text}`, {
        x: 20,
        y: 180 + (i * 20)
      });
    });
  }
}
```

---

## 📋 Implementation Checklist

### Phase 1: Bluesky Mirroring
- [ ] Create `system/backend/bluesky-mirror.mjs`
- [ ] Add Bluesky credentials to `system/.env`
- [ ] Hook into `mood.mjs` after ATProto sync
- [ ] Store Bluesky post reference in MongoDB
- [ ] Test with @jeffrey mood post
- [ ] Add configurable mirror handles list

### Phase 2: Mood Permalinks
- [ ] Add `GET /api/mood/single` endpoint
- [ ] Add `getMoodByRkey()` to database.mjs
- [ ] Update moods.mjs to detect permalink params
- [ ] Create single mood view renderer
- [ ] Test permalink URL: `moods~jeffrey~<rkey>`

### Phase 3: Engagement Display
- [ ] Create `system/backend/bluesky-engagement.mjs`
- [ ] Fetch engagement on single mood load
- [ ] Display likes/reposts count
- [ ] Display replies with author handles
- [ ] Add refresh mechanism for live engagement

### Phase 4: Polish
- [ ] Add meta tags for social sharing (OG image, etc.)
- [ ] Handle missing/deleted moods gracefully
- [ ] Cache Bluesky engagement (rate limits)
- [ ] Add "Share" button to moods list view

---

## 🔑 Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| **Permalink ID** | ATProto rkey | Already exists, unique, federated |
| **URL Format** | `moods~handle~rkey` | Consistent with existing AC URL conventions |
| **Mirror Account** | @aesthetic.computer | Official brand presence |
| **Mirror Scope** | @jeffrey only (configurable) | Start small, expand later |
| **Engagement Source** | Bluesky only | ATProto PDS has no social features |

---

## 🔗 Related Files

- [at/MOODS-GET-STARTED.md](../at/MOODS-GET-STARTED.md) - ATProto integration guide
- [at/MOODS-ATPROTO-INTEGRATION-PLAN.md](../at/MOODS-ATPROTO-INTEGRATION-PLAN.md) - Original integration plan
- [at/post-to-bluesky.mjs](../at/post-to-bluesky.mjs) - Reference implementation for Bluesky posting

---

## 🧪 Testing Commands

```bash
# Test single mood API
curl "https://aesthetic.computer/api/mood/single?handle=@jeffrey&rkey=<RKEY>"

# Test Bluesky posting (dry run)
node at/post-to-bluesky.mjs "Test mood from AC"

# View @jeffrey's moods
open "https://aesthetic.computer/moods~@jeffrey"

# View single mood permalink
open "https://aesthetic.computer/moods~jeffrey~<RKEY>"
```

---

## ⚠️ Considerations

1. **Rate Limits** - Bluesky has rate limits; cache engagement data
2. **Credential Security** - Bluesky app password in vault, not committed
3. **Deletion Sync** - If mood deleted, should we delete Bluesky post?
4. **Edit Sync** - Moods can't be edited currently, so no concern
5. **ATProto Failures** - Continue if ATProto fails (MongoDB is primary)
6. **Bluesky Failures** - Continue if Bluesky fails (ATProto is secondary)
