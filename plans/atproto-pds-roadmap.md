# ATProto PDS Integration Roadmap for Aesthetic Computer

**Strategy:** Run Aesthetic Computer as its own ATProto PDS (Personal Data Server), replacing Auth0 with native ATProto authentication.

**Goal:** Transform AC into a first-class ATProto citizen where AC handles become ATProto identities (e.g., `@jeffrey` → `jeffrey.aesthetic.computer`), enabling users to interact across the entire ATProto network while maintaining AC's unique features.

---

## 🚀 Intermediate Path: Start Simple with Official Account

**Before committing to running a full PDS**, you can start exploring ATProto today:

### Use the Official @aesthetic.computer Bluesky Account

Post from AC to the **official @aesthetic.computer** Bluesky account when users share paintings:

**Benefits:**
- ✅ **Zero infrastructure** - no PDS to run, no servers to maintain
- ✅ **Instant Bluesky presence** - use existing @aesthetic.computer account
- ✅ **Test federation** - see how AC content looks on Bluesky
- ✅ **Learn ATProto** - experiment with SDK before big commitment
- ✅ **Simple auth** - just need app password from Bluesky settings
- ✅ **No migration** - MongoDB/Auth0 stay exactly as-is

**Implementation:**
```javascript
// When user clicks "Share to Bluesky"
await agent.post({
  text: `New painting by @${user.handle}: "${painting.title}" 🎨`,
  embed: { $type: 'app.bsky.embed.images', images: [{ image: blob, alt: painting.title }] }
})
```

**Good for:**
- Marketing AC on Bluesky
- Showcasing user creations  
- Testing federation features
- Building Bluesky following

**See `/at` directory for ready-to-run experiments!**

Then when ready, migrate to full PDS for per-user identities.

---

## Executive Summary

### Why This Approach?

- ✅ **No handle conflicts** - AC owns the `aesthetic.computer` namespace
- ✅ **Simpler auth** - Replace Auth0 with native ATProto OAuth (one less dependency)
- ✅ **Automatic federation** - AC users automatically part of ATProto network (Bluesky, etc.)
- ✅ **Data sovereignty** - Full control over user data and moderation
- ✅ **Cost savings** - Eliminate Auth0 monthly fees
- ✅ **Creative network positioning** - AC becomes a creative-focused ATProto community

### Architecture Overview - Hybrid Data Model

**Key Principle:** PDS handles **identity & federation only**. AC-specific content stays in existing infrastructure.

```
┌──────────────────────────────────────────────────────────────────────┐
│                   Aesthetic Computer Infrastructure                   │
│                                                                        │
│  ┌────────────────┐         ┌──────────────────────────────┐        │
│  │   AC Client    │◄───────►│   Session Server (Fastify)   │        │
│  │  (Browser/     │ WebSocket│   - WebSocket routing        │        │
│  │   VS Code)     │         │   - Session management       │        │
│  └────────────────┘         └──────────┬───────────────────┘        │
│         │                               │                             │
│         │ Auth                          │ Data CRUD                   │
│         │                               │                             │
│         ▼                               ▼                             │
│  ┌──────────────────┐         ┌────────────────────────────┐        │
│  │  AC PDS Server   │         │   MongoDB (KEEP)           │        │
│  │  (@atproto/pds)  │         │   - Paintings metadata     │        │
│  │                  │         │   - Moods                  │        │
│  │ ONLY FOR:        │         │   - KidLisp snippets       │        │
│  │ - User DIDs      │         │   - Chat messages          │        │
│  │ - OAuth auth     │         │   - Pieces                 │        │
│  │ - Profile info   │         │   - User preferences       │        │
│  │ - ATProto posts  │         │   - Logs                   │        │
│  │   (optional)     │         │   - @handles → DID mapping │        │
│  └────────┬─────────┘         └────────────────────────────┘        │
│           │                              │                            │
│           ▼                              │                            │
│  ┌──────────────────┐                   │                            │
│  │   PostgreSQL     │                   │                            │
│  │  - ATProto repos │                   │                            │
│  │  - User DIDs     │                   │                            │
│  └──────────────────┘                   │                            │
│                                          │                            │
│                                          ▼                            │
│                              ┌───────────────────────┐               │
│                              │ DigitalOcean Spaces   │               │
│                              │  (KEEP)               │               │
│                              │  - Painting files     │               │
│                              │  - Asset storage      │               │
│                              │  - Media blobs        │               │
│                              └───────────────────────┘               │
└──────────────────────────────────────────────────────────────────────┘
            │ ATProto Federation (OPTIONAL - user choice)
            ▼
    Bluesky & ATProto Network
    (Only for content user explicitly shares)
```

### Data Storage Strategy

| Data Type | Current Storage | After ATProto | Reason |
|-----------|----------------|---------------|---------|
| **User Identity** | MongoDB (`@handles`) | PDS (PostgreSQL) + MongoDB mapping | PDS owns DIDs, MongoDB maps DIDs ↔ handles |
| **Authentication** | Auth0 | PDS OAuth | Replace Auth0 with ATProto |
| **Paintings** | DigitalOcean Spaces | DigitalOcean Spaces ✅ | **NO CHANGE** - AC-specific content |
| **Moods** | MongoDB | MongoDB ✅ | **NO CHANGE** - AC-specific feature |
| **KidLisp snippets** | MongoDB | MongoDB ✅ | **NO CHANGE** - AC-specific feature |
| **Chat messages** | MongoDB | MongoDB ✅ | **NO CHANGE** - AC internal chat |
| **Pieces/Projects** | MongoDB | MongoDB ✅ | **NO CHANGE** - AC-specific content |
| **User preferences** | MongoDB | MongoDB ✅ | **NO CHANGE** - AC settings |
| **Logs** | MongoDB | MongoDB ✅ | **NO CHANGE** - AC activity logs |
| **ATProto posts** | N/A | PDS (optional) | New: If user shares to ATProto network |
| **ATProto profile** | N/A | PDS | New: ATProto-standard profile |
| **Social graph** | N/A | PDS (optional) | New: Follows/followers on ATProto |

---

## Understanding ATProto Federation

### How Posts Reach Bluesky (Critical Concept)

**Key Question:** "If I run my own PDS, why would posts appear on Bluesky?"

**Answer:** ATProto federation works like email:

```
┌─────────────────────────────────────────────────────────────┐
│              ATProto Federation (Like Email)                 │
│                                                              │
│  AC PDS                    Bluesky PDS                       │
│  (your server)             (bsky.social)                     │
│     │                           │                            │
│     │ Firehose Events           │ Firehose Events            │
│     │                           │                            │
│     └───────────┬───────────────┘                            │
│                 │                                            │
│                 ▼                                            │
│         ┌───────────────┐                                   │
│         │  Bluesky      │                                   │
│         │  AppView      │  Indexes ALL PDS servers          │
│         │  (Relay)      │  Filters by lexicon type          │
│         └───────┬───────┘                                   │
│                 │                                            │
│                 ▼                                            │
│         Only shows posts                                    │
│         using app.bsky.*                                    │
│         lexicons                                            │
└─────────────────────────────────────────────────────────────┘
```

### What Appears on Bluesky vs AC-Only

| Record Type | Appears on Bluesky? | Appears in AC? | Why? |
|-------------|---------------------|----------------|------|
| `app.bsky.feed.post` | ✅ Yes | ✅ Yes | Bluesky's standard post format |
| `app.bsky.feed.like` | ✅ Yes | ✅ Yes | Bluesky's standard like |
| `app.bsky.graph.follow` | ✅ Yes | ✅ Yes | Bluesky's follow system |
| `computer.aesthetic.painting` | ❌ No | ✅ Yes | Custom lexicon, Bluesky doesn't understand it |
| `computer.aesthetic.kidlisp` | ❌ No | ✅ Yes | Custom lexicon, AC-specific |
| `computer.aesthetic.piece` | ❌ No | ✅ Yes | Custom lexicon, AC-specific |

### Publishing Strategy: Two Options

#### Option 1: Bluesky-Compatible Posts (Recommended)

If you want AC users to appear on Bluesky, use **standard Bluesky lexicons**:

```javascript
// This WILL appear on Bluesky
const post = await agent.post({
  $type: 'app.bsky.feed.post',
  text: 'Check out my new painting on Aesthetic Computer!',
  embed: {
    $type: 'app.bsky.embed.images',
    images: [{ 
      image: blob,  // Painting uploaded to PDS
      alt: 'My artwork'
    }]
  },
  createdAt: new Date().toISOString()
})

// Result: Appears on Bluesky as a normal post with image
// Users see: @jeffrey.aesthetic.computer posted
```

#### Option 2: AC-Only Custom Records

If you want rich AC-specific metadata that Bluesky won't display:

```javascript
// This will NOT appear on Bluesky
const painting = await agent.com.atproto.repo.createRecord({
  repo: did,
  collection: 'computer.aesthetic.painting',
  record: {
    $type: 'computer.aesthetic.painting',
    title: 'Abstract Dreams',
    description: 'Created with brush and stamp tools',
    media: { blob: paintingBlob, mimeType: 'image/png' },
    tools: ['brush', 'stamp', 'filter'],
    dimensions: { width: 1920, height: 1080 },
    layers: 5,
    brushStrokes: 1247,
    timeSpent: 3600,  // seconds
    originalUrl: 'https://assets.aesthetic.computer/paintings/...',
    createdAt: new Date().toISOString()
  }
})

// Result: Stored in AC's PDS, only visible to AC or custom AppViews
// Bluesky won't show this because it doesn't understand the lexicon
```

#### Option 3: Both (Best of Both Worlds)

Create **two records** - one for Bluesky visibility, one for AC metadata:

```javascript
// 1. Post to Bluesky (for visibility)
const bskyPost = await agent.post({
  $type: 'app.bsky.feed.post',
  text: 'New painting: Abstract Dreams 🎨\n\nCreated on @aesthetic.computer',
  embed: {
    $type: 'app.bsky.embed.images',
    images: [{ image: blob, alt: 'Abstract Dreams' }]
  },
  createdAt: new Date().toISOString()
})

// 2. Store rich metadata in AC lexicon
const acRecord = await agent.com.atproto.repo.createRecord({
  repo: did,
  collection: 'computer.aesthetic.painting',
  record: {
    $type: 'computer.aesthetic.painting',
    title: 'Abstract Dreams',
    tools: ['brush', 'stamp'],
    brushStrokes: 1247,
    timeSpent: 3600,
    bskyPostUri: bskyPost.uri,  // Link to Bluesky post
    originalUrl: 'https://assets.aesthetic.computer/paintings/...',
    // ... all AC-specific metadata
  }
})

// 3. Store in MongoDB for AC features
await paintings.insertOne({
  _id: ObjectId(),
  user: did,
  title: 'Abstract Dreams',
  url: 'https://assets.aesthetic.computer/paintings/...',
  atprotoUri: acRecord.uri,
  bskyPostUri: bskyPost.uri,
  // ... other AC data
})
```

**Result:** 
- ✅ Appears on Bluesky as normal post
- ✅ AC has full metadata in custom lexicon
- ✅ AC can display rich details
- ✅ MongoDB has everything for AC features

### Why Run Your Own PDS?

**"If I need Bluesky lexicons to appear on Bluesky, why run my own PDS?"**

Great question! Here's why:

1. **Identity Control**
   - AC users get `jeffrey.aesthetic.computer` handles
   - You own the namespace, not Bluesky
   - Users can migrate away from AC but keep their DID

2. **Data Sovereignty**
   - You control user authentication
   - You decide moderation policies
   - You own the user data

3. **Custom Features**
   - Store AC-specific data in custom lexicons
   - Build AC-specific AppViews later
   - Not limited by Bluesky's features

4. **No Middleman**
   - No Auth0 fees
   - No dependency on Bluesky's PDS hosting
   - Full control over uptime and performance

5. **Hybrid Publishing**
   - Users can share to Bluesky when they want
   - Or keep content AC-only
   - Best of both worlds

### Federation Modes

You can configure AC to work in different modes:

#### Mode 1: AC-Only (No Federation)

```env
# Don't register with Bluesky's relay
PDS_CRAWLERS=
PDS_BSKY_APP_VIEW_URL=
```

- AC users can't appear on Bluesky
- Completely independent network
- Faster, simpler, more private
- Use case: Private AC community

#### Mode 2: Federated (Recommended)

```env
# Register with Bluesky's relay
PDS_CRAWLERS=https://bsky.network
PDS_BSKY_APP_VIEW_URL=https://api.bsky.app
```

- AC users can post to Bluesky using `app.bsky.*` lexicons
- AC users can follow/be followed by Bluesky users
- AC maintains its own features via custom lexicons
- Use case: Public creative community

#### Mode 3: Hybrid (Most Flexible)

```env
# Same as Mode 2, but with user choice
PDS_CRAWLERS=https://bsky.network
PDS_BSKY_APP_VIEW_URL=https://api.bsky.app
```

Plus in AC client:
```javascript
// User settings
user.preferences = {
  autoShareToBluesky: false,  // User opts in
  shareMode: 'manual'  // 'auto', 'manual', 'never'
}

// Only share when user explicitly chooses
if (user.preferences.autoShareToBluesky) {
  await publishToBluesky()
}
```

- Default: AC-only
- Users opt-in to Bluesky sharing
- Privacy-first approach
- Use case: Let users decide

### Building AC's Own AppView (Future)

Eventually, you could build an AC-specific AppView:

```
┌─────────────────────────────────────────────────┐
│           AC AppView (Future)                    │
│                                                  │
│  Subscribes to AC PDS + other PDS servers       │
│  Understands computer.aesthetic.* lexicons      │
│  Displays AC-specific features                  │
│  Shows painting tools, brush strokes, etc.      │
│                                                  │
│  Would appear at: https://atmosphere.aesthetic  │
└─────────────────────────────────────────────────┘
```

This would let other PDS servers use AC's lexicons too!

---

## Data Browsing & Deletion (Important!)

### Can Users Browse Their ATProto Data?

**YES!** This is one of ATProto's killer features - users can browse all their data through standard ATProto clients:

```bash
# Users can view their entire repo
https://pds.aesthetic.computer/xrpc/com.atproto.sync.getRepo?did=did:plc:abc123

# Or individual collections
https://pds.aesthetic.computer/xrpc/com.atproto.repo.listRecords?repo=did:plc:abc123&collection=computer.aesthetic.painting
```

**Third-party tools** that work with any PDS:
- **ATProto Browser:** https://blue.mackuba.eu/atproto-browser/
- **Skyview:** https://skyview.social
- **ATProto Explorer:** Various community tools

**Users can:**
- ✅ See all their paintings in `computer.aesthetic.painting`
- ✅ See all their KidLisp code in `computer.aesthetic.kidlisp`
- ✅ Export their entire repo as CAR files
- ✅ Download blobs (images, files)
- ✅ Migrate to another PDS with all data

### The Deletion Problem: Hybrid Architecture Requires Sync

**Scenario:** User deletes a painting from AC's interface

**What needs to happen:**

```
┌────────────────────────────────────────────────────┐
│         User Deletes "Abstract Dreams"             │
└────────────────┬───────────────────────────────────┘
                 │
                 ▼
        ┌────────────────────┐
        │  AC Frontend       │
        │  Sends DELETE      │
        └────────┬───────────┘
                 │
                 ▼
        ┌────────────────────────────────────────┐
        │  Backend Logic (NEW: Sync Required)    │
        │                                        │
        │  1. Delete from MongoDB                │
        │  2. Delete from DigitalOcean Spaces    │
        │  3. Delete from ATProto PDS ← NEW!     │
        └────────────────────────────────────────┘
```

#### Current AC Deletion Code

```javascript
// system/netlify/functions/painting-delete.js (hypothetical)
export async function handler(event) {
  const { paintingId } = JSON.parse(event.body)
  
  // 1. Delete from MongoDB
  await paintings.deleteOne({ _id: ObjectId(paintingId) })
  
  // 2. Delete from DigitalOcean Spaces
  await s3.deleteObject({
    Bucket: 'aesthetic-computer',
    Key: `paintings/${paintingId}.png`
  })
  
  return { statusCode: 200 }
}
```

#### Updated Code with ATProto Sync

```javascript
import { Agent } from '@atproto/api'

export async function handler(event) {
  const { paintingId } = JSON.parse(event.body)
  const userDid = event.headers['x-user-did']
  
  // Get painting record
  const painting = await paintings.findOne({ _id: ObjectId(paintingId) })
  
  // 1. Delete from MongoDB
  await paintings.deleteOne({ _id: ObjectId(paintingId) })
  
  // 2. Delete from DigitalOcean Spaces
  await s3.deleteObject({
    Bucket: 'aesthetic-computer',
    Key: `paintings/${paintingId}.png`
  })
  
  // 3. Delete from ATProto PDS (NEW!)
  if (painting.atprotoUri) {
    const agent = new Agent({
      service: 'https://pds.aesthetic.computer'
    })
    await agent.resumeSession(userSession)
    
    await agent.com.atproto.repo.deleteRecord({
      repo: userDid,
      collection: 'computer.aesthetic.painting',
      rkey: painting.atprotoRkey  // Record key from URI
    })
  }
  
  return { statusCode: 200 }
}
```

### Sync Strategies: Three Approaches

#### Strategy 1: ATProto as Source of Truth (Full Migration)

```
User Action → ATProto PDS → Webhook → Update MongoDB + DO Spaces
```

**Pros:**
- Users own their data completely
- ATProto browser shows everything
- True data portability
- Simpler mental model

**Cons:**
- Major refactor of AC's architecture
- All features must work through ATProto
- More complex for AC-specific features
- Higher PDS storage costs

#### Strategy 2: MongoDB as Source of Truth (Current + Optional ATProto)

```
User Action → MongoDB + DO Spaces → Optionally sync to ATProto
```

**Pros:**
- Minimal changes to AC
- Keep existing features working
- ATProto is "bonus" feature
- Lower costs

**Cons:**
- Must manually sync deletes
- Data could get out of sync
- Users can't fully manage via ATProto
- Less true to ATProto spirit

#### Strategy 3: Dual Write with MongoDB Primary (Recommended)

```
User Action → Write to both simultaneously
            → MongoDB is primary
            → ATProto is secondary
            → Sync job reconciles differences
```

**Implementation:**

```javascript
// Wrapper for all content operations
class ContentManager {
  async createPainting(user, painting) {
    // 1. Write to MongoDB (primary)
    const mongoResult = await paintings.insertOne({
      _id: ObjectId(),
      user: user.did,
      title: painting.title,
      url: painting.url,
      createdAt: new Date()
    })
    
    // 2. Upload to DigitalOcean Spaces
    await s3.putObject({
      Bucket: 'aesthetic-computer',
      Key: `paintings/${mongoResult.insertedId}.png`,
      Body: painting.blob
    })
    
    // 3. Write to ATProto (secondary)
    try {
      const atprotoResult = await agent.com.atproto.repo.createRecord({
        repo: user.did,
        collection: 'computer.aesthetic.painting',
        record: {
          $type: 'computer.aesthetic.painting',
          title: painting.title,
          mongoId: mongoResult.insertedId.toString(),  // Link back
          imageUrl: painting.url,
          createdAt: new Date().toISOString()
        }
      })
      
      // 4. Update MongoDB with ATProto URI
      await paintings.updateOne(
        { _id: mongoResult.insertedId },
        { $set: { atprotoUri: atprotoResult.uri } }
      )
    } catch (error) {
      // Log but don't fail - ATProto is secondary
      console.error('ATProto sync failed:', error)
      await logSyncFailure(mongoResult.insertedId, error)
    }
    
    return mongoResult
  }
  
  async deletePainting(paintingId) {
    const painting = await paintings.findOne({ _id: ObjectId(paintingId) })
    
    // 1. Delete from MongoDB
    await paintings.deleteOne({ _id: ObjectId(paintingId) })
    
    // 2. Delete from DigitalOcean Spaces
    await s3.deleteObject({
      Bucket: 'aesthetic-computer',
      Key: `paintings/${paintingId}.png`
    })
    
    // 3. Delete from ATProto if it exists there
    if (painting.atprotoUri) {
      try {
        const [, , repo, collection, rkey] = painting.atprotoUri.split('/')
        await agent.com.atproto.repo.deleteRecord({
          repo,
          collection,
          rkey
        })
      } catch (error) {
        console.error('ATProto delete failed:', error)
        // Don't fail the operation - it's already deleted from MongoDB
      }
    }
  }
}
```

### What if User Deletes from ATProto Browser?

**Problem:** User uses a third-party ATProto tool to delete `at://jeffrey.aesthetic.computer/computer.aesthetic.painting/abc123`

**MongoDB still has the record!**

**Solution: Listen to PDS Firehose Events**

```javascript
// Background sync job
import { Firehose } from '@atproto/sync'

const firehose = new Firehose({
  service: 'wss://pds.aesthetic.computer'
})

firehose.on('commit', async (event) => {
  if (event.ops) {
    for (const op of event.ops) {
      if (op.action === 'delete') {
        // User deleted something from PDS
        const uri = `at://${event.repo}/${op.path}`
        
        // Find corresponding MongoDB record
        const painting = await paintings.findOne({ atprotoUri: uri })
        
        if (painting) {
          console.log(`User deleted ${uri} from PDS, cleaning up MongoDB...`)
          
          // Clean up MongoDB
          await paintings.deleteOne({ _id: painting._id })
          
          // Clean up DigitalOcean Spaces
          await s3.deleteObject({
            Bucket: 'aesthetic-computer',
            Key: `paintings/${painting._id}.png`
          })
          
          console.log(`Sync complete for deletion of ${uri}`)
        }
      }
    }
  }
})
```

### Sync Job for Reconciliation

Run periodically to catch missed syncs:

```javascript
// Runs every hour
async function reconcilePaintings() {
  // 1. Get all paintings from MongoDB
  const mongoPaintings = await paintings.find({}).toArray()
  
  // 2. Get all paintings from ATProto
  const agent = new Agent({ service: 'https://pds.aesthetic.computer' })
  
  for (const painting of mongoPaintings) {
    if (painting.atprotoUri) {
      try {
        // Check if it still exists in ATProto
        const [, , repo, collection, rkey] = painting.atprotoUri.split('/')
        await agent.com.atproto.repo.getRecord({ repo, collection, rkey })
      } catch (error) {
        if (error.status === 404) {
          // Record was deleted from ATProto but still in MongoDB
          console.log(`Painting ${painting._id} deleted from ATProto, cleaning up...`)
          await paintings.deleteOne({ _id: painting._id })
          await s3.deleteObject({
            Bucket: 'aesthetic-computer',
            Key: `paintings/${painting._id}.png`
          })
        }
      }
    }
  }
}
```

### Recommendation: Strategy 3 with Firehose Listener

**Best approach:**
1. **MongoDB remains primary** - all AC features keep working
2. **Dual write on create** - new paintings go to both MongoDB and ATProto
3. **Dual delete on delete** - delete from both when user deletes in AC
4. **Firehose listener** - catch deletes from ATProto browser, clean up MongoDB
5. **Hourly reconciliation** - fix any missed syncs

**Cost:** One small background process listening to firehose (~$5-10/month if separate)

**Result:**
- ✅ Users can browse paintings in ATProto tools
- ✅ Users can delete from ATProto, AC stays in sync
- ✅ Users can delete from AC, ATProto stays in sync
- ✅ Data portability maintained
- ✅ AC features all work
- ✅ Minimal refactoring needed

---

## Data Flow & Content Strategy

### How AC Content Works with ATProto

**Core Principle:** AC content lives on AC infrastructure. ATProto publishing is **optional** and **user-controlled**.

#### Scenario 1: User Creates a Painting

```javascript
// Current flow (UNCHANGED):
1. User paints in AC
2. Painting saved to DigitalOcean Spaces: 
   paintings/jeffrey/my-artwork-2025.png
3. Metadata saved to MongoDB:
   {
     _id: ObjectId("..."),
     user: "did:plc:abc123",  // ← Changed from auth0|123 to DID
     filename: "my-artwork-2025.png",
     url: "https://assets.aesthetic.computer/paintings/jeffrey/...",
     created: ISODate(),
     // ... other AC-specific metadata
   }

// NEW: Optional ATProto publishing
4. User clicks "Share to ATProto Network"
5. AC creates ATProto post record:
   {
     $type: 'app.bsky.feed.post',
     text: 'Check out my new painting!',
     embed: {
       $type: 'app.bsky.embed.images',
       images: [{
         image: blob,  // Uploaded to PDS blob storage
         alt: 'My artwork'
       }]
     }
   }
6. Post appears on Bluesky for @jeffrey.aesthetic.computer
7. Original painting STILL in DigitalOcean Spaces (not moved)
```

**Result:** Painting exists in TWO places:
- **DigitalOcean Spaces** (primary, full resolution, AC-controlled)
- **PDS blob storage** (optional, shared copy for ATProto network)

#### Scenario 2: User Creates a Mood

```javascript
// Current flow (COMPLETELY UNCHANGED):
1. User sets mood in AC
2. Saved to MongoDB moods collection:
   {
     _id: ObjectId("..."),
     user: "did:plc:abc123",  // ← Only change: DID instead of auth0|123
     mood: "happy",
     when: ISODate()
   }
3. Displayed in AC interface
4. Other AC users see the mood

// NO ATProto integration needed
// Moods are AC-internal feature only
```

#### Scenario 3: User Writes KidLisp Code

```javascript
// Current flow (COMPLETELY UNCHANGED):
1. User writes KidLisp in AC
2. Code cached in MongoDB:
   {
     _id: ObjectId("..."),
     user: "did:plc:abc123",  // ← Only change: DID
     code: "(defn hello [] (print \"Hello\"))",
     hash: "abc123...",
     when: ISODate()
   }
3. Code executed in AC runtime

// OPTIONAL: Custom ATProto lexicon for sharing
4. User chooses to share KidLisp snippet
5. Publish as custom ATProto record:
   {
     $type: 'computer.aesthetic.kidlisp',
     code: "(defn hello [] (print \"Hello\"))",
     description: "My cool function",
     createdAt: ISODate()
   }
```

#### Scenario 4: Chat Messages

```javascript
// Chat is AC-INTERNAL ONLY (no change):
1. User sends chat message
2. Stored in MongoDB chat-system collection
3. Broadcasted via WebSocket to AC clients
4. NEVER published to ATProto (chat is private to AC)
```

### Content Publishing Decision Matrix

| AC Feature | Storage | Publishable to ATProto? | Why? |
|------------|---------|------------------------|------|
| **Paintings** | DO Spaces + MongoDB | ✅ Optional | User may want to share artwork on ATProto network |
| **Moods** | MongoDB | ❌ No | AC-internal feature, no ATProto equivalent |
| **KidLisp code** | MongoDB | ✅ Optional (custom lexicon) | Shareable programming content |
| **Chat messages** | MongoDB | ❌ No | Private/ephemeral communication |
| **Pieces/projects** | MongoDB | ✅ Optional | Could share as ATProto records |
| **User profile** | MongoDB + PDS | ✅ Synced | ATProto profile mirrors AC profile |
| **Social posts** | N/A currently | ✅ Yes | New feature: post to ATProto like Bluesky |

### Storage Cost Implications

**Before ATProto:**
- DigitalOcean Spaces: ~$5-20/month
- MongoDB Atlas: ~$0-50/month (or self-hosted)
- Auth0: $25-240/month
- **Total:** ~$30-310/month

**After ATProto:**
- DigitalOcean Spaces: ~$5-20/month (same)
- MongoDB Atlas: ~$0-50/month (same)
- PDS Server: ~$15-20/month (replaces Auth0)
- PDS PostgreSQL: ~$0-15/month (small, only identity data)
- PDS Blob Storage: ~$1-10/month (only for shared content)
- **Total:** ~$21-115/month

**Savings:** ~$9-195/month (eliminating Auth0, minimal PDS costs)

---

## Phase 0: Research & Infrastructure Planning (Week 1-2)

### ✅ Research Tasks

- [x] **Document PDS deployment options**
  - Official PDS supports: Ubuntu 20.04/22.04/24.04, Debian 11/12
  - Requires: 1GB RAM, 1 CPU, 20GB SSD minimum
  - Docker-based deployment with Caddy for TLS
  - Installer script available: `https://raw.githubusercontent.com/bluesky-social/pds/main/installer.sh`

- [ ] **Research cloud hosting options**
  - [ ] Google Cloud Platform (GCP)
    - **Recommended:** Compute Engine e2-small instance ($15-20/month)
    - Existing AC infrastructure on GCP
    - Use Cloud SQL for PostgreSQL or self-managed on VM
  - [ ] Cloudflare Workers/Pages
    - ❌ **Not suitable** - PDS requires long-running server process
    - Cloudflare Workers are stateless edge functions
    - PDS needs persistent WebSocket connections
  - [ ] Digital Ocean
    - Basic Droplet: $6/month (1GB RAM)
    - Managed PostgreSQL: $15/month
    - Simple setup, good for prototyping
  - [ ] Existing Jamsocket/Netlify infrastructure
    - Current session-server could coexist with PDS
    - Netlify Functions won't work (need persistent server)

- [ ] **ATProto SDK & library selection**
  - **Core packages to install:**
    - `@atproto/api` - Main API client (70k weekly downloads)
    - `@atproto/oauth-client-node` - OAuth for backend
    - `@atproto/oauth-client-browser` - OAuth for frontend
    - `@atproto/identity` - DID resolution
    - `@atproto/lexicon` - Schema validation
  - **Optional packages:**
    - `@atproto/sync` - Firehose/relay integration (for Phase 2)
    - `@atproto/repo` - Repository management

- [ ] **MongoDB to PostgreSQL coexistence strategy**
  - ATProto PDS uses PostgreSQL/SQLite for ATProto data only
  - **Decision: Keep MongoDB for ALL AC-specific data**
    - ✅ Paintings metadata stays in MongoDB
    - ✅ Moods stay in MongoDB
    - ✅ KidLisp snippets stay in MongoDB
    - ✅ Chat messages stay in MongoDB
    - ✅ Pieces/projects stay in MongoDB
    - ✅ User preferences stay in MongoDB
  - **PDS PostgreSQL stores ONLY:**
    - User DIDs and ATProto authentication
    - ATProto repository data (if user publishes)
    - ATProto-standard profile info
  - **MongoDB `@handles` collection updated to:**
    - Map DID ↔ AC handle
    - Store user preferences
    - Cache ATProto handle for display

- [ ] **DNS requirements documentation**
  - Need: `pds.aesthetic.computer` A record
  - Need: `*.aesthetic.computer` wildcard A record (for user subdomains)
  - Need: `_atproto.aesthetic.computer` TXT record for verification
  - Current DNS provider: Check existing setup

### 📋 Deliverables
- [ ] Infrastructure decision document (GCP vs DO vs hybrid)
- [ ] Cost analysis spreadsheet (PDS hosting + PostgreSQL)
- [ ] DNS configuration checklist
- [ ] Package dependency list with versions

---

## Phase 1: PDS Infrastructure Setup (Week 3-4)

### 🚀 Deployment Tasks

- [ ] **Provision PDS server**
  - [ ] Create GCP Compute Engine instance (or Digital Ocean droplet)
    ```bash
    # GCP example
    gcloud compute instances create ac-pds \
      --machine-type=e2-small \
      --image-family=ubuntu-2204-lts \
      --image-project=ubuntu-os-cloud \
      --boot-disk-size=20GB \
      --zone=us-central1-a
    ```
  - [ ] Open firewall ports 80/tcp and 443/tcp
  - [ ] Set up SSH access with key authentication
  - [ ] Configure static IP address

- [ ] **Configure DNS for PDS**
  - [ ] Add A record: `pds.aesthetic.computer` → `<server-ip>`
  - [ ] Add wildcard A record: `*.aesthetic.computer` → `<server-ip>`
  - [ ] Add TXT record: `_atproto.aesthetic.computer` → `"did=did:web:aesthetic.computer"`
  - [ ] Verify DNS propagation using https://dnschecker.org/

- [ ] **Install PDS using official installer**
  ```bash
  # SSH into server
  ssh root@pds.aesthetic.computer
  
  # Download and run installer
  wget https://raw.githubusercontent.com/bluesky-social/pds/main/installer.sh
  sudo bash installer.sh
  
  # Follow prompts:
  # - Hostname: pds.aesthetic.computer
  # - Admin email: admin@aesthetic.computer
  # - Handle for admin: jeffrey.aesthetic.computer
  ```

- [ ] **Configure PDS environment** (`/pds/pds.env`)
  ```bash
  PDS_HOSTNAME=pds.aesthetic.computer
  PDS_SERVICE_DID=did:web:aesthetic.computer
  PDS_ADMIN_PASSWORD=<secure-password>
  PDS_DATA_DIRECTORY=/pds/data
  PDS_BLOBSTORE_DISK_LOCATION=/pds/blocks
  PDS_DID_PLC_URL=https://plc.directory
  PDS_CRAWLERS=https://bsky.network
  
  # Email setup (using Resend or SendGrid)
  PDS_EMAIL_SMTP_URL=smtps://resend:<api-key>@smtp.resend.com:465/
  PDS_EMAIL_FROM_ADDRESS=noreply@aesthetic.computer
  
  # Optional: Bluesky federation
  PDS_BSKY_APP_VIEW_URL=https://api.bsky.app
  PDS_REPORT_SERVICE_URL=https://mod.bsky.app
  ```

- [ ] **Verify PDS is running**
  ```bash
  # Check health endpoint
  curl https://pds.aesthetic.computer/xrpc/_health
  # Should return: {"version":"0.x.x"}
  
  # Test WebSocket
  npm install -g wsdump
  wsdump "wss://pds.aesthetic.computer/xrpc/com.atproto.sync.subscribeRepos?cursor=0"
  ```

- [ ] **Set up PostgreSQL database**
  - [ ] Option A: Use PDS's included SQLite (for prototype)
  - [ ] Option B: Migrate to PostgreSQL
    ```bash
    # Install PostgreSQL
    sudo apt install postgresql postgresql-contrib
    
    # Create PDS database
    sudo -u postgres createdb atproto_pds
    sudo -u postgres createuser pds_user
    
    # Configure PDS to use PostgreSQL
    # Edit /pds/pds.env:
    PDS_DB_POSTGRES_URL=postgresql://pds_user:password@localhost/atproto_pds
    ```

- [ ] **Create first test account**
  ```bash
  sudo pdsadmin account create
  # Handle: jeffrey.aesthetic.computer
  # Email: jeffrey@aesthetic.computer
  # Password: <secure-password>
  ```

- [ ] **Set up monitoring & logging**
  - [ ] Configure log output: `LOG_DESTINATION=/pds/logs/pds.log`
  - [ ] Set log level: `LOG_LEVEL=info`
  - [ ] Set up log rotation
  - [ ] Configure uptime monitoring (UptimeRobot, Pingdom, etc.)

### 📋 Deliverables
- [ ] Running PDS accessible at `https://pds.aesthetic.computer`
- [ ] Admin account created and verified
- [ ] DNS fully propagated and validated
- [ ] Monitoring dashboard set up
- [ ] Backup strategy documented

---

## Phase 2: Backend Integration (Week 5-7)

### 🔧 Backend Development Tasks

- [ ] **Install ATProto packages in project**
  ```bash
  # In aesthetic-computer root
  npm install @atproto/api \
              @atproto/oauth-client-node \
              @atproto/identity \
              @atproto/lexicon
  
  # In session-server
  cd session-server
  npm install @atproto/api @atproto/oauth-client-node
  ```

- [ ] **Create ATProto backend module** (`system/backend/atproto.mjs`)
  - [ ] Initialize ATProto agent pointing to AC's PDS
    ```javascript
    import { AtpAgent } from '@atproto/api'
    
    const agent = new AtpAgent({
      service: 'https://pds.aesthetic.computer'
    })
    
    export { agent }
    ```
  - [ ] Implement user creation function
  - [ ] Implement authentication function
  - [ ] Implement session validation function
  - [ ] Implement DID resolution utilities

- [ ] **Update authorization.mjs for ATProto**
  - [ ] Create `authorizeATProto()` function
    ```javascript
    export async function authorizeATProto({ authorization }) {
      try {
        // Validate JWT from AC's PDS
        const session = await agent.resumeSession(authorization)
        return {
          did: session.did,
          handle: session.handle,
          email: session.email
        }
      } catch (err) {
        return undefined
      }
    }
    ```
  - [ ] Update `handleFor()` to use DIDs as primary keys
  - [ ] Create `createATProtoUser()` function
  - [ ] Add DID-to-handle mapping utilities

- [ ] **MongoDB schema updates**
  - [ ] Add migration script for new fields
    ```javascript
    // @handles collection changes (KEEPS EXISTING STRUCTURE):
    {
      _id: "did:plc:abc123",              // New: DID as primary key (was auth0|123)
      handle: "jeffrey",                   // Existing: AC handle (unchanged)
      atprotoHandle: "jeffrey.aesthetic.computer", // New
      email: "jeffrey@example.com",       // Existing
      authProvider: "atproto",            // New: "auth0" or "atproto"
      legacyAuth0Sub: "auth0|123",        // New: For migration period
      created: ISODate(),
      updated: ISODate()
    }
    
    // ALL OTHER COLLECTIONS UNCHANGED:
    // - paintings (stays same, references user DID or handle)
    // - moods (stays same)
    // - kidlisp_cache (stays same)
    // - chat-system (stays same)
    // - pieces (stays same)
    // - logs (stays same)
    ```
  - [ ] Create indexes on new fields (did, atprotoHandle)
  - [ ] Write migration script for existing Auth0 users
  - [ ] **NO changes needed to other collections** (paintings, moods, etc.)

- [ ] **Session-server WebSocket auth updates**
  - [ ] Support both Auth0 and ATProto tokens during transition
  - [ ] Update message authentication in `session-server/session.mjs`
  - [ ] Add DID-based client identification

- [ ] **API endpoint creation**
  - [ ] `POST /api/atproto/signup` - Create ATProto account
  - [ ] `POST /api/atproto/login` - Authenticate with ATProto
  - [ ] `POST /api/atproto/session/refresh` - Refresh session token
  - [ ] `GET /api/atproto/session` - Get current session info
  - [ ] `POST /api/atproto/migrate` - Migrate Auth0 user to ATProto

### 📋 Deliverables
- [ ] `system/backend/atproto.mjs` module with full ATProto integration
- [ ] Updated `authorization.mjs` with dual Auth0/ATProto support
- [ ] MongoDB migration scripts tested
- [ ] New API endpoints functional and documented
- [ ] Backend tests passing

---

## Phase 3: Frontend Integration (Week 8-9)

### 🎨 Frontend Development Tasks

- [ ] **Install ATProto browser packages**
  ```bash
  cd system
  npm install @atproto/api @atproto/oauth-client-browser
  ```

- [ ] **Update boot.mjs with ATProto client**
  - [ ] Import and initialize ATProto agent
    ```javascript
    import { BskyAgent } from '@atproto/api'
    
    const atprotoAgent = new BskyAgent({
      service: 'https://pds.aesthetic.computer'
    })
    ```
  - [ ] Create `window.acLOGIN_ATPROTO()` function
    ```javascript
    window.acLOGIN_ATPROTO = async (handle, password) => {
      const fullHandle = handle.includes('.') 
        ? handle 
        : `${handle}.aesthetic.computer`
      
      await atprotoAgent.login({
        identifier: fullHandle,
        password: password
      })
      
      const session = {
        did: atprotoAgent.session.did,
        handle: atprotoAgent.session.handle,
        accessJwt: atprotoAgent.session.accessJwt,
        refreshJwt: atprotoAgent.session.refreshJwt
      }
      
      localStorage.setItem('ac-atproto-session', JSON.stringify(session))
      
      window.acUSER = {
        did: session.did,
        handle: session.handle.replace('.aesthetic.computer', ''),
        provider: 'atproto'
      }
      
      window.acDISK_SEND({
        type: "session:started",
        content: { user: window.acUSER }
      })
    }
    ```
  - [ ] Create `window.acSIGNUP_ATPROTO()` function
  - [ ] Update session resumption logic
  - [ ] Handle ATProto session refresh

- [ ] **Create signup/login UI components**
  - [ ] Add "Sign up with ATProto" flow in relevant pieces
  - [ ] Update login piece to support both Auth0 and ATProto
  - [ ] Add toggle between auth methods
  - [ ] Create account migration prompt for existing users

- [ ] **Update pieces that use authentication**
  - [ ] `prompt.mjs` - Show ATProto handle
  - [ ] `profile.mjs` - Display ATProto identity info
  - [ ] `handle.mjs` - Support ATProto handle creation
  - [ ] Any piece using `window.acUSER`

- [ ] **Session persistence updates**
  - [ ] Store ATProto sessions in localStorage
  - [ ] Handle session refresh before expiry
  - [ ] Implement logout functionality
  - [ ] Clear sessions on logout

- [ ] **VS Code extension updates**
  - [ ] Update `aestheticAuthenticationProviderRemote.ts`
  - [ ] Support ATProto OAuth flow in extension
  - [ ] Pass ATProto sessions to webview
  - [ ] Update session encoding/decoding

### 📋 Deliverables
- [ ] `boot.mjs` updated with full ATProto support
- [ ] Signup/login UI working for ATProto
- [ ] Session management functional across browser/VS Code
- [ ] All authentication-dependent pieces updated
- [ ] Frontend tests passing

---

## Phase 4: Migration Strategy (Week 10-11)

### 🔄 User Migration Tasks

- [ ] **Create migration endpoint** (`/api/migrate-to-atproto`)
  ```javascript
  async function migrateUser(auth0Sub, newPassword) {
    // 1. Verify Auth0 session is active
    const user = await handles.findOne({ _id: auth0Sub })
    if (!user) throw new Error('User not found')
    
    // 2. Create ATProto account on AC's PDS
    const result = await agent.createAccount({
      handle: `${user.handle}.aesthetic.computer`,
      email: user.email,
      password: newPassword,
    })
    
    // 3. Migrate AC data to ATProto records
    await migrateUserContent(user, result.did)
    
    // 4. Update MongoDB with dual identity
    await handles.updateOne(
      { _id: auth0Sub },
      { 
        $set: { 
          migratedDID: result.did,
          migratedAt: new Date(),
          authProvider: 'atproto'
        }
      }
    )
    
    // 5. Create new primary document with DID
    await handles.insertOne({
      _id: result.did,
      handle: user.handle,
      atprotoHandle: `${user.handle}.aesthetic.computer`,
      email: user.email,
      legacyAuth0Sub: auth0Sub,
      authProvider: 'atproto',
      created: user.created,
      migratedAt: new Date()
    })
    
    return result
  }
  ```

- [ ] **Content migration utilities**
  - [ ] Migrate user paintings to ATProto blobs
  - [ ] Convert AC posts to ATProto post records
  - [ ] Preserve timestamps and metadata
  - [ ] Handle migration errors gracefully

- [ ] **Build migration UI**
  - [ ] Create migration prompt piece
  - [ ] Show benefits of ATProto migration
  - [ ] Collect new password from user
  - [ ] Display migration progress
  - [ ] Show success confirmation with new handle

- [ ] **Implement gradual rollout**
  - [ ] Phase 1: Beta users only (admin flag)
  - [ ] Phase 2: Opt-in for all users
  - [ ] Phase 3: Encourage migration (banner/prompts)
  - [ ] Phase 4: Require migration for new features
  - [ ] Phase 5: Deprecate Auth0 (set deadline)

- [ ] **Data integrity verification**
  - [ ] Verify all handles are unique across both systems
  - [ ] Check DID resolution works for all migrated users
  - [ ] Validate ATProto records are properly formatted
  - [ ] Test WebSocket authentication with both systems

### 📋 Deliverables
- [ ] Working migration endpoint with full data transfer
- [ ] Migration UI piece with clear instructions
- [ ] Rollout plan documented with timelines
- [ ] Data integrity verification scripts
- [ ] Rollback procedure documented

---

## Phase 5: ATProto Feature Integration (Week 12-14)

### 🌐 Federation & Publishing Tasks

- [ ] **Publishing pipeline setup**
  - [ ] Create `shared/atproto-publish.mjs` module
  - [ ] Implement post creation to ATProto
    ```javascript
    export async function publishPost(agent, text, media) {
      const rt = new RichText({ text })
      await rt.detectFacets(agent) // Auto-detect links/mentions
      
      const record = {
        $type: 'app.bsky.feed.post',
        text: rt.text,
        facets: rt.facets,
        createdAt: new Date().toISOString()
      }
      
      if (media) {
        const blob = await agent.uploadBlob(media)
        record.embed = {
          $type: 'app.bsky.embed.images',
          images: [{ image: blob.data.blob, alt: '' }]
        }
      }
      
      return await agent.post(record)
    }
    
    // IMPORTANT: Publishing to ATProto does NOT move files
    export async function publishPainting(agent, paintingId) {
      // 1. Fetch painting metadata from MongoDB
      const painting = await paintings.findOne({ _id: paintingId })
      
      // 2. Download painting from DigitalOcean Spaces
      const imageData = await fetch(painting.url).then(r => r.arrayBuffer())
      
      // 3. Upload to PDS as blob (creates COPY, doesn't move original)
      const blob = await agent.uploadBlob(imageData, {
        encoding: 'image/png'
      })
      
      // 4. Create ATProto post with embedded image
      const post = await publishPost(agent, painting.description || 'My artwork', blob)
      
      // 5. Store ATProto URI in MongoDB for reference
      await paintings.updateOne(
        { _id: paintingId },
        { $set: { atprotoUri: post.uri } }
      )
      
      // Original painting stays in DigitalOcean Spaces!
      return post
    }
    ```
  - [ ] Implement blob upload for images/media
  - [ ] Add ATProto publishing to relevant pieces
  - [ ] Create "Share to ATProto" commands in KidLisp
  - [ ] **Document that publishing creates copies, doesn't move data**

- [ ] **Define AC-specific Lexicons** (Advanced/Optional)
  - [ ] `computer.aesthetic.piece` - Creative piece records
  - [ ] `computer.aesthetic.kidlisp` - KidLisp program records
  - [ ] `computer.aesthetic.painting` - Painting/artwork records
  - [ ] Register lexicons in `spec/lexicons/` directory
  - [ ] Generate TypeScript types from lexicons
  - [ ] **Important:** Custom lexicons won't appear on Bluesky automatically
    - Bluesky only displays `app.bsky.*` lexicons
    - Custom lexicons are for AC's own use or future AppViews
    - See "Federation Strategy" section below

- [ ] **Implement firehose ingestion** (Optional for Phase 5)
  - [ ] Set up ingestion worker using `@atproto/sync`
  - [ ] Subscribe to AC's PDS firehose
  - [ ] Store events for "Atmosphere" feed
  - [ ] Build feed aggregation queries

- [ ] **Create "Atmosphere" feed piece**
  - [ ] Display recent posts from AC users on ATProto
  - [ ] Show cross-network interactions
  - [ ] Support filtering by content type
  - [ ] Enable likes/reposts from AC interface

- [ ] **Notification system integration**
  - [ ] Extend `ac-event-daemon` for ATProto events
  - [ ] Show mentions from ATProto network
  - [ ] Display likes/reposts/follows
  - [ ] Create notification overlay UI

### 📋 Deliverables
- [ ] Publishing pipeline functional for posts and media
- [ ] Custom AC lexicons registered and validated
- [ ] Atmosphere feed piece displaying federated content
- [ ] Notifications working for ATProto events
- [ ] Cross-posting between AC and ATProto network

---

## Phase 6: Testing & Launch (Week 15-16)

### 🧪 Testing Tasks

- [ ] **Create comprehensive test suite**
  - [ ] Unit tests for ATProto backend functions
  - [ ] Integration tests for PDS communication
  - [ ] End-to-end tests for signup/login flows
  - [ ] Migration tests with sample Auth0 users
  - [ ] WebSocket authentication tests

- [ ] **Security audit**
  - [ ] Review OAuth token storage and encryption
  - [ ] Test session refresh mechanisms
  - [ ] Validate JWT signature verification
  - [ ] Check for token leakage in logs/errors
  - [ ] Test rate limiting on auth endpoints

- [ ] **Performance testing**
  - [ ] Load test PDS with concurrent signups
  - [ ] Measure session creation latency
  - [ ] Test WebSocket connection stability
  - [ ] Benchmark blob upload speeds
  - [ ] Profile memory usage

- [ ] **User acceptance testing**
  - [ ] Recruit 5-10 beta testers
  - [ ] Test signup flow from scratch
  - [ ] Test Auth0 migration flow
  - [ ] Test publishing to ATProto network
  - [ ] Verify cross-platform identity (Bluesky app)

- [ ] **Documentation**
  - [ ] Write user guide for ATProto features
  - [ ] Document migration process for users
  - [ ] Create developer docs for ATProto integration
  - [ ] Update README with ATProto info
  - [ ] Write blog post announcing ATProto support

### 🚀 Launch Tasks

- [ ] **Beta launch checklist**
  - [ ] Enable ATProto signup for beta users
  - [ ] Monitor error rates and user feedback
  - [ ] Set up support channels (Discord, email)
  - [ ] Create feedback form
  - [ ] Track key metrics (signups, migrations, posts)

- [ ] **Gradual rollout**
  - [ ] Week 1: Invite-only beta (10 users)
  - [ ] Week 2: Open beta (remove invite requirement)
  - [ ] Week 3: Promote to all AC users
  - [ ] Week 4: Default to ATProto for new users
  - [ ] Week 5: Begin Auth0 sunset communication

- [ ] **Marketing & communication**
  - [ ] Announce on AC social channels
  - [ ] Post to Bluesky network from official AC account
  - [ ] Write blog post about becoming ATProto PDS
  - [ ] Share in ATProto developer community
  - [ ] Update website with ATProto branding

### 📋 Deliverables
- [ ] Complete test suite with >80% coverage
- [ ] Security audit report with issues resolved
- [ ] Performance benchmarks documented
- [ ] User documentation published
- [ ] Beta launch successful with positive feedback
- [ ] Public announcement made

---

## Phase 7: Auth0 Deprecation (Week 17-20)

### 🔚 Sunset Tasks

- [ ] **Auth0 deprecation timeline**
  - [ ] Week 1: Announce 90-day migration deadline
  - [ ] Week 4: Add migration banner for Auth0 users
  - [ ] Week 8: Restrict new features to ATProto users
  - [ ] Week 12: Send email reminders to unmigrated users
  - [ ] Week 13: Disable Auth0 signup (login still works)
  - [ ] Week 14: Final migration push
  - [ ] Week 15: Disable Auth0 login
  - [ ] Week 16: Remove Auth0 dependencies from codebase

- [ ] **Data cleanup**
  - [ ] Archive Auth0 user records (don't delete)
  - [ ] Remove Auth0 integration code
  - [ ] Clean up unused environment variables
  - [ ] Update DNS records if needed
  - [ ] Cancel Auth0 subscription

- [ ] **Final verification**
  - [ ] Verify all users migrated or archived
  - [ ] Confirm no Auth0 dependencies remain
  - [ ] Update all documentation
  - [ ] Remove Auth0 from VSCode extension

### 📋 Deliverables
- [ ] 100% of active users migrated to ATProto
- [ ] Auth0 integration completely removed
- [ ] All documentation updated
- [ ] Cost savings realized (Auth0 fees eliminated)

---

## Technical Stack Summary

### Core Dependencies

```json
{
  "dependencies": {
    "@atproto/api": "^0.17.0",
    "@atproto/oauth-client-node": "^0.2.0",
    "@atproto/oauth-client-browser": "^0.2.0",
    "@atproto/identity": "^0.4.0",
    "@atproto/lexicon": "^0.4.0"
  },
  "optionalDependencies": {
    "@atproto/sync": "^0.1.0",
    "@atproto/repo": "^0.4.0"
  }
}
```

### Infrastructure Components

| Component | Technology | Hosting | Cost (est.) |
|-----------|-----------|---------|-------------|
| PDS Server | Docker (Bluesky PDS) | GCP Compute Engine e2-small | $15-20/mo |
| Database | PostgreSQL 14+ | Same VM or Cloud SQL | $0-15/mo |
| DNS | Cloudflare or Google Domains | Existing | $0 |
| Blob Storage | PDS built-in or GCS | GCP Storage | $1-5/mo |
| Email (SMTP) | Resend or SendGrid | SaaS | $0-10/mo |
| **Total** | | | **~$16-50/mo** |

*Compare to Auth0: $25-240/mo depending on MAU*

### DNS Configuration Required

```
pds.aesthetic.computer         A      <server-ip>
*.aesthetic.computer           A      <server-ip>
_atproto.aesthetic.computer    TXT    "did=did:web:aesthetic.computer"
```

---

## Risk Mitigation

| Risk | Mitigation Strategy |
|------|---------------------|
| **PDS downtime affects all auth** | Run redundant PDS instance, implement caching layer |
| **Migration errors lose user data** | Comprehensive backups, dry-run migrations, rollback plan |
| **Users resist migration** | Clear benefits communication, gradual rollout, support |
| **Performance issues at scale** | Load testing before launch, auto-scaling, monitoring |
| **ATProto protocol changes** | Follow ATProto Discord, pin PDS versions, test updates |
| **Existing Auth0 users locked out** | Maintain Auth0 for 6 months minimum, clear migration path |

---

## Success Metrics

- [ ] **Technical:**
  - 99.9% PDS uptime
  - <500ms average auth latency
  - Zero data loss during migration
  - <1% error rate on ATProto operations

- [ ] **User Adoption:**
  - 50% of active users on ATProto within 3 months
  - 90% of active users migrated within 6 months
  - >50 posts/day to ATProto network from AC users
  - Positive sentiment in feedback (>80%)

- [ ] **Business:**
  - Reduce monthly auth costs by $25+
  - Increase user engagement via federation
  - Position AC as creative ATProto community
  - Drive traffic from Bluesky network

---

## Resources & Links

### Official Documentation
- ATProto Docs: https://atproto.com/
- PDS Self-Hosting Guide: https://atproto.com/guides/self-hosting
- PDS GitHub: https://github.com/bluesky-social/pds
- ATProto TypeScript SDK: https://github.com/bluesky-social/atproto/tree/main/packages/api

### Community
- ATProto Discord: https://discord.gg/e7hpHxRfBP
- Bluesky Developers: https://bsky.app/profile/atproto.com

### Tools
- DNS Checker: https://dnschecker.org/
- DID Resolver: https://web.plc.directory/
- ATProto API Explorer: https://docs.bsky.app/

---

## Next Steps

1. **Immediate (This Week):**
   - [ ] Review and approve this roadmap
   - [ ] Decide on hosting provider (GCP recommended)
   - [ ] Provision PDS server
   - [ ] Set up DNS records

2. **Short Term (Next 2 Weeks):**
   - [ ] Complete Phase 1 (PDS setup)
   - [ ] Create test accounts
   - [ ] Begin backend integration (Phase 2)

3. **Medium Term (Next 2 Months):**
   - [ ] Complete backend and frontend integration
   - [ ] Build migration tools
   - [ ] Start beta testing

4. **Long Term (6 Months):**
   - [ ] Full ATProto launch
   - [ ] Deprecate Auth0
   - [ ] Build advanced ATProto features

---

**Last Updated:** October 6, 2025  
**Author:** GitHub Copilot + Jeffrey Alan Scudder  
**Status:** Planning Phase
