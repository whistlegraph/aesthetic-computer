# Piece Analytics Plan

## Current State

- **MongoDB Atlas** is already set up and used for:
  - `@handles` - user handles
  - `pieces` - user-uploaded pieces (via track-media.mjs)
  - `paintings` - user paintings
  - `tapes` - recorded tapes
  - `moods` - user moods
  - `logs` - system logs
  - `chat-system` - chat messages

- **No tracking exists** for system piece hits (the built-in pieces like `prompt`, `line`, `colors`, etc.)

## Proposed Schema

### Collection: `piece-hits` (Aggregate Stats)

```javascript
{
  _id: ObjectId,
  piece: "colors",           // piece name (slug)
  type: "system" | "user",   // system pieces vs @handle/piece
  hits: 12345,               // total hit count
  uniqueUsers: 8234,         // unique user count
  firstHit: Date,            // first recorded hit
  lastHit: Date,             // most recent hit
  
  // Rolling daily aggregation (last 30 days)
  daily: {
    "2025-12-31": { hits: 45, unique: 32 },
    "2025-12-30": { hits: 52, unique: 41 },
  }
}
```

### Collection: `piece-user-hits` (Per-User Stats)

```javascript
{
  _id: ObjectId,
  piece: "colors",           // piece name
  user: "auth0|abc123",      // user sub (permanent ID)
  hits: 42,                  // how many times this user hit this piece
  firstHit: Date,
  lastHit: Date,
}
// Compound unique index: { piece: 1, user: 1 }
// Note: handles resolved at query time from @handles collection
```

### Collection: `piece-hit-log` (Optional Raw Event Log)

```javascript
{
  _id: ObjectId,
  piece: "colors",
  user: "auth0|abc123" | null,
  timestamp: Date,
  referrer: "prompt",        // previous piece
  params: ["dark"],          // piece parameters
  sessionId: "xyz789",       // to group session activity
}
// TTL index: auto-delete after 90 days
```
```

### Collection: `piece-hit-events` (Optional, for detailed analytics)

```javascript
{
  _id: ObjectId,
  piece: "colors",
  user: "sub_123..." | null,  // null for anonymous
  timestamp: Date,
  referrer: "prompt" | null,  // which piece they came from
  params: ["param1"],         // piece parameters used
  duration: 45000,            // ms spent in piece (optional, tracked on leave)
  device: "mobile" | "desktop"
}
```

## Implementation Options

### Option A: Client-Side Tracking (Lightweight)
**Where**: `disk.mjs` or `bios.mjs` (client-side)

```javascript
// In boot() when piece loads
async function trackPieceHit(pieceName) {
  try {
    await fetch('/api/piece-hit', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ 
        piece: pieceName,
        referrer: document.referrer
      })
    });
  } catch (e) { /* silent fail */ }
}
```

**Pros**: Accurate user-initiated loads, can track duration  
**Cons**: Can be blocked, adds latency to piece load

### Option B: Server-Side Tracking (index.mjs)
**Where**: `system/netlify/functions/index.mjs`

Track every page render request:

```javascript
// In the handler, after parsing slug
if (statusCode === 200 && slug && !previewOrIcon) {
  // Fire-and-forget hit tracking (don't await)
  trackHit(slug, parsed).catch(e => console.error('Hit tracking failed:', e));
}
```

**Pros**: More reliable, no client-side blocking, catches all loads  
**Cons**: Includes bots/crawlers, no duration tracking

### Option C: Hybrid (Recommended)
- **Server-side** for page view counts (simple increment)
- **Client-side** for engagement metrics (duration, interactions)

## New API Endpoint: `/api/piece-hit`

**File**: `system/netlify/functions/piece-hit.mjs`

```javascript
// piece-hit.mjs
import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  const database = await connect();
  
  // GET: Return stats for a piece or all pieces
  if (event.httpMethod === "GET") {
    const { piece, top, users } = event.queryStringParameters || {};
    const hitsCol = database.db.collection("piece-hits");
    const userHitsCol = database.db.collection("piece-user-hits");
    const handlesCol = database.db.collection("@handles");
    
    if (piece) {
      const stats = await hitsCol.findOne({ piece });
      
      // Optionally include top users for this piece
      let topUsers = [];
      if (users) {
        const userStats = await userHitsCol
          .find({ piece, user: { $ne: "anonymous" } })
          .sort({ hits: -1 })
          .limit(10)
          .toArray();
        
        // Resolve handles from subs
        for (const u of userStats) {
          const handleDoc = await handlesCol.findOne({ user: u.user });
          topUsers.push({
            handle: handleDoc?._id || null,
            hits: u.hits,
            lastHit: u.lastHit
          });
        }
      }
      
      return respond(200, { 
        ...stats, 
        topUsers: users ? topUsers : undefined 
      });
    }
    
    // Return top pieces overall
    const pieces = await hitsCol
      .find({})
      .sort({ hits: -1 })
      .limit(parseInt(top) || 50)
      .toArray();
    return respond(200, { pieces });
  }
  
  // POST: Record a hit
  if (event.httpMethod === "POST") {
    const { piece, type = "system", referrer, params } = JSON.parse(event.body || "{}");
    if (!piece) return respond(400, { error: "piece required" });
    
    // Try to get user from auth header
    let user = null;
    try {
      user = await authorize(event.headers);
    } catch (e) { /* anonymous hit */ }
    
    const now = new Date();
    const today = now.toISOString().split("T")[0];
    const hitsCol = database.db.collection("piece-hits");
    const userHitsCol = database.db.collection("piece-user-hits");
    
    // 1. Update aggregate stats
    const updateOps = {
      $inc: { 
        hits: 1,
        [`daily.${today}.hits`]: 1
      },
      $set: { lastHit: now, type },
      $setOnInsert: { firstHit: now, uniqueUsers: 0 }
    };
    
    await hitsCol.updateOne({ piece }, updateOps, { upsert: true });
    
    // 2. Update per-user stats (only sub, no handle)
    const userKey = user?.sub || "anonymous";
    const userResult = await userHitsCol.updateOne(
      { piece, user: userKey },
      {
        $inc: { hits: 1 },
        $set: { lastHit: now },
        $setOnInsert: { firstHit: now }
      },
      { upsert: true }
    );
    
    // If this was a new user for this piece, increment uniqueUsers
    if (userResult.upsertedCount > 0 && userKey !== "anonymous") {
      await hitsCol.updateOne(
        { piece },
        { $inc: { uniqueUsers: 1, [`daily.${today}.unique`]: 1 } }
      );
    }
    
    await database.disconnect();
    return respond(200, { success: true });
  }
  
  return respond(405, { error: "Method not allowed" });
}
```

## New API Endpoint: `/api/piece-fans`

**File**: `system/netlify/functions/piece-fans.mjs`

Get top users ("fans") of a piece:

```javascript
// piece-fans.mjs - Get users who love a specific piece
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  if (event.httpMethod !== "GET") return respond(405);
  
  const { piece, limit = 20 } = event.queryStringParameters || {};
  if (!piece) return respond(400, { error: "piece required" });
  
  const database = await connect();
  const userHitsCol = database.db.collection("piece-user-hits");
  const handlesCol = database.db.collection("@handles");
  
  // Get top users by hits (excluding anonymous)
  const userStats = await userHitsCol
    .find({ piece, user: { $ne: "anonymous" } })
    .sort({ hits: -1 })
    .limit(parseInt(limit))
    .toArray();
  
  // Resolve handles from subs at query time
  const fans = [];
  for (const u of userStats) {
    const handleDoc = await handlesCol.findOne({ user: u.user });
    if (handleDoc) {  // Only include users with handles
      fans.push({
        handle: handleDoc._id,
        hits: u.hits,
        firstHit: u.firstHit,
        lastHit: u.lastHit
      });
    }
  }
  
  await database.disconnect();
  return respond(200, { piece, fans });
}
```

## Using Hit Data in list.mjs

```javascript
// In boot(), fetch popular pieces
const hitStats = await fetch('/api/piece-hit').then(r => r.json());
const hitMap = new Map(hitStats.pieces?.map(p => [p.piece, p.hits]) || []);

// Add "🔥 Popular" category sorted by hits
const popularPieces = allItems
  .filter(item => hitMap.get(item.name) > 100)
  .sort((a, b) => (hitMap.get(b.name) || 0) - (hitMap.get(a.name) || 0))
  .slice(0, 20);
```

## Implementation Steps

1. **Create endpoint**: `piece-hit.mjs` with GET/POST + user tracking
2. **Create endpoint**: `piece-fans.mjs` to query top users per piece
3. **Add server-side tracking**: In `index.mjs`, fire-and-forget POST on each page load
4. **Create indexes**:
   - `piece-hits`: `{ piece: 1 }` unique, `{ hits: -1 }` for sorting
   - `piece-user-hits`: `{ piece: 1, user: 1 }` unique compound, `{ piece: 1, hits: -1 }` for fan queries
   - `piece-hit-log`: `{ timestamp: 1 }` TTL 90 days (optional)
5. **Update list.mjs**: Add "🔥 Popular" category using hit data
6. **Add to metrics.mjs**: Include piece hit stats in `/api/metrics`
7. **Create piece detail view**: Show fans/top users for each piece

## Example Queries

```javascript
// Top 10 most visited pieces
db.collection("piece-hits").find().sort({ hits: -1 }).limit(10)

// Top fans of "colors" piece
db.collection("piece-user-hits")
  .find({ piece: "colors", user: { $ne: "anonymous" } })
  .sort({ hits: -1 })
  .limit(10)

// User's favorite pieces (most visited by a specific user)
db.collection("piece-user-hits")
  .find({ user: "auth0|abc123" })
  .sort({ hits: -1 })
  .limit(10)

// Pieces a user has never visited
const visited = await db.collection("piece-user-hits")
  .find({ user: "auth0|abc123" })
  .project({ piece: 1 }).toArray();
const visitedSet = new Set(visited.map(v => v.piece));
// Then filter allPieces - visitedSet
```

## Privacy Considerations

- Don't store IP addresses
- User IDs only if logged in (optional)
- No personal data in hit events
- Consider GDPR compliance for EU users
- Add rate limiting to prevent abuse

## Estimated Effort

- **Endpoint + Server tracking**: 1-2 hours
- **Client-side duration tracking**: 2-3 hours
- **list.mjs integration**: 1 hour
- **Indexes + testing**: 1 hour

**Total**: ~5-7 hours for full implementation

## Questions to Decide

1. **Track duration?** Requires client-side tracking on piece leave
2. **Track user vs anonymous separately?** More complex queries
3. **Rolling daily stats?** Need cleanup job for old data
4. **Bot filtering?** Could use User-Agent checking
5. **Rate limiting?** Prevent spam hits from single client

---

*Created: 2025.12.31*
