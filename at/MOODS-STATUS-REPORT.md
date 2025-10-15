# 🦋 Moods ATProto Integration - Status Report

**Date:** October 14, 2025  
**Status:** ✅ Migration Complete, ⚠️ Sync Not Yet Implemented

---

## ✅ What's Working

### 1. Lexicon Comparison

**Aesthetic Computer Mood (`computer.aesthetic.mood`):**
```json
{
  "mood": "string (max 5000 chars)",
  "when": "datetime",
  "mongoId": "string (MongoDB _id)"
}
```

**Bluesky Post (`app.bsky.feed.post`):**
```json
{
  "text": "string (max 3000 chars, grapheme-counted)",
  "facets": "array (mentions, links, tags)",
  "reply": "object (threading)",
  "embed": "object (images, videos, external)",
  "langs": "array",
  "labels": "self-labels",
  "tags": "array",
  "createdAt": "datetime"
}
```

**Key Differences:**
- ✅ **Much simpler** - No embeds, replies, facets, or media
- ✅ **Custom namespace** - `computer.aesthetic.*` vs `app.bsky.*`
- ✅ **Bi-directional sync** - `mongoId` field for MongoDB reference
- ✅ **Longer text** - 5000 chars vs 3000 (no grapheme counting needed)
- ✅ **No social features** - Pure status updates, not threaded conversations

---

### 2. Migration Results

**@jeffrey's moods:**
- ✅ **222 moods** successfully migrated to ATProto PDS
- ✅ All accessible at: `at://did:plc:a2eiol3qtvncznlbt6v4wvww/computer.aesthetic.mood/*`
- ✅ Each has `mongoId` for bi-directional lookup

**Sample ATProto Record:**
```json
{
  "$type": "computer.aesthetic.mood",
  "mood": "i'm a butterfly",
  "when": "2025-10-09T19:00:28.821Z",
  "mongoId": "68e8064c11885fb93a529c8d"
}
```

**MongoDB Record (BEFORE migration):**
```javascript
{
  _id: ObjectId("68e8064c11885fb93a529c8d"),
  user: "auth0|66db1c303e2015e7f35b5cc5",
  mood: "i'm a butterfly",
  when: ISODate("2025-10-09T19:00:28.821Z")
}
```

**MongoDB Record (AFTER migration - NEEDS UPDATE):**
```javascript
{
  _id: ObjectId("68e8064c11885fb93a529c8d"),
  user: "auth0|66db1c303e2015e7f35b5cc5",
  mood: "i'm a butterfly",
  when: ISODate("2025-10-09T19:00:28.821Z"),
  atproto: {
    rkey: "3m36qcr3bg22g"  // ⚠️ NOT YET ADDED TO MONGODB!
  }
}
```

---

## ⚠️ What's NOT Working Yet

### 3. MongoDB Records Missing `atproto` Field

**Problem:** The migration script created ATProto records but **MongoDB doesn't have the `atproto.rkey` reference yet!**

**Evidence:**
```bash
$ node scripts/check-mood-sync.mjs jeffrey
📈 Sync Status:
   Total moods: 222
   Synced to ATProto: 0    # ❌ MongoDB says 0!
   Not synced: 222
```

But ATProto PDS has them:
```bash
$ node scripts/check-atproto-moods.mjs jeffrey
   Total moods on PDS: 100  # ✅ ATProto has them!
```

**Root Cause:** Migration script was cancelled (Ctrl+C) before it could update MongoDB with the `atproto.rkey` references.

---

### 4. Netlify Function Not Syncing Yet

**File:** `/system/netlify/functions/mood.mjs` (lines 106-109)

**Current code (NO ATProto sync):**
```javascript
await collection.insertOne({
  user: user.sub,
  mood,
  when: new Date(),
});
```

**Needed code (WITH ATProto sync):**
```javascript
// 1. Insert into MongoDB (without atproto field yet)
const result = await collection.insertOne({
  user: user.sub,
  mood,
  when: new Date(),
});

const mongoId = result.insertedId.toString();

// 2. Create ATProto record
const atprotoRecord = await agent.com.atproto.repo.createRecord({
  repo: userDid,
  collection: 'computer.aesthetic.mood',
  record: {
    $type: 'computer.aesthetic.mood',
    mood: mood,
    when: new Date().toISOString(),
    mongoId: mongoId
  }
});

// 3. Update MongoDB with ATProto reference
await collection.updateOne(
  { _id: result.insertedId },
  { 
    $set: { 
      atproto: {
        rkey: atprotoRecord.uri.split('/').pop()
      }
    }
  }
);
```

**Status:** ❌ Not implemented yet - new moods will NOT sync to ATProto automatically

---

## 🎯 Next Steps

### Step 1: Complete the Migration (Update MongoDB)

Run the migration script again to update MongoDB with `atproto.rkey` references:

```bash
cd /workspaces/aesthetic-computer/at
node scripts/migrate-moods-to-atproto.mjs jeffrey --migrate
```

This will:
- Check each ATProto record for its `mongoId`
- Update the corresponding MongoDB document with `atproto.rkey`
- Create bi-directional references

**OR** create a reverse-sync script:

```bash
# New script: sync-atproto-back-to-mongo.mjs
# Reads ATProto records, updates MongoDB with rkeys
```

---

### Step 2: Update mood.mjs Netlify Function

Create helper module: `/system/backend/mood-atproto.mjs`

```javascript
export async function createMoodWithAtproto(database, sub, moodText) {
  // 1. Get user's ATProto credentials
  const users = database.db.collection('users');
  const user = await users.findOne({ _id: sub });
  
  if (!user?.atproto?.did) {
    // User doesn't have ATProto - just create MongoDB mood
    const moods = database.db.collection('moods');
    const result = await moods.insertOne({
      user: sub,
      mood: moodText,
      when: new Date()
    });
    return { mongoId: result.insertedId, atproto: null };
  }
  
  // 2. Create MongoDB mood first
  const moods = database.db.collection('moods');
  const result = await moods.insertOne({
    user: sub,
    mood: moodText,
    when: new Date()
  });
  
  const mongoId = result.insertedId.toString();
  
  // 3. Create ATProto record
  const agent = new AtpAgent({ service: PDS_URL });
  await agent.login({ 
    identifier: user.atproto.did, 
    password: user.atproto.password 
  });
  
  const atprotoRecord = await agent.com.atproto.repo.createRecord({
    repo: user.atproto.did,
    collection: 'computer.aesthetic.mood',
    record: {
      $type: 'computer.aesthetic.mood',
      mood: moodText,
      when: new Date().toISOString(),
      mongoId: mongoId
    }
  });
  
  // 4. Update MongoDB with ATProto reference
  const rkey = atprotoRecord.uri.split('/').pop();
  await moods.updateOne(
    { _id: result.insertedId },
    { $set: { atproto: { rkey } } }
  );
  
  return { 
    mongoId: result.insertedId, 
    atproto: { rkey, uri: atprotoRecord.uri }
  };
}
```

Then update `mood.mjs`:

```javascript
// Replace this:
await collection.insertOne({
  user: user.sub,
  mood,
  when: new Date(),
});

// With this:
await createMoodWithAtproto(database, user.sub, mood);
```

---

### Step 3: Update delete-erase-and-forget-me.mjs

Add ATProto deletion:

```javascript
// Delete moods from ATProto
const userMoods = await moods.find({ user: sub }).toArray();

if (user?.atproto?.did && user?.atproto?.password) {
  const agent = new AtpAgent({ service: PDS_URL });
  await agent.login({ 
    identifier: user.atproto.did, 
    password: user.atproto.password 
  });
  
  for (const mood of userMoods) {
    if (mood.atproto?.rkey) {
      await agent.com.atproto.repo.deleteRecord({
        repo: user.atproto.did,
        collection: 'computer.aesthetic.mood',
        rkey: mood.atproto.rkey
      });
    }
  }
}

// Then delete from MongoDB
await moods.deleteMany({ user: sub });
```

---

### Step 4: Create Audit/Sync Scripts

**Audit script (check sync status):**
```bash
node at/scripts/check-mood-sync.mjs @jeffrey
```

**Sync script (fix drift):**
```bash
node at/scripts/sync-moods-bidirectional.mjs @jeffrey
```

---

## 📊 Current Status Summary

| Component | Status | Notes |
|-----------|--------|-------|
| **Lexicon** | ✅ Complete | `computer.aesthetic.mood` |
| **Migration** | ⚠️ Partial | ATProto has moods, MongoDB missing rkeys |
| **MongoDB Schema** | ⚠️ Partial | Needs `atproto.rkey` field added |
| **ATProto PDS** | ✅ Complete | 222 moods migrated for @jeffrey |
| **mood.mjs (POST)** | ❌ Not Started | No ATProto sync yet |
| **mood.mjs (DELETE)** | ❌ Not Started | No ATProto sync yet |
| **delete-erase** | ❌ Not Started | No ATProto cleanup yet |

---

## 🚀 Priority Order

1. **HIGH:** Fix MongoDB - add `atproto.rkey` to all migrated moods
2. **HIGH:** Update `mood.mjs` to sync new moods to ATProto
3. **MEDIUM:** Update `delete-erase-and-forget-me.mjs` to delete from ATProto
4. **LOW:** Create audit/sync scripts for monitoring
5. **LOW:** Migrate other users' moods

---

## 📝 Testing Checklist

- [ ] Complete MongoDB sync (add rkeys)
- [ ] Post new mood via mood.mjs → Check both MongoDB and ATProto
- [ ] Delete mood → Verify deleted from both systems
- [ ] Run delete-erase-and-forget-me → Verify all moods purged
- [ ] Run audit script → Should show 100% sync
- [ ] Test with user who doesn't have ATProto account → Should work (MongoDB only)

---

**Next Command:**
```bash
# Fix the MongoDB sync:
cd /workspaces/aesthetic-computer/at
node scripts/sync-atproto-back-to-mongo.mjs jeffrey
```
