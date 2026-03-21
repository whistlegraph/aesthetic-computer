# 🦋 Moods Bi-Directional Sync Strategy

**Date:** October 14, 2025  
**Decision:** Use bi-directional references between MongoDB and ATProto

---

## 🎯 Why Bi-Directional?

### Advantages ✅
1. **Fast lookups both ways** - No need to scan entire collections
2. **Easy debugging** - Can verify sync status from either system
3. **Resilient to failures** - If one side fails, can recover from the other
4. **Delete operations** - `delete-erase-and-forget-me` can find ATProto records instantly
5. **Migration verification** - Easy to identify which moods are/aren't synced
6. **No N+1 queries** - Direct references prevent cascading lookups

### Trade-offs ⚖️
- Slightly more storage (minimal: ~100 bytes per mood)
- Must update both references (atomic operation in MongoDB)
- Risk of reference drift (mitigated by audit scripts)

**Conclusion:** Benefits far outweigh costs for this use case

---

## 📊 Data Schemas

### MongoDB Collection: `moods`

```javascript
{
  _id: ObjectId("507f1f77bcf86cd799439011"),  // MongoDB unique ID
  user: "auth0|68ee500f0729405b6c0dfa18",     // Auth0 user sub
  mood: "feeling creative today 🎨",          // Mood text (max 5000 chars)
  when: ISODate("2025-10-14T19:30:00.000Z"), // Creation timestamp
  deleted: false,                              // Soft delete (optional)
  
  // ATProto sync metadata (NEW - minimal)
  atproto: {
    rkey: "3kh5dye2qpc2a"                     // Record key (TID) - all we need to delete
  }
  // Note: Full URI can be constructed as: at://{userDid}/computer.aesthetic.mood/{rkey}
  // Note: CID omitted (not needed for sync, only useful for versioning)
}
```

**New Fields:**
- `atproto.rkey` - ATProto's record key (TID format, sortable by time) - **only field we need!**

Full URI can be constructed when needed: `at://{userDid}/computer.aesthetic.mood/{rkey}`

### ATProto Record: `computer.aesthetic.mood`

```javascript
{
  "$type": "computer.aesthetic.mood",
  "mood": "feeling creative today 🎨",
  "when": "2025-10-14T19:30:00.000Z",
  "mongoId": "507f1f77bcf86cd799439011"       // MongoDB _id as hex string
}
```

**Stored at:** `at://did:plc:u6wl.../computer.aesthetic.mood/3kh5dye2qpc2a`

---

## 🔄 Sync Flow

### Creating a Mood (New)

```javascript
// 1. Create mood in MongoDB
const result = await moods.insertOne({
  user: sub,
  mood: "feeling great!",
  when: new Date()
  // atproto field NOT set yet
});

const mongoId = result.insertedId.toString();

// 2. Create ATProto record
const atprotoRecord = await agent.com.atproto.repo.createRecord({
  repo: userDid,
  collection: 'computer.aesthetic.mood',
  record: {
    $type: 'computer.aesthetic.mood',
    mood: "feeling great!",
    when: new Date().toISOString(),
    mongoId: mongoId  // Reference to MongoDB
  }
});

// 3. Update MongoDB with ATProto reference (minimal)
await moods.updateOne(
  { _id: result.insertedId },
  { 
    $set: { 
      atproto: {
        rkey: atprotoRecord.uri.split('/').pop()  // Just the rkey
      }
    }
  }
);
```

### Deleting a Mood

```javascript
// 1. Lookup mood in MongoDB
const mood = await moods.findOne({ 
  _id: new ObjectId(moodId),
  user: sub 
});

if (!mood) return { error: 'Not found' };

// 2. Delete from ATProto (if synced)
if (mood.atproto?.rkey) {
  await agent.com.atproto.repo.deleteRecord({
    repo: userDid,
    collection: 'computer.aesthetic.mood',
    rkey: mood.atproto.rkey
  });
}

// 3. Soft delete in MongoDB
await moods.updateOne(
  { _id: mood._id },
  { $set: { deleted: true } }
);
```

### Delete-Erase-And-Forget-Me (Full Purge)

```javascript
// 1. Find all user's moods
const userMoods = await moods.find({ user: sub }).toArray();

// 2. Delete all from ATProto
for (const mood of userMoods) {
  if (mood.atproto?.rkey) {
    await agent.com.atproto.repo.deleteRecord({
      repo: userDid,
      collection: 'computer.aesthetic.mood',
      rkey: mood.atproto.rkey  // Direct lookup - no scanning!
    });
  }
}

// 3. Hard delete from MongoDB
await moods.deleteMany({ user: sub });
```

---

## 🔍 Verification & Auditing

### Check Sync Status

```javascript
// Find moods not yet synced to ATProto
const unsynced = await moods.find({
  user: sub,
  atproto: { $exists: false }
}).toArray();

// Find orphaned ATProto records (MongoDB deleted but ATProto exists)
const atprotoMoods = await agent.com.atproto.repo.listRecords({
  repo: userDid,
  collection: 'computer.aesthetic.mood'
});

const orphaned = atprotoMoods.records.filter(record => {
  const mongoExists = await moods.findOne({ 
    _id: new ObjectId(record.value.mongoId) 
  });
  return !mongoExists || mongoExists.deleted;
});
```

### Audit Script Output

```
🔍 Mood Sync Audit for user: auth0|123...

MongoDB moods: 42
  ✅ Synced to ATProto: 40
  ⚠️  Not synced: 2
  🗑️  Soft deleted: 3

ATProto moods: 40
  ✅ Matched in MongoDB: 38
  ⚠️  Orphaned (MongoDB deleted): 2

Issues:
  - 2 moods not synced to ATProto (IDs: 507f..., 8a3b...)
  - 2 orphaned ATProto records (rkeys: 3kh5..., 9bk2...)
```

---

## 📝 Migration Strategy

### Phase 1: Backfill Existing Moods (Read-Only)

```javascript
// Migrate old moods to ATProto (dry-run mode)
const oldMoods = await moods.find({ 
  atproto: { $exists: false },
  deleted: { $ne: true }
}).toArray();

console.log(`Found ${oldMoods.length} moods to migrate`);

for (const mood of oldMoods) {
  // Create ATProto record
  const record = await createAtprotoMood(mood);
  
  // Update MongoDB with reference
  await moods.updateOne(
    { _id: mood._id },
    { $set: { atproto: record } }
  );
}
```

### Phase 2: Enable Dual-Write (New Moods)

Update `mood.mjs` to write to both MongoDB and ATProto for all new moods.

### Phase 3: Verify & Cleanup

Run audit scripts to verify sync, fix orphaned records.

---

## 🎯 Implementation Checklist

- [x] Design bi-directional schema
- [x] Update lexicon with `mongoId` (required)
- [x] Update MongoDB schema docs with `atproto` field
- [ ] Create `/system/backend/mood-atproto.mjs` helper
  - [ ] `createAtprotoMood(mood, userDid)`
  - [ ] `deleteAtprotoMood(rkey, userDid)`
  - [ ] `getAtprotoMoods(userDid, limit)`
- [ ] Update `/system/netlify/functions/mood.mjs`
  - [ ] POST: Dual-write to MongoDB + ATProto
  - [ ] DELETE: Remove from both systems
  - [ ] GET: Option to fetch from ATProto
- [ ] Update `/system/netlify/functions/delete-erase-and-forget-me.mjs`
  - [ ] Delete all moods from both MongoDB and ATProto
- [ ] Create migration scripts
  - [ ] `migrate-moods-to-atproto.mjs` (backfill)
  - [ ] `audit-mood-sync.mjs` (verification)
  - [ ] `cleanup-orphaned-moods.mjs` (fix drift)
- [ ] Testing
  - [ ] Test create mood (both systems updated)
  - [ ] Test delete mood (both systems updated)
  - [ ] Test delete-all (full purge)
  - [ ] Test migration (old moods backfilled)
  - [ ] Test audit (sync verification)

---

## 📚 References

- MongoDB Schema: `/system/netlify/functions/mood.mjs` lines 106-109
- Delete Function: `/system/netlify/functions/delete-erase-and-forget-me.mjs` lines 75-78
- ATProto Lexicon: `/at/lexicons/computer/aesthetic/mood.json`
- Helper Functions: `/system/backend/at.mjs` (createAtprotoAccount, etc.)

---

**Status:** ✅ Ready for implementation  
**Next Step:** Create `/system/backend/mood-atproto.mjs` helper module
