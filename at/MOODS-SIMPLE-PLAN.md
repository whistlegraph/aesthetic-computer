# 🦋 Moods ATProto Integration Plan (Simplified)

**Date:** October 14, 2025  
**Status:** Planning

---

## 📊 Current MongoDB Schema

```javascript
// Collection: moods
{
  _id: ObjectId("..."),
  user: "auth0|123...",     // User sub
  mood: "feeling good",     // Text (can be multiline)
  when: ISODate("..."),     // Timestamp
  deleted: false            // Soft delete flag (optional)
}

// Indexes:
// - user: 1
// - when: 1
```

**Source:** `/system/netlify/functions/mood.mjs` lines 106-109

---

## 🦋 ATProto Lexicon (Matching MongoDB)

```json
{
  "lexicon": 1,
  "id": "computer.aesthetic.mood",
  "defs": {
    "main": {
      "type": "record",
      "key": "tid",
      "record": {
        "required": ["mood", "when"],
        "properties": {
          "mood": {
            "type": "string",
            "maxLength": 5000
          },
          "when": {
            "type": "string",
            "format": "datetime"
          },
          "mongoId": {
            "type": "string"
          }
        }
      }
    }
  }
}
```

**File:** `/at/lexicons/computer/aesthetic/mood.json` ✅ Created

---

## 🔄 MongoDB Schema Update

Add optional `atproto` field to moods:

```javascript
{
  _id: ObjectId("..."),
  user: "auth0|123...",
  mood: "feeling good",
  when: ISODate("..."),
  deleted: false,
  atproto: {                              // NEW (optional)
    uri: "at://did:plc:abc.../computer.aesthetic.mood/3k...",
    cid: "bafyrei...",
    created: ISODate("..."),
    synced: true
  }
}
```

**No migration needed** - field is optional and added on new moods only.

---

## 🛠️ Implementation Steps

### Phase 1: Create Helper Functions

**File:** `/system/backend/mood-atproto.mjs` (new)

```javascript
import { AtpAgent } from '@atproto/api';

const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';

/**
 * Create ATProto mood record
 * @param {Object} database - MongoDB connection
 * @param {string} userSub - auth0|... user ID
 * @param {string} moodText - The mood text
 * @param {Date} timestamp - When the mood was created
 * @param {string} mongoId - MongoDB _id as string
 */
export async function createAtprotoMood(database, userSub, moodText, timestamp, mongoId) {
  try {
    const users = database.db.collection('users');
    const user = await users.findOne({ _id: userSub });
    
    if (!user?.atproto?.did || !user?.atproto?.password) {
      console.log(`ℹ️  User ${userSub} has no ATProto account, skipping mood sync`);
      return { created: false, reason: 'no_atproto_account' };
    }

    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password,
    });

    const record = {
      $type: 'computer.aesthetic.mood',
      mood: moodText,
      when: timestamp.toISOString(),
      mongoId: mongoId,
      createdAt: new Date().toISOString(),
    };

    const result = await agent.com.atproto.repo.createRecord({
      repo: user.atproto.did,
      collection: 'computer.aesthetic.mood',
      record,
    });

    console.log(`✅ ATProto mood created: ${result.uri}`);
    
    return {
      created: true,
      uri: result.uri,
      cid: result.cid,
    };
  } catch (error) {
    console.error(`❌ Failed to create ATProto mood: ${error.message}`);
    return { created: false, error: error.message };
  }
}

/**
 * Delete ATProto mood record
 * @param {Object} database - MongoDB connection
 * @param {string} userSub - auth0|... user ID  
 * @param {string} atprotoUri - AT-URI of the mood record
 */
export async function deleteAtprotoMood(database, userSub, atprotoUri) {
  try {
    const users = database.db.collection('users');
    const user = await users.findOne({ _id: userSub });
    
    if (!user?.atproto?.did || !user?.atproto?.password) {
      return { deleted: false, reason: 'no_atproto_account' };
    }

    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password,
    });

    // Parse AT-URI: at://did:plc:abc.../computer.aesthetic.mood/3k...
    const parts = atprotoUri.replace('at://', '').split('/');
    const rkey = parts[parts.length - 1];

    await agent.com.atproto.repo.deleteRecord({
      repo: user.atproto.did,
      collection: 'computer.aesthetic.mood',
      rkey,
    });

    console.log(`✅ ATProto mood deleted: ${atprotoUri}`);
    
    return { deleted: true };
  } catch (error) {
    console.error(`❌ Failed to delete ATProto mood: ${error.message}`);
    return { deleted: false, error: error.message };
  }
}

/**
 * Delete all ATProto moods for a user (for delete-erase-and-forget-me)
 * @param {Object} database - MongoDB connection
 * @param {string} userSub - auth0|... user ID
 */
export async function deleteAllAtprotoMoods(database, userSub) {
  try {
    const users = database.db.collection('users');
    const moods = database.db.collection('moods');
    
    const user = await users.findOne({ _id: userSub });
    
    if (!user?.atproto?.did || !user?.atproto?.password) {
      return { deleted: 0, reason: 'no_atproto_account' };
    }

    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password,
    });

    // Get all moods with ATProto URIs
    const userMoods = await moods.find({
      user: userSub,
      'atproto.uri': { $exists: true },
    }).toArray();

    let deleted = 0;
    for (const mood of userMoods) {
      const result = await deleteAtprotoMood(database, userSub, mood.atproto.uri);
      if (result.deleted) deleted++;
    }

    console.log(`✅ Deleted ${deleted}/${userMoods.length} ATProto moods for ${userSub}`);
    
    return { deleted, total: userMoods.length };
  } catch (error) {
    console.error(`❌ Failed to delete ATProto moods: ${error.message}`);
    return { deleted: 0, error: error.message };
  }
}
```

---

### Phase 2: Update mood.mjs Function

**File:** `/system/netlify/functions/mood.mjs`

**Current code (lines 106-109):**
```javascript
await collection.insertOne({
  user: user.sub,
  mood,
  when: new Date(),
});
```

**Updated code:**
```javascript
import { createAtprotoMood } from '../../backend/mood-atproto.mjs';

// ... in the handler function after line 109:

const timestamp = new Date();
const result = await collection.insertOne({
  user: user.sub,
  mood,
  when: timestamp,
});

// Sync to ATProto (non-blocking)
try {
  const atprotoResult = await createAtprotoMood(
    database,
    user.sub,
    mood,
    timestamp,
    result.insertedId.toString()
  );
  
  if (atprotoResult.created) {
    // Update MongoDB with ATProto URI
    await collection.updateOne(
      { _id: result.insertedId },
      {
        $set: {
          atproto: {
            uri: atprotoResult.uri,
            cid: atprotoResult.cid,
            created: new Date(),
            synced: true,
          }
        }
      }
    );
    console.log(`✅ Mood synced to ATProto: ${atprotoResult.uri}`);
  }
} catch (error) {
  // Don't fail the request if ATProto sync fails
  console.error(`⚠️  ATProto sync failed (non-fatal): ${error.message}`);
}
```

---

### Phase 3: Update delete-erase-and-forget-me

**File:** `/system/netlify/functions/delete-erase-and-forget-me.mjs`

Add import:
```javascript
import { deleteAllAtprotoMoods } from '../../backend/mood-atproto.mjs';
```

Find the moods deletion section and add:
```javascript
// After deleting from MongoDB
const atprotoResult = await deleteAllAtprotoMoods(database, user.sub);
console.log(`🦋 ATProto moods deleted: ${atprotoResult.deleted}`);
```

---

### Phase 4: Backfill Historical Moods

**File:** `/at/scripts/migrate-moods-to-atproto.mjs` (new)

```javascript
#!/usr/bin/env node
// migrate-moods-to-atproto.mjs
// Backfill existing moods from MongoDB to ATProto

import { connect } from '../../system/backend/database.mjs';
import { createAtprotoMood } from '../../system/backend/mood-atproto.mjs';
import { config } from 'dotenv';

config();

async function migrateUserMoods(database, userSub, limit = null, dryRun = true) {
  console.log(`\n📋 Migrating moods for: ${userSub}`);
  console.log(`   Dry run: ${dryRun ? 'YES' : 'NO'}`);
  if (limit) console.log(`   Limit: ${limit} moods`);
  
  const moods = database.db.collection('moods');
  
  // Find moods without ATProto URI
  const query = {
    user: userSub,
    deleted: { $ne: true },
    'atproto.uri': { $exists: false },
  };
  
  const moodsToMigrate = await moods.find(query)
    .sort({ when: 1 })  // Oldest first
    .limit(limit || 0)
    .toArray();
  
  console.log(`   Found ${moodsToMigrate.length} moods to migrate\n`);
  
  if (dryRun) {
    console.log('   ⚠️  Dry run - no changes made\n');
    return { migrated: 0, total: moodsToMigrate.length };
  }
  
  let migrated = 0;
  for (const mood of moodsToMigrate) {
    console.log(`   Processing: ${mood._id} - "${mood.mood.substring(0, 50)}..."`);
    
    const result = await createAtprotoMood(
      database,
      mood.user,
      mood.mood,
      mood.when,
      mood._id.toString()
    );
    
    if (result.created) {
      await moods.updateOne(
        { _id: mood._id },
        {
          $set: {
            atproto: {
              uri: result.uri,
              cid: result.cid,
              created: new Date(),
              synced: true,
            }
          }
        }
      );
      migrated++;
      console.log(`      ✅ Migrated: ${result.uri}`);
    } else {
      console.log(`      ⚠️  Skipped: ${result.reason || result.error}`);
    }
    
    // Rate limiting (1 per second)
    await new Promise(resolve => setTimeout(resolve, 1000));
  }
  
  console.log(`\n✅ Migration complete: ${migrated}/${moodsToMigrate.length} moods\n`);
  return { migrated, total: moodsToMigrate.length };
}

async function migrateAllUsers(limit = null, dryRun = true) {
  const database = await connect();
  
  try {
    const users = database.db.collection('users');
    const moods = database.db.collection('moods');
    
    // Find users with ATProto accounts who have moods
    const usersWithAtproto = await users.find({
      'atproto.did': { $exists: true },
    }).toArray();
    
    console.log(`\n📊 Found ${usersWithAtproto.length} users with ATProto accounts\n`);
    
    for (const user of usersWithAtproto) {
      const moodCount = await moods.countDocuments({
        user: user._id,
        deleted: { $ne: true },
      });
      
      if (moodCount === 0) continue;
      
      console.log(`\n${'='.repeat(80)}`);
      await migrateUserMoods(database, user._id, limit, dryRun);
    }
    
  } finally {
    await database.disconnect();
  }
}

// Parse command line args
const args = process.argv.slice(2);
const dryRun = !args.includes('--execute');
const limitArg = args.find(arg => arg.startsWith('--limit='));
const limit = limitArg ? parseInt(limitArg.split('=')[1]) : null;
const userArg = args.find(arg => arg.startsWith('auth0|') || arg.startsWith('@'));

if (dryRun) {
  console.log('\n⚠️  DRY RUN MODE - No changes will be made');
  console.log('   Add --execute flag to actually migrate\n');
}

if (userArg) {
  // Migrate single user
  const database = await connect();
  try {
    const userSub = userArg.startsWith('@') 
      ? await userIDFromHandleOrEmail(userArg, database)
      : userArg;
    await migrateUserMoods(database, userSub, limit, dryRun);
  } finally {
    await database.disconnect();
  }
} else {
  // Migrate all users
  await migrateAllUsers(limit, dryRun);
}
```

**Usage:**
```bash
# Dry run for single user
node at/scripts/migrate-moods-to-atproto.mjs auth0|123...

# Dry run for all users
node at/scripts/migrate-moods-to-atproto.mjs

# Actually migrate (single user, limit 10)
node at/scripts/migrate-moods-to-atproto.mjs auth0|123... --limit=10 --execute

# Migrate all users
node at/scripts/migrate-moods-to-atproto.mjs --execute
```

---

### Phase 5: Audit & Verification

**File:** `/at/scripts/audit-mood-atproto-sync.mjs` (new)

```javascript
#!/usr/bin/env node
// audit-mood-atproto-sync.mjs
// Verify moods sync between MongoDB and ATProto

import { connect } from '../../system/backend/database.mjs';
import { AtpAgent } from '@atproto/api';
import { config } from 'dotenv';

config();

const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';

async function auditUser(database, userSub) {
  console.log(`\n${'='.repeat(80)}`);
  console.log(`📊 Auditing moods for: ${userSub}\n`);
  
  const users = database.db.collection('users');
  const moods = database.db.collection('moods');
  
  const user = await users.findOne({ _id: userSub });
  if (!user) {
    console.log('❌ User not found\n');
    return;
  }
  
  console.log(`   User: ${user._id}`);
  console.log(`   Handle: ${user.atproto?.handle || 'N/A'}`);
  console.log(`   DID: ${user.atproto?.did || 'N/A'}`);
  
  // MongoDB stats
  const totalMoods = await moods.countDocuments({
    user: userSub,
    deleted: { $ne: true },
  });
  const moodsWithAtproto = await moods.countDocuments({
    user: userSub,
    deleted: { $ne: true },
    'atproto.uri': { $exists: true },
  });
  
  console.log(`\n   MongoDB Moods:`);
  console.log(`      Total: ${totalMoods}`);
  console.log(`      With ATProto: ${moodsWithAtproto}`);
  console.log(`      Without ATProto: ${totalMoods - moodsWithAtproto}`);
  
  // ATProto stats (if user has account)
  if (user.atproto?.did && user.atproto?.password) {
    try {
      const agent = new AtpAgent({ service: PDS_URL });
      await agent.login({
        identifier: user.atproto.did,
        password: user.atproto.password,
      });
      
      const result = await agent.com.atproto.repo.listRecords({
        repo: user.atproto.did,
        collection: 'computer.aesthetic.mood',
      });
      
      console.log(`\n   ATProto PDS:`);
      console.log(`      Total records: ${result.data.records.length}`);
      
      // Sample recent moods
      console.log(`\n   Recent moods (last 5):\n`);
      const recentMoods = await moods.find({
        user: userSub,
        deleted: { $ne: true },
      })
      .sort({ when: -1 })
      .limit(5)
      .toArray();
      
      recentMoods.forEach(mood => {
        console.log(`      ${mood.when.toISOString()}: "${mood.mood.substring(0, 50)}..."`);
        console.log(`         ATProto: ${mood.atproto?.uri ? '✅' : '❌'}`);
      });
      
    } catch (error) {
      console.log(`\n   ❌ Failed to query ATProto: ${error.message}`);
    }
  } else {
    console.log(`\n   ℹ️  User has no ATProto account`);
  }
  
  console.log(`\n${'='.repeat(80)}\n`);
}

// Parse args
const args = process.argv.slice(2);
const userArg = args[0];

if (!userArg) {
  console.log('Usage: node audit-mood-atproto-sync.mjs <auth0|...>');
  process.exit(1);
}

const database = await connect();
try {
  await auditUser(database, userArg);
} finally {
  await database.disconnect();
}
```

---

## 📋 Testing Checklist

### Unit Tests
- [ ] Create mood → MongoDB gets record
- [ ] Create mood with ATProto account → Both MongoDB + ATProto
- [ ] Create mood without ATProto account → MongoDB only (no error)
- [ ] Soft delete mood (`nuke`) → MongoDB only (ATProto keeps)
- [ ] Hard delete user → Both MongoDB + ATProto deleted

### Integration Tests
- [ ] New mood → Firebase notification sent
- [ ] New mood → ATProto record created
- [ ] New mood → MongoDB has `atproto.uri`
- [ ] Delete user → All moods deleted from both systems

### Migration Tests
- [ ] Dry run → Shows counts, no changes
- [ ] Single user migration → Successful
- [ ] Rate limiting → 1 req/sec (no PDS throttling)
- [ ] Audit after migration → 100% synced

---

## 🚨 Important Notes

### What This Does
✅ Adds ATProto support to existing moods  
✅ Maintains MongoDB as primary (no breaking changes)  
✅ Gracefully handles missing ATProto accounts  
✅ Enables federated mood discovery

### What This Doesn't Do
❌ Change existing mood schema (just adds optional field)  
❌ Require ATProto for moods to work  
❌ Break existing integrations  
❌ Expose private data

### Edge Cases
- **No ATProto account:** Mood saved to MongoDB only
- **ATProto PDS down:** Mood saved to MongoDB, sync logged as failed
- **Duplicate moods:** De-duped by checking last mood text
- **Soft delete:** MongoDB marked deleted, ATProto keeps (immutable)
- **Hard delete:** Both systems cleared

---

## 📞 Quick Commands

```bash
# Test single mood creation
curl -X POST https://aesthetic.computer/api/mood \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -d '{"mood": "testing atproto!"}'

# Audit user sync
node at/scripts/audit-mood-atproto-sync.mjs auth0|123...

# Migrate user (dry run)
node at/scripts/migrate-moods-to-atproto.mjs auth0|123...

# Migrate user (execute)
node at/scripts/migrate-moods-to-atproto.mjs auth0|123... --execute

# Migrate all users with limit
node at/scripts/migrate-moods-to-atproto.mjs --limit=100 --execute
```

---

## 🎉 Success Criteria

- [ ] Lexicon deployed to PDS
- [ ] Helper functions created and tested
- [ ] mood.mjs updated with dual-write
- [ ] delete-erase-and-forget-me updated
- [ ] Migration script created
- [ ] Audit script created
- [ ] Test moods synced successfully
- [ ] Historical moods backfilled (at least 100)
- [ ] Documentation complete
- [ ] No breaking changes to existing moods

---

**Status:** ✅ Plan complete - Ready for implementation  
**Next Step:** Create `/system/backend/mood-atproto.mjs` helper functions
