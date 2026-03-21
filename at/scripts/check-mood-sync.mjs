#!/usr/bin/env node
// Check mood sync status for a user

import { connect } from '../../system/backend/database.mjs';
import { userIDFromHandle } from '../../system/backend/authorization.mjs';

const handle = process.argv[2] || 'jeffrey';

const database = await connect();
const sub = await userIDFromHandle(handle, database);
const moods = database.db.collection('moods');

// Get a few recent moods with atproto field
const recentMoods = await moods.find({ 
  user: sub,
  'atproto.rkey': { $exists: true }
}).sort({ when: -1 }).limit(5).toArray();

console.log(`\n📊 Recent @${handle} moods WITH atproto sync:\n`);
recentMoods.forEach((mood, i) => {
  console.log(`[${i+1}] Mood: ${mood.mood.substring(0, 50)}`);
  console.log(`    When: ${mood.when.toISOString()}`);
  console.log(`    MongoDB _id: ${mood._id}`);
  console.log(`    ATProto rkey: ${mood.atproto.rkey}`);
  console.log(`    Full URI: at://${sub}/computer.aesthetic.mood/${mood.atproto.rkey}`);
  console.log('');
});

// Count synced vs unsynced
const synced = await moods.countDocuments({ user: sub, 'atproto.rkey': { $exists: true } });
const total = await moods.countDocuments({ user: sub, deleted: { $ne: true } });

console.log(`📈 Sync Status:`);
console.log(`   Total moods: ${total}`);
console.log(`   Synced to ATProto: ${synced}`);
console.log(`   Not synced: ${total - synced}`);
console.log('');

await database.disconnect();
process.exit(0);
