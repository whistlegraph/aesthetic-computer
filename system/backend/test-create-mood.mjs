#!/usr/bin/env node
// Test createMoodOnAtproto to debug response structure

import { connect } from './database.mjs';
import { userIDFromHandle } from './authorization.mjs';
import { createMoodOnAtproto } from './mood-atproto.mjs';

const handle = process.argv[2] || 'jeffrey';
const testMood = `Test mood @ ${new Date().toISOString()}`;

console.log(`\n🧪 Testing createMoodOnAtproto for @${handle}...`);
console.log(`   Mood: "${testMood}"\n`);

const database = await connect();
const sub = await userIDFromHandle(handle, database);
const moods = database.db.collection('moods');

// Create a test mood in MongoDB first
const mongoResult = await moods.insertOne({
  user: sub,
  mood: testMood,
  when: new Date(),
});

console.log(`✅ Created MongoDB mood: ${mongoResult.insertedId}\n`);

// Try to sync to ATProto
console.log(`🔄 Syncing to ATProto...`);
const atprotoResult = await createMoodOnAtproto(
  database,
  sub,
  testMood,
  new Date(),
  mongoResult.insertedId.toString()
);

console.log(`\n📊 Result:`, JSON.stringify(atprotoResult, null, 2));

if (atprotoResult.rkey) {
  console.log(`\n✅ SUCCESS - Got rkey: ${atprotoResult.rkey}`);
  
  // Update MongoDB
  await moods.updateOne(
    { _id: mongoResult.insertedId },
    { $set: { atproto: { rkey: atprotoResult.rkey } } }
  );
  
  console.log(`✅ Updated MongoDB with rkey\n`);
} else {
  console.log(`\n❌ FAILED - ${atprotoResult.error}\n`);
  
  // Clean up test mood
  await moods.deleteOne({ _id: mongoResult.insertedId });
  console.log(`🧹 Cleaned up test mood\n`);
}

await database.disconnect();
process.exit(0);
