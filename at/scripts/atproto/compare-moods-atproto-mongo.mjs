#!/usr/bin/env node

// Compare latest moods in ATProto vs MongoDB

import { connect } from './database.mjs';
import { AtpAgent } from '@atproto/api';
import { config } from 'dotenv';

config({ path: '../.env' });

const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';

const database = await connect();
const moods = database.db.collection('moods');
const users = database.db.collection('users');
const handles = database.db.collection('@handles');

console.log('🔍 Comparing Latest Moods: ATProto vs MongoDB\n');

// Get latest moods from MongoDB (with ATProto records)
console.log('📊 MongoDB Moods (with atproto.rkey):');
const mongoMoodsWithAtproto = await moods.find({
  deleted: { $ne: true },
  'atproto.rkey': { $exists: true }
}).sort({ when: -1 }).limit(10).toArray();

console.log(`   Found ${mongoMoodsWithAtproto.length} moods with ATProto records\n`);

for (let i = 0; i < Math.min(5, mongoMoodsWithAtproto.length); i++) {
  const mood = mongoMoodsWithAtproto[i];
  const handleDoc = await handles.findOne({ _id: mood.user });
  const handle = handleDoc?.handle || 'unknown';
  
  console.log(`   [${i+1}] @${handle} - "${mood.mood.substring(0, 40)}..."`);
  console.log(`       MongoDB _id: ${mood._id}`);
  console.log(`       ATProto rkey: ${mood.atproto.rkey}`);
  console.log(`       When: ${mood.when.toISOString()}`);
  console.log('');
}

// Get latest moods from MongoDB (ALL)
console.log('\n📊 MongoDB Moods (latest, all):');
const allMongoMoods = await moods.find({
  deleted: { $ne: true }
}).sort({ when: -1 }).limit(10).toArray();

console.log(`   Found ${allMongoMoods.length} total moods\n`);

for (let i = 0; i < Math.min(5, allMongoMoods.length); i++) {
  const mood = allMongoMoods[i];
  const handleDoc = await handles.findOne({ _id: mood.user });
  const handle = handleDoc?.handle || 'unknown';
  
  console.log(`   [${i+1}] @${handle} - "${mood.mood.substring(0, 40)}..."`);
  console.log(`       MongoDB _id: ${mood._id}`);
  console.log(`       ATProto rkey: ${mood.atproto?.rkey || 'NOT SYNCED'}`);
  console.log(`       When: ${mood.when.toISOString()}`);
  console.log('');
}

// Now check ATProto PDS directly
console.log('\n📊 Checking ATProto PDS directly...');

// Get a user with recent moods to check their ATProto records
const recentMood = allMongoMoods[0];
if (!recentMood) {
  console.log('❌ No moods found in MongoDB');
  await database.disconnect();
  process.exit(0);
}

const user = await users.findOne({ _id: recentMood.user });
if (!user?.atproto?.did) {
  console.log('❌ User has no ATProto account');
  await database.disconnect();
  process.exit(0);
}

const handleDoc = await handles.findOne({ _id: recentMood.user });
const handle = handleDoc?.handle || 'unknown';

console.log(`\n   Checking @${handle}'s ATProto records...`);
console.log(`   DID: ${user.atproto.did}\n`);

// Login as admin to list records
const agent = new AtpAgent({ service: PDS_URL });

try {
  await agent.login({
    identifier: user.atproto.did,
    password: user.atproto.password
  });

  const result = await agent.com.atproto.repo.listRecords({
    repo: user.atproto.did,
    collection: 'computer.aesthetic.mood',
    limit: 10
  });

  const records = result.data?.records || [];
  
  console.log(`   Found ${records.length} mood records on ATProto PDS:`);
  
  for (let i = 0; i < Math.min(5, records.length); i++) {
    const record = records[i];
    const rkey = record.uri.split('/').pop();
    
    console.log(`\n   [${i+1}] rkey: ${rkey}`);
    console.log(`       Mood: "${record.value.mood.substring(0, 40)}..."`);
    console.log(`       When: ${record.value.when}`);
    console.log(`       MongoDB ref: ${record.value.mongoId || record.value.ref || 'N/A'}`);
  }

  // Cross-reference: Check if MongoDB moods exist in ATProto
  console.log('\n\n🔄 Cross-Reference Check:');
  console.log(`   Checking if @${handle}'s MongoDB moods exist in ATProto...`);
  
  const userMongoMoods = await moods.find({
    user: recentMood.user,
    deleted: { $ne: true }
  }).sort({ when: -1 }).limit(10).toArray();
  
  const atprotoRkeys = new Set(records.map(r => r.uri.split('/').pop()));
  
  let synced = 0;
  let notSynced = 0;
  
  console.log('');
  for (const mood of userMongoMoods) {
    const rkey = mood.atproto?.rkey;
    const exists = rkey && atprotoRkeys.has(rkey);
    
    if (exists) {
      console.log(`   ✅ "${mood.mood.substring(0, 30)}..." - synced (${rkey})`);
      synced++;
    } else {
      console.log(`   ❌ "${mood.mood.substring(0, 30)}..." - NOT synced (${rkey || 'no rkey'})`);
      notSynced++;
    }
  }
  
  console.log(`\n   Summary: ${synced} synced, ${notSynced} not synced`);

} catch (error) {
  console.error(`   ❌ Error accessing ATProto: ${error.message}`);
}

await database.disconnect();
