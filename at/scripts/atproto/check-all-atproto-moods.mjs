#!/usr/bin/env node

// Check ATProto PDS for ALL mood records (admin view)

import { connect } from './database.mjs';
import { AtpAgent } from '@atproto/api';
import { config } from 'dotenv';

config({ path: '../.env' });

const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';
const PDS_ADMIN_PASSWORD = process.env.PDS_ADMIN_PASSWORD;

if (!PDS_ADMIN_PASSWORD) {
  console.error('❌ Missing PDS_ADMIN_PASSWORD environment variable');
  process.exit(1);
}

console.log('🔍 Checking ATProto PDS for all mood records\n');

const database = await connect();
const users = database.db.collection('users');
const handles = database.db.collection('@handles');
const moods = database.db.collection('moods');

// Get all users with ATProto accounts
const usersWithAtproto = await users.find({
  'atproto.did': { $exists: true, $ne: null }
}).toArray();

console.log(`👥 Found ${usersWithAtproto.length} users with ATProto accounts\n`);

// Try to use admin API to list all records
// First, let's check a few users' records individually
console.log('📊 Sampling mood records from users:\n');

let totalAtprotoMoods = 0;
const recentMoods = [];

for (let i = 0; i < Math.min(10, usersWithAtproto.length); i++) {
  const user = usersWithAtproto[i];
  const handleDoc = await handles.findOne({ _id: user._id });
  const handle = handleDoc?.handle || 'unknown';
  
  try {
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password
    });

    const result = await agent.com.atproto.repo.listRecords({
      repo: user.atproto.did,
      collection: 'computer.aesthetic.mood',
      limit: 100
    });

    const records = result.data?.records || [];
    totalAtprotoMoods += records.length;
    
    console.log(`   @${handle}: ${records.length} moods`);
    
    // Collect recent moods
    for (const record of records) {
      recentMoods.push({
        handle,
        did: user.atproto.did,
        mood: record.value.mood,
        when: record.value.when,
        rkey: record.uri.split('/').pop(),
        mongoId: record.value.mongoId || record.value.ref
      });
    }
    
  } catch (error) {
    console.log(`   @${handle}: Error - ${error.message}`);
  }
}

console.log(`\n📊 Sample ATProto moods: ${totalAtprotoMoods} moods from ${Math.min(10, usersWithAtproto.length)} users\n`);

// Sort by date and show latest
recentMoods.sort((a, b) => new Date(b.when) - new Date(a.when));

console.log('Latest moods in ATProto (from sampled users):');
for (let i = 0; i < Math.min(10, recentMoods.length); i++) {
  const mood = recentMoods[i];
  console.log(`   [${i+1}] @${mood.handle} - "${mood.mood.substring(0, 40)}..."`);
  console.log(`       When: ${mood.when}`);
  console.log(`       rkey: ${mood.rkey}`);
  console.log(`       MongoDB ref: ${mood.mongoId || 'N/A'}`);
  console.log('');
}

// Now let's try the admin API approach if available
console.log('\n🔐 Attempting admin API access...');

try {
  const adminAgent = new AtpAgent({ service: PDS_URL });
  
  // Try to login as admin
  await adminAgent.login({
    identifier: 'admin',
    password: PDS_ADMIN_PASSWORD
  });
  
  console.log('✅ Admin login successful');
  
  // Try to list all records (if admin API supports it)
  // Note: This might not be available in standard ATProto PDS
  console.log('   (Admin record listing may not be available in standard PDS)');
  
} catch (error) {
  console.log(`❌ Admin login failed: ${error.message}`);
  console.log('   (This is normal - admin API might not be enabled)');
}

// Final comparison
console.log('\n\n📊 Final Statistics:');

const totalMongoMoods = await moods.countDocuments({ deleted: { $ne: true } });
const syncedMongoMoods = await moods.countDocuments({
  deleted: { $ne: true },
  'atproto.rkey': { $exists: true, $ne: null }
});

console.log(`MongoDB moods: ${totalMongoMoods}`);
console.log(`MongoDB moods with rkey: ${syncedMongoMoods}`);
console.log(`ATProto moods (sampled): ${totalAtprotoMoods} from ${Math.min(10, usersWithAtproto.length)} users`);

console.log('\n💡 To get complete ATProto count, would need to query all ' + usersWithAtproto.length + ' users');

await database.disconnect();
