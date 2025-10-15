#!/usr/bin/env node
// Delete all ATProto moods and re-migrate with 'ref' field instead of 'mongoId'

import { connect } from '../../system/backend/database.mjs';
import { userIDFromHandle } from '../../system/backend/authorization.mjs';
import { AtpAgent } from '@atproto/api';

const handle = process.argv[2];
const doIt = process.argv.includes('--delete-and-recreate');

if (!handle) {
  console.error('Usage: node remigrate-moods-with-ref.mjs <handle> [--delete-and-recreate]');
  console.error('Example: node remigrate-moods-with-ref.mjs jeffrey --delete-and-recreate');
  process.exit(1);
}

const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';

console.log('='.repeat(80));
console.log(`🔄 RE-MIGRATING MOODS WITH 'ref' FIELD FOR @${handle}`);
if (!doIt) {
  console.log('   [DRY RUN MODE - use --delete-and-recreate to actually do it]');
}
console.log('='.repeat(80));

const database = await connect();
const sub = await userIDFromHandle(handle, database);
const users = database.db.collection('users');
const moods = database.db.collection('moods');

// Get user's ATProto credentials
const user = await users.findOne({ _id: sub });
const { did, password } = user.atproto;

// Login to ATProto
console.log('\n📋 Step 1: Logging into ATProto...');
const agent = new AtpAgent({ service: PDS_URL });
await agent.login({ identifier: did, password });
console.log('   ✅ Logged in');

// Fetch ALL existing ATProto moods
console.log('\n📋 Step 2: Fetching existing ATProto moods...');
const existingMoods = [];
let cursor;

do {
  const response = await agent.com.atproto.repo.listRecords({
    repo: did,
    collection: 'computer.aesthetic.mood',
    limit: 100,
    cursor,
  });
  
  existingMoods.push(...response.data.records);
  cursor = response.data.cursor;
} while (cursor);

console.log(`   Found ${existingMoods.length} existing ATProto moods`);

// Delete all existing ATProto moods
if (existingMoods.length > 0) {
  console.log('\n📋 Step 3: Deleting existing ATProto moods...');
  
  let deleteCount = 0;
  for (const record of existingMoods) {
    const rkey = record.uri.split('/').pop();
    
    if (doIt) {
      await agent.com.atproto.repo.deleteRecord({
        repo: did,
        collection: 'computer.aesthetic.mood',
        rkey,
      });
      deleteCount++;
      
      if (deleteCount % 20 === 0) {
        console.log(`   Deleted ${deleteCount}/${existingMoods.length}...`);
      }
    } else {
      console.log(`   [DRY RUN] Would delete rkey: ${rkey}`);
      if (deleteCount >= 5) {
        console.log(`   ... and ${existingMoods.length - 5} more`);
        break;
      }
      deleteCount++;
    }
  }
  
  if (doIt) {
    console.log(`   ✅ Deleted ${deleteCount} ATProto moods`);
  }
}

// Get all MongoDB moods
console.log('\n📋 Step 4: Fetching MongoDB moods...');
const mongoMoods = await moods.find({ 
  user: sub,
  deleted: { $ne: true }
}).sort({ when: 1 }).toArray(); // Sort by date ascending (oldest first)

console.log(`   Found ${mongoMoods.length} MongoDB moods`);

// Re-create with 'ref' field
console.log('\n📋 Step 5: Re-creating moods with \'ref\' field...');

let createCount = 0;
let failCount = 0;

for (let i = 0; i < mongoMoods.length; i++) {
  const mood = mongoMoods[i];
  
  if (doIt) {
    try {
      // Create ATProto record with 'ref' field
      const record = await agent.com.atproto.repo.createRecord({
        repo: did,
        collection: 'computer.aesthetic.mood',
        record: {
          $type: 'computer.aesthetic.mood',
          mood: mood.mood,
          when: mood.when.toISOString(),  // Preserves original timestamp in content
          ref: mood._id.toString(),  // NEW: Using 'ref' instead of 'mongoId'
        },
      });
      
      // Handle different response structures
      const uri = record.uri || record.data?.uri;
      
      if (!uri) {
        console.error(`   ❌ Failed to create mood ${mood._id}: Response missing URI`);
        failCount++;
        continue;
      }
      
      const rkey = uri.split('/').pop();
      
      // Update MongoDB with new rkey
      await moods.updateOne(
        { _id: mood._id },
        { $set: { atproto: { rkey } } }
      );
      
      createCount++;
      
      if (createCount % 20 === 0) {
        console.log(`   Created ${createCount}/${mongoMoods.length}...`);
      }
    } catch (error) {
      console.error(`   ❌ Failed to create mood ${mood._id}: ${error.message}`);
      failCount++;
    }
  } else {
    console.log(`   [DRY RUN] Would create mood: "${mood.mood.substring(0, 40)}" (${mood.when.toISOString()})`);
    if (i >= 4) {
      console.log(`   ... and ${mongoMoods.length - 5} more`);
      break;
    }
  }
}

if (doIt && createCount > 0) {
  console.log(`   ✅ Created ${createCount} ATProto moods with 'ref' field`);
  if (failCount > 0) {
    console.log(`   ⚠️  Failed: ${failCount}`);
  }
}

// Summary
console.log('\n' + '='.repeat(80));
console.log('\n📊 Summary:');
console.log(`   Existing ATProto moods: ${existingMoods.length} (had 'mongoId')`);
console.log(`   MongoDB moods: ${mongoMoods.length}`);
if (doIt) {
  console.log(`   Deleted: ${existingMoods.length}`);
  console.log(`   Re-created: ${createCount} (now with 'ref')`);
  if (failCount > 0) {
    console.log(`   Failed: ${failCount}`);
  }
  console.log(`\n✅ Migration complete! All moods now use 'ref' field.`);
} else {
  console.log(`\n   Run with --delete-and-recreate to perform migration`);
}
console.log('='.repeat(80) + '\n');

await database.disconnect();
process.exit(0);
